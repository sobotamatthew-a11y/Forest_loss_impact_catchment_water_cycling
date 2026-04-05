# 1. Setup Environment
library(h2o, lib.loc = "/home/mas22047/rlibs/")
array_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))

# 2. Map Configuration
mapping <- list(
  "1" = list(prefix = "EI1", num = 1),
  "2" = list(prefix = "EI1", num = 2),
  "3" = list(prefix = "EI1", num = 5),
  "4" = list(prefix = "RB1", num = 1),
  "5" = list(prefix = "RB1", num = 2),
  "6" = list(prefix = "RB1", num = 5)
)
conf <- mapping[[as.character(array_id)]]
prefix <- conf$prefix
script_id_num <- conf$num
folder_name <- paste0(tolower(prefix), "_", script_id_num)
base_project_path <- file.path("/home/mas22047/Forest_loss_RB_EI", folder_name)

# 3. Dynamic Pathing
data_path <- file.path(base_project_path, "data.csv")

# 4. Initialize H2O
h2o.init(
  ip = "127.0.0.1",
  port = as.numeric(Sys.getenv("H2O_PORT")),
  startH2O = FALSE,
  name = paste0("H2O_Node_", array_id)
)

gpu_to_use <- as.numeric(strsplit(Sys.getenv("SLURM_JOB_GPUS", "0"), ",")[[1]][1])

df <- h2o.importFile(path = data_path, destination_frame = paste0("data_", array_id))

# 5. Prepare Output
output_dir <- file.path(base_project_path, "output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
checkpoint_file <- file.path(output_dir, paste0("checkpoint_", prefix, "_", script_id_num, ".RData"))

# 6. Split Data
splits <- h2o.splitFrame(df, ratios = c(0.7, 0.2), seed = 123)
train <- splits[[1]]; valid <- splits[[2]]; test <- splits[[3]]

if (grepl("^EI", prefix)) {
  response <- "ei_diff1"
} else if (grepl("^RB", prefix)) {
  response <- "rb_diff1"
} else {
  stop("Unknown prefix, cannot determine response variable")
}

bad_cols <- c('gauge_name', 'gauge_id', 'country', 'loss_year')
available_cols <- h2o.names(df)

if (!(response %in% available_cols)) {
  stop(paste("Response column", response, "NOT found in dataset"))
}

initial_features <- setdiff(available_cols, c(response, bad_cols))
current_features <- initial_features

# 7. Hyperparameters + Search Criteria (updated)
hyper_params <- list(
    ntrees = c(100, 200, 500),
    max_depth = c(6, 9, 12, 15),
    learn_rate = c(0.01, 0.05, 0.1),
    sample_rate = c(0.7, 0.8, 1.0),
    col_sample_rate = c(0.7, 0.8, 1.0)
)

search_criteria <- list(
    strategy = 'RandomDiscrete',
    max_models = 250,
    seed = 123,
    stopping_rounds = 3,
    stopping_tolerance = 1e-4,
    stopping_metric = 'RMSE'
)

results_file <- file.path(output_dir, paste0("rmse_results_", prefix, "_", script_id_num, ".csv"))

# 8. Load checkpoint or initialize
if (file.exists(checkpoint_file)) {
  load(checkpoint_file)
} else {
  iteration_num <- 0
  best_rmse <- Inf
  current_features <- initial_features
  best_features <- initial_features
  best_params <- list(ntrees=200, max_depth=6)
  rmse_results <- data.frame(Num_Features=integer(), RMSE=numeric(), R2=numeric(), PBIAS=numeric(), MASE=numeric())
  write.csv(rmse_results, results_file, row.names = FALSE)
}

# 9. Feature Elimination Loop (ONE FEATURE AT A TIME)
repeat {
  if (length(current_features) < 1) break
  iteration_num <- iteration_num + 1
  gc()
  
  cat(paste0("\n--- RFE Iteration ", iteration_num, " | Total Features: ", length(current_features), " ---\n"))

  # --- Manual Grid Search using hyper_params ---
  best_grid_rmse <- Inf
  for(nt in hyper_params$ntrees) {
    for(md in hyper_params$max_depth) {
      m <- h2o.xgboost(x = current_features, y = response, training_frame = train,
                       ntrees = nt, max_depth = md, nfolds = 5, backend='cpu', seed=123)
      rmse_val <- h2o.rmse(m, xval=TRUE)
      if (rmse_val < best_grid_rmse) {
        best_grid_rmse <- rmse_val
        best_params <- list(ntrees=nt, max_depth=md)
      }
      h2o.rm(m); gc()
    }
  }
  cat("--- Grid Search Complete. Best RMSE:", best_grid_rmse, "---\n")

  # --- Bagging / Feature Importance ---
  imp_list <- list()
  for(b in 1:5) {
    bag_model <- h2o.xgboost(x=current_features, y=response, training_frame=train,
                             ntrees=best_params$ntrees, max_depth=best_params$max_depth,
                             backend='cpu', seed=b)
    imp_list[[b]] <- as.data.frame(h2o.varimp(bag_model))[, c("variable","relative_importance")]
    h2o.rm(bag_model); gc()
  }
  combined_imp <- Reduce(function(x,y) merge(x,y,by="variable"), imp_list)
  combined_imp$mean_imp <- rowMeans(combined_imp[,-1])
  combined_imp <- combined_imp[order(combined_imp$mean_imp), ]  # lowest importance first

  # --- Train current model & compute metrics ---
  temp_model <- h2o.xgboost(x=current_features, y=response, training_frame=train,
                            ntrees=best_params$ntrees, max_depth=best_params$max_depth,
                            backend='cpu', seed=123)
  perf <- h2o.performance(temp_model, newdata=valid)
  curr_rmse <- h2o.rmse(perf)
  y_pred <- as.data.frame(h2o.predict(temp_model, valid))$predict
  y_actual <- as.data.frame(valid[[response]])[,1]

  new_row <- data.frame(
    Num_Features=length(current_features),
    RMSE=curr_rmse,
    R2=h2o.r2(perf),
    PBIAS=100*sum(y_pred-y_actual)/sum(y_actual),
    MASE=mean(abs(y_pred-y_actual))/mean(abs(y_actual - mean(as.data.frame(train[[response]])[,1])))
  )

  rmse_results <- rbind(rmse_results, new_row)
  write.csv(rmse_results, results_file, row.names=FALSE)

  if(curr_rmse < best_rmse){
    best_rmse <- curr_rmse
    best_features <- current_features
    cat("--- New Best Found! RMSE:", best_rmse, "Features:", length(best_features), "---\n")
  }

  # --- Drop single least important feature ---
  feature_to_drop <- combined_imp$variable[1]
  current_features <- setdiff(current_features, feature_to_drop)
  cat("Dropping feature:", feature_to_drop, "\n")

  h2o.rm(temp_model); gc()
  
  # --- Save checkpoint ---
  save(iteration_num, best_rmse, best_features, current_features, best_params, rmse_results, file=checkpoint_file)
}

# 10. Final Model & Reporting
final_model <- h2o.xgboost(x=best_features, y=response, training_frame=h2o.rbind(train, valid),
                           ntrees=best_params$ntrees, max_depth=best_params$max_depth,
                           backend='cpu', seed=123)
final_perf_test <- h2o.performance(final_model, newdata=test)
final_rmse_test <- h2o.rmse(final_perf_test)
final_r2_test <- h2o.r2(final_perf_test)
test_preds <- as.data.frame(h2o.predict(final_model, test))$predict
test_actuals <- as.data.frame(test[[response]])[,1]
f_mae <- mean(abs(test_preds - test_actuals))
f_naive_mae <- mean(abs(test_actuals - mean(test_actuals)))
final_mase_test <- f_mae / f_naive_mae
final_pbias_test <- 100 * sum(test_preds - test_actuals)/sum(test_actuals)

report_text <- paste0("Final Performance Report\nPrefix: ", prefix, "\n",
                      "Best Feature Count: ", length(best_features), "\n",
                      "Validation RMSE: ", best_rmse, "\n",
                      "Final Test RMSE: ", final_rmse_test, "\n",
                      "Final Test R2: ", final_r2_test, "\n",
                      "Final Test PBIAS: ", final_pbias_test, " %\n",
                      "Final Test MASE: ", final_mase_test, "\n")
writeLines(report_text, file.path(output_dir, paste0("final_report_", prefix, ".txt")))

h2o.saveModel(final_model, path=file.path(output_dir, paste0("final_model_", prefix)), force=TRUE)
write.csv(best_features, file.path(output_dir, paste0("best_features_", prefix, "_", script_id_num, ".csv")))
write.csv(as.data.frame(h2o.varimp(final_model)), file.path(output_dir, paste0("varimp_", prefix, "_", script_id_num, ".csv")), row.names=FALSE)
if(file.exists(checkpoint_file)) file.remove(checkpoint_file)
cat("Workflow Complete. Test RMSE:", final_rmse_test, "\n")

# 11. Hold session briefly
cat("Keeping H2O alive for 60 seconds before R exit...\n")
Sys.sleep(60)
