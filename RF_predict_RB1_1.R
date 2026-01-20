###########################################################################
# Train RF models to impute delta RB-Index 1 based on catchment database attributes (1yr)
# Grid search at each RFE iteration to determine model hyperparameters
# For HPC use
# (01/20/2026)
###########################################################################

#install.packages("h2o")
library(bitops,lib.loc = '/home/mas22047/rlibs/')
library(h2o,lib.loc = '/home/mas22047/rlibs/')

#library(h2o)

# Check for SLURM environment variables and set defaults if not found
nthreads <- 24  # Default threads
mem_size <- "20G" # Default memory

# Get the number of cores from the SLURM environment variable
nthreads_val <- Sys.getenv("SLURM_CPUS_PER_TASK")
if (nchar(nthreads_val) > 0) {
  nthreads <- as.integer(nthreads_val)
  message(paste("SLURM_CPUS_PER_TASK detected. Using", nthreads, "threads."))
} else {
  message("SLURM_CPUS_PER_TASK not found. Using default threads.")
}

# Get the memory from the SLURM environment variable (in MB)
mem_mb_val <- Sys.getenv("SLURM_MEM_PER_NODE")
if (nchar(mem_mb_val) > 0) {
  # Convert MB to a string with a "G" suffix for h2o, e.g., "64G"
  mem_gb <- floor(as.numeric(mem_mb_val) / 1024)
  if (mem_gb < 1) { # If requested memory is less than 1 GB, use "1G"
    mem_size <- "1G"
  } else {
    mem_size <- paste0(mem_gb, "G")
  }
  message(paste("SLURM_MEM_PER_NODE detected. Using", mem_size, "memory."))
} else {
  message("SLURM_MEM_PER_NODE not found. Using default memory.")
}

# Initialize h2o with the specified resources
# The nthreads and max_mem_size arguments are crucial

h2o.init(nthreads = nthreads, max_mem_size = mem_size)

################################################################################3

data_path = "/home/mas22047/Forest_loss_RB_EI/rb1_1/"
output_dir = "/home/mas22047/Forest_loss_RB_EI/rb1_1/output/"

# Ensure output directory exists
if (!dir.exists(output_dir)) dir.create(output_dir)

RBdata = read.csv('RB_diff1_1yr.csv')
df = as.h2o(RBdata)

# Split Data: 80% train, 20% valid
splits <- h2o.splitFrame(df, ratios = 0.8, seed = 123)
train <- splits[[1]]
valid <- splits[[2]]

response = "rb_diff1"
features <- setdiff(names(RBdata), response)
# remove columns not related to RB or EI metrics
bad_cols = c('gauge_name', 'gauge_id', 'country')
features = features[!features %in% bad_cols]

# Store Best Model Info 
best_rmse <- Inf
best_features <- features
best_model <- NULL
best_params <- NULL
rmse_results <- data.frame(Num_Features = integer(), RMSE = numeric())

iteration_num = 0 # Initialize an iteration counter

###  Feature Elimination Loop ###
repeat {
  iteration_num = iteration_num + 1 # Increment counter
  
  # Print the current state before training
  cat(paste("\n--- RFE Iteration", iteration_num, "---"), "\n", flush = TRUE)
  cat(paste("Starting iteration with", length(features), "features."), "\n", flush = TRUE)
  
  #  Hyperparameter Grid 
  hyper_params <- list(
    ntrees = c(50, 100, 200, 300),
    max_depth = c(5, 10, 20, 30, 50),
    mtries = seq(1, min(length(features), 7)),  
    sample_rate = seq(0.6, 1.0, by = 0.2),
    col_sample_rate_per_tree = seq(0.6, 1.0, by = 0.2),
    min_rows = c(1, 5, 10, 20)
  )
  
  grid_id_current = paste0("rf_grid_", length(features)) # Assign grid_id to a variable for easier cleanup
  search_criteria <- list(strategy = "RandomDiscrete", max_models = 500, seed = 123) # change max_models when testing vs full run
  
  grid <- h2o.grid(
    algorithm = "randomForest",
    grid_id = grid_id_current,
    x = features,
    y = response,
    training_frame = train,
    validation_frame = valid,
    hyper_params = hyper_params,
    search_criteria = search_criteria,
    stopping_metric = "RMSE",
    seed = 123
  )
  
  #  Get Best Model 
  grid_perf <- h2o.getGrid(grid_id = grid_id_current, sort_by = "RMSE", decreasing = FALSE)
  best_grid_model <- h2o.getModel(grid_perf@model_ids[[1]])
  
  #  Evaluate 
  perf <- h2o.performance(best_grid_model, newdata = valid)
  rmse <- h2o.rmse(perf)
  
  cat(paste("Validation RMSE for", length(features), "features:", round(rmse, 4)), "\n", flush = TRUE)
  
  # Store RMSE Result 
  rmse_results <- rbind(rmse_results, data.frame(Num_Features = length(features), RMSE = rmse))
  
  if (rmse < best_rmse) {
    best_rmse <- rmse
    best_features <- features
    best_model <- best_grid_model
    best_params <- best_grid_model@allparameters
  }
  
  # Stop if only one feature left 
  if (length(features) <= 1) {
    cat(paste("Loop stopping: Only 1 feature remains."), "\n", flush = TRUE)
    break
  }
  
  #  Get Feature Importance and Remove Least Important 
  var_imp <- as.data.frame(h2o.varimp(best_grid_model))
  least_important <- var_imp[nrow(var_imp), "variable"]
  
  # Print the feature being removed
  cat(paste("Removing least important feature:", least_important), "\n", flush = TRUE)
  
  # Remove the feature
  features <- setdiff(features, least_important)
  
  # Print the number of features remaining for the *next* iteration
  cat(paste("Number of features remaining:", length(features)), "\n", flush = TRUE)
  
  # --- Memory Cleanup ---
  # Remove the current best model from the grid
  h2o.rm(best_grid_model) 
  # Remove the entire grid object and all its models
  h2o.rm(grid_id_current) 
  
}
cat(paste("\n\n--- RFE Process Complete ---"), "\n", flush = TRUE)
cat(paste("Best RMSE achieved:", round(best_rmse, 4)), "\n", flush = TRUE)
cat(paste("Number of features in best model:", length(best_features)), "\n", flush = TRUE)

### Train Final Model on Full Dataset ###
final_model <- h2o.randomForest(
  x = best_features,
  y = response,
  training_frame = df,
  ntrees = best_params$ntrees,
  max_depth = best_params$max_depth,
  mtries = best_params$mtries,
  seed = 123
)

#### Save Final Model & Results ###
h2o.saveModel(final_model, path = file.path(output_dir, "final_rf_model_1_h2o"), force = TRUE)
write.csv(best_features, file.path(output_dir, "selected_features_1.csv"), row.names = FALSE)
write.csv(rmse_results, file.path(output_dir, "rmse_results_1.csv"), row.names = FALSE)
