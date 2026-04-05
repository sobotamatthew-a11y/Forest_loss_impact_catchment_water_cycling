#Forest loss study
#Training Validation Test and Varimp plots EI 1, 2, 5
#03/13/2026

# 1. System Settings & Timeouts
options(h2o.startH2O.timeout = 600)
options(h2o.request.timeout = 600)
Sys.setenv(no_proxy = "localhost,127.0.0.1")

library(h2o)
library(ggplot2)
library(tidyr)
library(dplyr)
library(forcats)

# --- Define and Create Output Directory ---
output_dir <- "D:/Forest_loss/HPC/Plots/Manuscript_plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Identify which EI run this is
run_name <- "ei5"   # change to "ei1", "ei2", or "ei5"

# 2. Initialize H2O
h2o.init(max_mem_size = "4G") 

h2o.removeAll() 

# 3. Load data 
data_path <- "D:/Forest_loss/HPC/030926_result/datasets/data_ei5.csv"
df_raw <- read.csv(data_path)

# 4. Split the data 
set.seed(123)
n <- nrow(df_raw)
spec <- sample(c(1, 2, 3), size = n, replace = TRUE, prob = c(0.7, 0.2, 0.1))

train <- as.h2o(df_raw[spec == 1, ])
valid <- as.h2o(df_raw[spec == 2, ])
test  <- as.h2o(df_raw[spec == 3, ])

# 5. Define variables
response <- "ei_diff1" 
### To be updated ###
predictors <- c(
  "area_sqkm",
  "loss_area_sqkm",
  "percent_loss_area",
  "gauge_lat",
  "gauge_lon",
  "area_fraction_used_for_aggregation",
  "ari_ix_sav",
  "cly_pc_sav",
  "dis_m3_pyr",
  "ero_kh_sav",
  "gwt_cm_sav",
  "hdi_ix_sav",
  "hft_ix_s09",
  "kar_pc_sse",
  "pop_ct_usu",
  "pst_pc_sse",
  "rdd_mk_sav",
  "sgr_dk_sav",
  "slt_pc_sav",
  "soc_th_sav",
  "high_prec_freq",
  "high_prec_dur",
  "aridity"
  
)

# --- Create a unique Grid ID for this specific run ---
grid_name <- paste0("rf_grid_", sample(1:10000, 1))

# 6. Execute Grid Search
cat("Starting Grid Search with ID:", grid_name, "...\n")
rf_grid <- h2o.grid(
  algorithm       = "randomForest",
  grid_id         = grid_name,
  x               = predictors,
  y               = response,
  training_frame  = train,
  validation_frame = valid,
  hyper_params    = list(
    ntrees      = c(100, 200, 500),
    max_depth   = c(6, 9, 12, 15),
    sample_rate = c(0.7, 0.8, 1.0),
    mtries      = c(2,4,6,8)  # c(2,4,6,8) when predictors > 2 c(1,2) when predictors = 2
  ),
  search_criteria = list(
    strategy = 'RandomDiscrete', max_models = 250, seed = 123
  ),
  seed = 123
)

# 7. Get Best Model & Retrain on Combined (Train + Valid)
grid_perf  <- h2o.getGrid(grid_id = grid_name, sort_by = "RMSE", decreasing = FALSE)
best_model <- h2o.getModel(grid_perf@model_ids[[1]])

final_train_combined <- h2o.rbind(train, valid)
best_p <- best_model@allparameters

cat("Training final model...\n")
final_rf_model <- h2o.randomForest(
  x               = predictors,
  y               = response,
  training_frame  = final_train_combined,
  ntrees          = best_p$ntrees,
  max_depth       = best_p$max_depth,
  sample_rate     = best_p$sample_rate,
  mtries          = best_p$mtries,
  seed            = 123
)

# --- Extract predictions for plotting ---
get_results <- function(model, data_h, label){
  
  preds   <- as.data.frame(h2o.predict(model, data_h))$predict
  actuals <- as.data.frame(data_h[[response]])[,1]
  
  rmse_val <- sqrt(mean((actuals - preds)^2, na.rm = TRUE))
  
  list(
    a = actuals,
    p = preds,
    rmse = rmse_val,
    label = label
  )
}

triple_results <- list(
  train = get_results(final_rf_model, train, "Training"),
  valid = get_results(final_rf_model, valid, "Validation"),
  test  = get_results(final_rf_model, test,  "Test")
)

# Automatically store results based on run_name
assign(paste0("results_", run_name), triple_results)

#After training each EI model:
final_rf_model_ei5 <- final_rf_model

################################################################################

### Triple Plot ###

make_3x3_ei_plot <- function(results_list){
  
  panel_labels <- c("(a)","(b)","(c)",
                    "(d)","(e)","(f)",
                    "(g)","(h)","(i)")
  
  # ----- SIZE SETTINGS -----
  plot_sizes <- list(
    axis_label_cex    = 1.0,
    point_cex         = 0.8,
    top_label_cex     = 1.35,
    panel_letter_cex  = 1.3,
    rmse_text_cex     = 1.05
  )
  
  ei_names <- names(results_list)
  
  # ----- GLOBAL AXIS LIMITS -----
  all_vals <- unlist(lapply(results_list, function(row){
    unlist(lapply(row, function(res) c(res$a, res$p)))
  }))
  lim <- range(all_vals, na.rm = TRUE)
  pad <- diff(lim) * 0.05
  lim <- c(lim[1]-pad, lim[2]+pad)
  
  # ----- LAYOUT -----
  layout(matrix(1:9, nrow=3, byrow=TRUE))
  par(mar=c(3.8,4.2,2,1), pty="s")
  
  panel_i <- 1
  
  for(row in 1:3){
    ei <- ei_names[row]
    row_results <- results_list[[ei]]
    
    for(col in 1:3){
      split_name <- names(row_results)[col]
      res <- row_results[[split_name]]
      show_y_axis <- (col == 1)
      
      # ----- SCATTER PLOT -----
      plot(res$a, res$p,
           xlim = lim, ylim = lim,
           xlab = "", ylab = "",
           pch = 19,
           col = rgb(0,0.2,0.6,0.5),
           cex = plot_sizes$point_cex,
           cex.lab = plot_sizes$axis_label_cex,
           cex.axis = 1.2,
           cex.main = plot_sizes$top_label_cex,
           axes = FALSE)
      
      abline(0,1,lty=2,lwd=2,col="red") # 1:1 line
      
      # ----- AXES -----
      if(show_y_axis) axis(2)
      axis(1)
      box()
      
      # ----- STATS -----
      text(
        x = lim[1] + 0.01 * diff(lim),
        y = lim[2] - 0.01 * diff(lim),  
        labels = paste0(
          "RMSE = ", format(round(res$rmse,3), nsmall=3),
          "\nR² = ", format(round(cor(res$a,res$p,use="complete.obs")^2,3), nsmall=3),
          "\nPBIAS = ", format(round(res$pbias,1), nsmall=1), " %",
          "\nMASE = ", format(round(res$mase,3), nsmall=3),
          "\nn = ", sum(complete.cases(res$a,res$p))
        ),
        adj = c(0,1),
        cex = plot_sizes$rmse_text_cex * 0.9,  
        family = "serif"
      )
      
      # Panel letters
      text(lim[2] - 0.02*diff(lim),
           lim[1] + 0.02*diff(lim),
           labels = panel_labels[panel_i],
           adj=c(1,0),
           cex = plot_sizes$panel_letter_cex,
           font=2)
      
      # Column titles
      if(row == 1){
        titles <- c("Training","Validation","Test")
        title(main=titles[col], cex.main=plot_sizes$top_label_cex)
      }
      
      # Y labels
      if(show_y_axis){
        y_labels <- c("Predicted EI1","Predicted EI2","Predicted EI5")
        mtext(y_labels[row], side=2, line=2.8, cex=plot_sizes$axis_label_cex)
      }
      
      # X labels
      if(col == 2){
        x_labels <- c("Observed EI1","Observed EI2","Observed EI5")
        mtext(x_labels[row], side=1, line=2.8, cex=plot_sizes$axis_label_cex)
      }
      
      panel_i <- panel_i + 1
    }
  }
  
  layout(1) # reset
}

###

add_metrics_to_results <- function(results_list){
  compute_metrics <- function(res){
    a <- res$a
    p <- res$p
    rmse_val <- sqrt(mean((a - p)^2, na.rm = TRUE))
    pbias_val <- 100 * sum(p - a, na.rm = TRUE) / sum(a, na.rm = TRUE)
    naive_scale <- mean(abs(diff(a)), na.rm = TRUE)
    mase_val <- mean(abs(p - a), na.rm = TRUE) / naive_scale
    res$rmse <- rmse_val
    res$pbias <- pbias_val
    res$mase <- mase_val
    res
  }
  
  lapply(results_list, function(ei_res){
    lapply(ei_res, compute_metrics)
  })
}

###

generate_final_outputs_from_results <- function(results_list, output_path){
  
  ei_names <- names(results_list)
  
  # --- Print summary ---
  for(ei in ei_names){
    cat("\n--- PERFORMANCE REPORT: ", ei, " ---\n")
    for(split in c("train","valid","test")){
      res <- results_list[[ei]][[split]]
      cat(toupper(split), 
          "RMSE:", round(res$rmse,3),
          "| R²:", round(cor(res$a,res$p,use="complete.obs")^2,3),
          "| PBIAS:", round(res$pbias,1),
          "| MASE:", round(res$mase,3), "\n")
    }
  }
  
  # --- Plot on screen ---
  make_3x3_ei_plot(results_list)
  
  # --- Save PNG ---
  png(file.path(output_path, "obs_vs_pred_EI_3x3.png"), width=6.5, height=6.5, units="in", res=600)
  make_3x3_ei_plot(results_list)
  dev.off()
}

# 1. Build initial results list
results_list <- list(
  ei1 = results_ei1,
  ei2 = results_ei2,
  ei5 = results_ei5
)

# 2. Compute and attach missing metrics
results_list <- add_metrics_to_results(results_list)

# 3. Generate summary, plot, and save PNG
generate_final_outputs_from_results(results_list, output_dir)

##################################################################################

# --- Variable Importance Plots ---

ei_models <- list(
  EI1 = final_rf_model_ei1,
  EI2 = final_rf_model_ei2,
  EI5 = final_rf_model_ei5
)

cat("Saving combined variable importance plot...\n")

# --- Extract and combine variable importance ---
vi_list <- lapply(ei_models, function(model) {
  vi <- h2o.varimp(model)
  vi[, c("variable", "relative_importance")]
})

# Get union of all variables
all_vars <- unique(unlist(lapply(vi_list, function(x) x$variable)))

# Initialize matrix
vi_mat <- matrix(0, nrow = length(all_vars), ncol = length(ei_models))
rownames(vi_mat) <- all_vars
colnames(vi_mat) <- names(ei_models)

# Fill matrix
for(i in seq_along(vi_list)){
  vi <- vi_list[[i]]
  
  # Normalize to percent
  rel_imp <- vi$relative_importance / sum(vi$relative_importance) * 100
  
  vi_mat[vi$variable, i] <- rel_imp
}

# Optional: select top variables across all models
top_n <- 15
top_vars <- names(sort(rowSums(vi_mat), decreasing = TRUE))[1:top_n]
vi_mat <- vi_mat[top_vars, ]

# Reverse for horizontal plotting
#vi_mat <- vi_mat[rev(rownames(vi_mat)), ]

# --- Convert matrix → data frame ---
vi_df <- as.data.frame(vi_mat)
vi_df$variable <- rownames(vi_df)

# Long format
vi_long <- vi_df %>%
  pivot_longer(
    cols = -variable,
    names_to = "Model",
    values_to = "Importance"
  )

# Scale importance for each model (0–100%)
vi_long <- vi_long %>%
  group_by(Model) %>%
  mutate(scaled_importance = Importance / sum(Importance, na.rm = TRUE) * 100) %>%
  ungroup()

# Order variables by total scaled importance (descending)
vi_long <- vi_long %>%
  group_by(variable) %>%
  mutate(total_scaled = sum(scaled_importance, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(variable = fct_reorder(variable, total_scaled, .desc = TRUE)) 

###

custom_labels <- c(
  "longitude",
  "soil clay content (%)",
  "stream gradient (mean)",
  "aridity",
  "high precip. frequency",
  "groundwater depth (mean)",
  "terrian slope (mean)",
  "latitude",
  "forest loss (% area)",
  "global aridity (mean)",
  "elevation (mean)",
  "karst area extent (mean)",
  "soil silt content (%)",
  "human development (mean)",
  "high precip. duration"
  
)

p <- ggplot(vi_long, aes(x = variable, y = scaled_importance, fill = Model)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  scale_x_discrete(labels = custom_labels) +
  scale_fill_manual(
    name = "Model",  
    values = c("steelblue", "darkorange", "forestgreen")
  ) +
  labs(
    x = "Catchment Feature",
    y = "Scaled Importance (%)",
    title = "Scaled Variable Importance Across EI Models"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8, margin = margin(t = 2)),  
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 11),
    axis.title.x = element_text(size = 11, margin = margin(t = 8)),
    plot.title = element_text(size = 12, face = "bold"),
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.direction = "horizontal",
    legend.title = element_text(size = 7.5, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 7),
    legend.key.size = unit(1, "lines"),   
    legend.background = element_rect(fill = alpha("white", 0.6), color = "black"),
    plot.margin = margin(10, 50, 10, 10)  # top, right, bottom, left
  ) +
  coord_cartesian(clip = "off")

print(p)

ggsave(
  filename = file.path(output_dir, "EI_varimp.png"),
  plot = p,
  width = 6,   
  height = 3.7,  
  units = "in",
  dpi = 300
)
