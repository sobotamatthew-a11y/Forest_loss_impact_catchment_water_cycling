# Catchment Properties Regulate Hydrologic Responses to Forest Loss Across Global Watersheds

This repository contains the code used to reproduce the analyses presented in the accompanying manuscript. The study integrates global catchment datasets with forest disturbance data to evaluate how catchment characteristics regulate hydrologic responses following forest loss.

## Project conceptual diagram
<img width="1280" height="720" alt="conceptual_diagram" src="https://github.com/user-attachments/assets/fb7f6409-287b-4285-9551-2825429acafb" />

## Data

This repository deoes not include the original datasets since they are available from their respective sources.

* CARAVAN catchment datasets
* Global Forest Change (Hansen et al., 2013)
* HydroATLAS
* ERA5-Land
* MacroSheds
* ROBIN

Please download these datasets from their original repositories before running the workflow.

## Workflow
1. Identify forest-loss years for each catchment
     - Performed in QGIS
       - Catchment-specific years of mode forest loss were identified using the Hansen et al. (2013) Global Forest Change dataset.

2. Calculate hydrologic response metrics
     - NetCDF_data_grab_*.R
       - Processes CARAVAN NetCDF streamflow and precipitation data to calculate pre- and post-disturbance Richards–Baker Index (RB) and Evaporative Index (EI) values for each catchment.

3. Merge hydrologic metrics with catchment attributes
     - add_attributes_to_loss_stat_frames.R
       - Combines calculated RB/EI metrics with catchment biophysical attributes for subsequent analyses.

4. Filter observations and remove correlated predictors
     - filter_full_record_RB_EI.R
       - Removes incomplete observations and physically unrealistic RB and EI values.
  
     - ranked_correlation_feature_drop.py
       - Identifies and removes highly correlated predictor variables using Spearman correlation.

5. Train Random Forest models 
     - RFE_master.R
       - Performs recursive feature elimination, hyperparameter tuning, and Random Forest model training to predict changes in RB Index and Evaporative Index and identify the most influential catchment characteristics.

6. Generate manuscript figures and tables
     - before_after_regression.m
       - Generates regression analyses comparing pre- and post-disturbance RB and EI across temporal windows.
    
     - plot_RFE_*.m
       - Creates recursive feature elimination performance metric figures (RMSE, MASE, PBIAS, and R²).
    
     - triple_set_varimp_plot_*.R
       - Generates variable importance figures, observed-versus-predicted plots, and model performance summaries.
    
     - supp_vars_tables.R 
       -  Creates supplementary tables describing predictor variables and Random Forest variable importance rankings.

     - vars_corr_longitude.R
       - Computes correlations between longitude and catchment attributes used in the manuscript.
