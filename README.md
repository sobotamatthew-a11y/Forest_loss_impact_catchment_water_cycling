# Catchment Properties Impact Water Cycling in the Context of Forest Loss: A Global Review

## Predict changes in water cycling from global catchment databases before vs after meaningful forest loss events

This project leverages data from the Hansen et al. 2013 Global Forest Cover Change dataset to identify the year of greatest forest loss in catchments across the world. Long-term water cycling variables (RB-index and evaporative index) 
are calculated from catchment databases (CARAVAN, ROBIN, MacroSheds, ect.). In each catchment, the water cycling time series is broken up into periods before and after the year of greatest forest loss so that change in hydrologic
regulation can be measured before and after the forest loss year. Each catchment contains up to 200+ attributes related to hydrological, geomorphic, anthropogenic, geological, and anthropogenic characteristics. Random forest models 
are used to test the importance of these attributes on the changes in catchment hydrologic regulation (RB and EI indices) after forest loss. 

### Project conceptual diagram
<img width="1280" height="720" alt="conceptual_diagram" src="https://github.com/user-attachments/assets/fb7f6409-287b-4285-9551-2825429acafb" />

### File descriptions
* NetCDF_data_grab_US.R
  - R script that loads .nc files containing precipitation and streamflow data for each catchment from CARAVAN database. The script loops through all CARAVAN US catchments and calculates RB-index and evaporative index before and after forest loss years. The .csv list of forest loss years from Hansen et al. 2013 was generated separately in QGIS.

* RFE_master.R
  - R script trains random forest models to impute the changes in RB-Index (RB) and evaporative index (EI) in the three temporal windows for each variables: 1-year, 2-year, and 5-year. Datasets of catchment attributes and RB-indices and EI are constructed separately. The script performs a hyperparameter grid search at each recursive feature elimination (RFE) iteration based on objective function. The script saves the optimal set of parameters and best model after RFE.

* EI1_table
  - R script that creates supplemental table listing and describing the RFE-selected catchment variables during the 1-year evaporative index window.
 
* EI2_table
  - R script that creates supplemental table listing and describing the RFE-selected catchment variables during the 2-year evaporative index window.
 
* EI5_table
  - R script that creates supplemental table listing and describing the RFE-selected catchment variables during the 5-year evaporative index window.
 
* RB1_table
  - R script that creates supplemental table listing and describing the RFE-selected catchment variables during the 1-year RB-index window.
 
* RB2_table
  - R script that creates supplemental table listing and describing the RFE-selected catchment variables during the 2-year RB-index window.
 
* RB5_table
  - R script that creates supplemental table listing and describing the RFE-selected catchment variables during the 5-year RB-index window.

* loss_yr_period_logic_guide.txt
  - .txt file describing the different periods water cycling variables are broken into before and after forest loss events in catchments.
 
* add_attributes_to_loss_stat_frames
  - R script that merges file with all calculated hydrologic variables (RB and EI) with their respective biophysical attribute variables.
 
* before_after_regression.m
  - MATLAB script plotting regression analysis of ΔRB and ΔEI: evaluating hydrologic changes across 1–5 year windows following forest loss. Includes linear fits, zero-change thresholds, and t-test significance markers (*).
 
* filter_full_record_RB_EI.R
  - R script that removes NA values from calculated RB and EI values in each temporal window dataset. This script also omits EI values outside of the physically plausible range of 0-1. RB values less than 0 are also removed.


