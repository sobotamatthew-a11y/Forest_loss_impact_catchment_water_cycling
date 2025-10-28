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
  - R script that loads .nc files containing precipitation and streamflow data for each catchment from CARAVAN database. The script loops through all CARAVAN US catchments and calculates RB-index and evaporative index before and
    after forest loss years. The .csv list of forest loss years from Hansen et al. 2013 was generated separately in QGIS.

* RF_predict_RB1.R
  - R script trains random forest models to impute the changes in RB-Index in the After1 period created in data grab script. Datasets of catchment attributes and RB-indices are contructed separately. The script performs a hyperparameter
    grid search at each recursive feature elimination (RFE) iteration based on objective function. The script saves the optimal set of parameters and best model after RFE.

* data_grab_ROBIN.R
  -  R script that loads files containing precipitation and streamflow data for each catchment from ROBIN database. The script loops through all CARAVAN US catchments and calculates RB-index and evaporative index before and
    after forest loss years. The .csv list of forest loss years from Hansen et al. 2013 was generated separately in QGIS.
