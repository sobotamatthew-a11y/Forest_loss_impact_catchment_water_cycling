# Make EI 1 Table 

library(gt)
library(tidyverse)

# 1. Create the data frame

varimp_data <- data.frame(
  Code = c("cly_pc_sav", "gauge_lon", "slp_dg_sav", "aridity", "high_prec_freq", 
           "sgr_dk_sav", "gwt_cm_sav", "ele_mt_sav", "ari_ix_sav", 
           "percent_loss_area"),
  Attribute_Name = c("Clay Content Percentage", "Gauge Longitude", 
                     "Terrain Slope", "Aridity Index", 
                     "High Precipitation Frequency", "Stream Gradient", 
                     "Groundwater Table Depth", "Elevation", 
                     "Global Aridity Index", "Percentage Forest Loss Area"),
  Category = c("Soils & Geology", "Metadata", "Physiography", "Climate", "Climate", "Physiography", 
                "Hydrology", "Physiography", "Climate", 
               "Land Cover"),
  Description = c("The spatial mean percentage of clay in the soil within the catchment.",
                  "The longitudinal coordinate of the streamflow gauging station.",
                  "The spatial mean of the terrain slope within the catchment.",
                  "The ratio of mean potential evapotranspiration to mean precipitation.",
                  "Frequency of high precipitation days.",
                  "The mean gradient of the stream reach segments.",
                  "The spatial mean depth to the groundwater table.",
                  "The spatial mean elevation of the catchment.",
                  "A standardized global index of aridity (spatial mean).",
                  "The percentage of the total catchment area that has experienced forest cover loss.")
)

# 2. Convert to a publication-ready 'gt' table
varimp_table <- varimp_data %>%
  gt() %>%
 tab_header(
    title = md("*Variable Importance for the 1-Year Pre- vs. Post-Forest Loss Envaporative Index (EI) Analysis Window*")
  ) %>%
  # Rename columns for the publication
  cols_label(
    Code = md("**Feature Code**"),
    Attribute_Name = md("**Full Attribute Name**"),
    Category = md("**Category**"),
    Description = md("**Brief Description**")
  ) %>%
  # A4 SCALING: Using percentages ensures it fits within page margins (Total = 100%)
  cols_width(
    Code ~ pct(15),
    Attribute_Name ~ pct(25),
    Category ~ pct(15),
    Description ~ pct(45) # Maximize space for description to prevent vertical stretching
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.width = pct(100), # Forces the table to exactly match A4 margins
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.top.color = "black",
    table_body.border.bottom.color = "black",
    table_body.hlines.color = "transparent" # Removes horizontal grid lines
  ) %>%
  # Font and Alignment
  opt_table_font(font = "Times New Roman") %>%
  cols_align(align = "left", columns = everything())

# 3. View the table
varimp_table

gtsave(
  zoom = 2,
  data = varimp_table,
  filename = "Table_EI1.png",
  path = "D:/Forest_loss/HPC/Plots", # Or your preferred folder
  vwidth = 900,   # High width ensures the A4 horizontal scaling looks correct
  vheight = 600   # Adjust height based on how many rows you have
)
