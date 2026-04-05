library(gt)
library(tidyverse)

# 1. Create the data frame
varimp_data <- data.frame(
  Code = c("gauge_lon", "cly_pc_sav", "slt_dg_sav", "hdi_ix_sav", "gwt_cm_sav", 
           "sgr_dk_sav", "high_prec_dur", "pst_pc_sse", "gauge_lat", 
           "high_prec_freq"),
  Attribute_Name = c("Gauge Longitude", "Clay Content Percentage", 
                     "Silt Content Percentage", "Human Development Index", 
                     "Groundwater Table Depth", "Stream Gradient", 
                     "High Precip. Duration", "Pasture Land Cover (%)", 
                     "Gauge Latitude", "High Precip. Frequency"),
  Category = c("Metadata", "Soils & Geology", "Soils & Geology", "Socioeconomic", "Hydrology", "Physiography", 
               "Climate", "Land Cover", "Metadata", 
               "Climate"),
  Description = c("The geographic longitudinal coordinate of the streamflow gauge.",
                  "The spatial mean percentage of clay in the soil within the catchment.",
                  "The spatial mean percentage of silt in the soil within the catchment.",
                  "A standardized index measuring social and economic development levels.",
                  "The spatial mean depth to the groundwater table.",
                  "The mean gradient of the stream reach segments.",
                  "The average length of time (duration) for high-precipitation events.",
                  "The percentage of the catchment area classified as pasture land.",
                  "The geographic latitudinal coordinate of the streamflow gauge.",
                  "Frequency of days where precipitation exceeds 5 times the daily mean.")
)

# 2. Convert to a publication-ready 'gt' table
varimp_table <- varimp_data %>%
  gt() %>%
  tab_header(
    title = md("*Variable Importance for the 5-Year Pre- vs. Post-Forest Loss Evaporative Index (EI) Analysis Window*")
  ) %>%
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
  filename = "Table_EI5.png",
  path = "D:/Forest_loss/HPC/Plots", # Or your preferred folder
  vwidth = 900,   # High width ensures the A4 horizontal scaling looks correct
  vheight = 600   # Adjust height based on how many rows you have
)
