library(gt)
library(tidyverse)

# 1. Create the data frame
varimp_data <- data.frame(
  Code = c("snd_pc_sav", "ari_ix_sav", "percent_loss_area", "cly_pc_sav", "pst_pc_sse", 
           "p_mean", "gauge_lat", "ele_mt_sav", "low_prec_freq", 
           "high_prec_freq"),
  Attribute_Name = c("Sand Content Percentage", "Global Aridity Index", 
                     "% Forest Loss Area", "Clay Content Percentage", 
                     "Pasture Land Cover (%)", "Mean Annual Precipitation", 
                     "Gauge Latitude", "Mean Elevation", 
                     "Low Precipitation Frequency", "High Precip. Frequency"),
  Category = c("Soils & Geology", "Climate", "Land Cover", "Soils % Geology", "Land Cover", "Climate", 
               "Metadata", "Physiography", "Climate", 
               "Climate"),
  Description = c("The spatial mean percentage of sand in the soil within the catchment.",
                  "A standardized index of long-term moisture availability (P/PET).",
                  "The percentage of the total catchment area that experienced forest cover loss.",
                  "The spatial mean percentage of clay in the soil within the catchment.",
                  "The percentage of the catchment area classified as pasture land",
                  "The average total annual precipitation depth over the catchment.",
                  "The geographic latitudinal coordinate of the streamflow gauge.",
                  "The mean height of the catchment area above sea level.",
                  "Frequency of days where precipitation is less than 0.1mm.",
                  "Frequency of days where precipitation exceeds 5 times the daily mean.")
)

# 2. Convert to a publication-ready 'gt' table
varimp_table <- varimp_data %>%
  gt() %>%
  tab_header(
    title = md("*Variable Importance for the 1-Year Pre- vs. Post-Forest Loss RB-Index (RB) Analysis Window*")
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
  filename = "Table_RB1.png",
  path = "D:/Forest_loss/HPC/Plots", # Or your preferred folder
  vwidth = 900,   # High width ensures the A4 horizontal scaling looks correct
  vheight = 600   # Adjust height based on how many rows you have
)
