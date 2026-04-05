library(gt)
library(tidyverse)

# 1. Create the data frame
varimp_data <- data.frame(
  Code = c("area_sqkm", "dis_m3_pyr"),
  Attribute_Name = c("Drainage Area", "Annual Mean Discharge"),
  Category = c("Physiography", "Hydrology"),
  Description = c("The total surface area of the catchment that drains to the gauge.",
                  "The long-term average annual streamflow volume")
)

# 2. Convert to a publication-ready 'gt' table
varimp_table <- varimp_data %>%
  gt() %>%
  # Add Table Name (Header) - Title is now italicized per previous request
  tab_header(
    title = md("*Variable Importance for the 5-Year Pre- vs. Post-Forest Loss RB-Index (RB) Analysis Window*")
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
  filename = "Table_RB5.png",
  path = "D:/Forest_loss/HPC/Plots", # Or your preferred folder
  vwidth = 900,   # High width ensures the A4 horizontal scaling looks correct
  vheight = 600   # Adjust height based on how many rows you have
)
