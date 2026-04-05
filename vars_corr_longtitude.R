
library(readr)
library(dplyr)

df <- read_csv("D:/Forest_loss/HPC/lon_corr_test_031726/data.csv")

numeric_df <- df %>% select(where(is.numeric))

# 2. Calculate Spearman correlation for gauge_lon with all other variables
cor_results <- cor(numeric_df, method = "spearman", use = "pairwise.complete.obs")["gauge_lon", ]

# 3. Sort and view the strongest correlations
cor_results_sorted <- sort(cor_results, decreasing = TRUE)
print(cor_results_sorted)

# 4. Perform a specific significance test for a key proxy 
aet_test <- cor.test(df$gauge_lon, df$aet_mm_syr, method = "spearman", exact = FALSE)
print(aet_test)

################################################################################

# Supplemental longitude correlation test

library(gt)
library(tidyverse)

# 1. Create the data frame with your correlation results
corr_data <- data.frame(
  Variable = c("aet_mm_syr", "cly_pc_sav", "hdi_ix_sav", "pre_mm_syr", "gauge_lat", "slt_pc_sav"),
  Correlation = c(0.67, 0.61, 0.47, 0.37, -0.69, -0.75),
  Interpretation = c(
    "Actual Evapotranspiration increases with longitude",
    "Clay content increases with longitude",
    "Human Development Index (Proxy for land use/intensity)",
    "Precipitation trends (Hydroclimatic gradient)",
    "Strong spatial covariance (NW-SE or NE-SW gradient)",
    "Silt content decreases significantly with longitude"
  )
)

library(gt)
library(tidyverse)

# 1. Create the data frame
corr_data <- data.frame(
  Variable = c("aet_mm_syr", "cly_pc_sav", "hdi_ix_sav", "pre_mm_syr", "gauge_lat", "slt_pc_sav"),
  Correlation = c(0.67, 0.61, 0.47, 0.37, -0.69, -0.75),
  Interpretation = c(
    "Actual Evapotranspiration increases with longitude",
    "Clay content increases with longitude",
    "Human Development Index (Proxy for land use/intensity)",
    "Precipitation trends (Hydroclimatic gradient)",
    "Strong spatial covariance (NW-SE or NE-SW gradient)",
    "Silt content decreases significantly with longitude"
  )
)

# 2. Convert to a 'gt' table
corr_table <- corr_data %>%
  gt() %>%
  tab_header(
    title = md("*Spearman Correlation ($\\rho$) of Key Environmental Variables with Gauge Longitude*")
  ) %>%
  cols_label(
    Variable = md("**Variable**"),
    Correlation = md("**Spearman Correlation ($\\rho$)**"),
    Interpretation = md("**Interpretation**")
  ) %>%
 
  cols_width(
    Variable ~ pct(20),
    Correlation ~ pct(30),
    Interpretation ~ pct(50) 
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.width = pct(100),
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.top.color = "black",
    table_body.border.bottom.color = "black",
    table_body.hlines.color = "transparent"
  ) %>%
  opt_table_font(font = "Times New Roman") %>%
  cols_align(align = "left", columns = c(Variable, Interpretation)) %>%
  cols_align(align = "center", columns = Correlation)

# 3. View and Save
corr_table

gtsave(
  zoom = 2,
  data = corr_table,
  filename = "Table_Lon_Correlations_Wide.png",
  path = "D:/Forest_loss/HPC/Plots",
  vwidth = 1000, # Increased width slightly for the save-out
  vheight = 500
)
