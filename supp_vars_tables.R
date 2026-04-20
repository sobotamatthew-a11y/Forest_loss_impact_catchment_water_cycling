#Forest Loss Study
# 1. Create table that lists all variables used in RFE and RF modeling
# 2. Create descriptive tables of variable sets identified in scaled variable importance analysis

### Load and create attributes used in my study data sets ###

library(tidyverse)
library(gt)
library(dplyr)
library(stringr)

ei1 = read.csv("D:/Forest_loss/HPC/result_041326/datasets/ei1_data.csv")
ei2 = read.csv("D:/Forest_loss/HPC/result_041326/datasets/ei2_data.csv")
ei5 = read.csv("D:/Forest_loss/HPC/result_041326/datasets/ei5_data.csv")
rb1 = read.csv("D:/Forest_loss/HPC/result_041326/datasets/rb1_data.csv")
rb2 = read.csv("D:/Forest_loss/HPC/result_041326/datasets/rb2_data.csv")
rb5 = read.csv("D:/Forest_loss/HPC/result_041326/datasets/rb5_data.csv")

datasets <- list(ei1, ei2, ei5, rb1, rb2, rb5)
col_lists <- lapply(datasets, colnames)
all_cols <- unlist(col_lists)
unique_cols <- unique(all_cols)
drop_cols <- c("gauge_id", "gauge_name", "country", "loss_year", "ei_diff1", "rb_diff1")
filtered_cols <- setdiff(unique_cols, drop_cols)
columns_table <- data.frame(variable = filtered_cols)

write.csv(columns_table, "D:/Forest_loss/HPC/Plots/Supplemental_plot/all_vars_supp.csv")

### Add CARAVAN attributes to grab matching descriptions ###
supp_vars = read.csv("D:/Forest_loss/HPC/Plots/Supplemental_plot/all_vars_supp.csv")
descrips = read.csv("D:/Forest_loss/HPC/Plots/Supplemental_plot/vars_descrip.csv")

all(supp_vars$variable %in% descrips$hydroatlas_name)
missing_values <- setdiff(descrips$hydroatlas_name, supp_vars$variable)
descrips_clean <- descrips[!(descrips$hydroatlas_name %in% missing_values), ]
write.csv(descrips_clean, "D:/Forest_loss/HPC/Plots/Supplemental_plot/vars_descrip.csv")

### Make table ###

df = read.csv("D:/Forest_loss/HPC/Plots/Supplemental_plot/vars_descrip.csv", stringsAsFactors = FALSE)

df_clean <- df %>%
  mutate(unit = unit %>%
           str_replace_all("\u00A0", " ") %>%  
           str_replace_all("\u2212", "-") %>%  
           
           str_replace_all("m3 s-1", "m<sup>3</sup> s<sup>-1</sup>") %>%
           str_replace_all("106 m3", "10<sup>6</sup> m<sup>3</sup>") %>%
           str_replace_all("km2", "km<sup>2</sup>") %>%
           str_replace_all("m/km2", "m/km<sup>2</sup>") %>%
           
           str_replace_all("°", "&deg;") %>%
           str_replace_all("\\(x", "(&times;")
  ) %>%
  mutate(unit = case_when(
    is.na(unit) | unit == "" | unit == " " ~ "&#45;",
    TRUE ~ unit
  ))

final_table <- df_clean %>%
  gt(id = "my_table") %>% 
  fmt_markdown(columns = unit) %>% 
  tab_header(
    title = md("**Supplementary Table 1**"),
    subtitle = "Description of catchment attributes and hydro-climatic variables"
  ) %>%
  cols_label(
    hydroatlas_name = "Variable Code",
    description = "Description",
    aggregation = "Aggregation",
    unit = "Units"
  ) %>%
  cols_width(
    hydroatlas_name ~ px(125),
    description ~ px(300),
    aggregation ~ px(150),
    unit ~ px(100)
  ) %>%
  opt_css(
    css = "
    #my_table td[data-quarto-table-cell-role='content'], 
    #my_table .gt_row {
      word-break: break-all !important;
      white-space: normal !important;
    }
    "
  ) %>%
  tab_options(
    table.width = pct(100),
    table.font.size = px(12),
    column_labels.font.weight = "bold",
    data_row.padding = px(4),
    table.border.top.color = "black",
    table.border.bottom.color = "black"
  ) %>%
  cols_align(align = "left", columns = everything()) %>%
  opt_stylize(style = 1, color = "gray")

gtsave(final_table, "D:/Forest_loss/HPC/Plots/Supplemental_plot/Supplementary_Table.pdf")

###############################################################################
### MAKE SUPPLEMENTAL TABLES VARIMP EACH ANALYSIS WINDOW ###

### EI1 ###
ei1_varimp = list("cly_pc_sav", "gauge_lon", "high_prec_freq", "high_prec_dur", "percent_loss_area", "gwt_cm_sav", "ero_kh_sav", "soc_th_sav", "hdi_ix_sav", "ari_ix_sav", "ele_mt_sav", "area_fraction_used_for_aggregation")

df_ei1 = df %>%
  filter(hydroatlas_name %in% ei1_varimp)

ei1_final <- df_ei1 %>%
  mutate(unit = unit %>%
           str_replace_all("\u00A0", " ") %>%  
           str_replace_all("\u2212", "-") %>%  
           str_replace_all("m3 s-1", "m<sup>3</sup> s<sup>-1</sup>") %>%
           str_replace_all("106 m3", "10<sup>6</sup> m<sup>3</sup>") %>%
           str_replace_all("km2", "km<sup>2</sup>") %>%
           str_replace_all("m/km2", "m/km<sup>2</sup>") %>%
           str_replace_all("°", "&deg;") %>%
           str_replace_all("\\(x", "(&times;")
  ) %>%
  mutate(unit = case_when(
    is.na(unit) | unit == "" | unit == " " ~ "&#45;",
    TRUE ~ unit
  ))

ei_table <- ei1_final %>%
  gt() %>%
  fmt_markdown(columns = unit) %>%
  tab_header(
    title = md("*Variable Description for Model-Selected Attributes in the 1-Year Pre- vs. Post-Forest Loss Evaporative Index (EI) Analysis Window*")
  ) %>%
  cols_label(
    hydroatlas_name = "Feature Code",
    description = "Description",
    aggregation = "Aggregation", 
    unit = "Units"
  ) %>%
 
  tab_options(
    table.background.color = "white",
    heading.title.font.size = px(18),
    column_labels.font.weight = "bold",
    table.font.names = "Times New Roman", 
  
    table.border.top.style = "solid",
    table.border.top.width = px(3),
    table.border.top.color = "black",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(3),
    table.border.bottom.color = "black",
    heading.border.bottom.style = "solid",
    heading.border.bottom.width = px(2),
    heading.border.bottom.color = "black",
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "black",
    
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#f9f9f9",
    data_row.padding = px(6)
  ) %>%
  opt_row_striping() %>%
  cols_align(align = "left", columns = everything())

gtsave(
  ei_table, 
  "D:/Forest_loss/HPC/Plots/Supplemental_plot/EI1_table.png",
  vwidth = 1100,
  zoom = 2
)

### EI2 ###
ei2_varimp = list("cly_pc_sav", "gauge_lon", "high_prec_freq", "high_prec_dur", "percent_loss_area", "gwt_cm_sav", "ero_kh_sav", "soc_th_sav", "pet_mean", "ari_ix_sav", "slt_pc_sav", "cls_cl_smj")

df_ei2 = df %>%
  filter(hydroatlas_name %in% ei2_varimp)

ei2_final <- df_ei2 %>%
  mutate(unit = unit %>%
           str_replace_all("\u00A0", " ") %>%  
           str_replace_all("\u2212", "-") %>%  
           str_replace_all("m3 s-1", "m<sup>3</sup> s<sup>-1</sup>") %>%
           str_replace_all("106 m3", "10<sup>6</sup> m<sup>3</sup>") %>%
           str_replace_all("km2", "km<sup>2</sup>") %>%
           str_replace_all("m/km2", "m/km<sup>2</sup>") %>%
           str_replace_all("°", "&deg;") %>%
           str_replace_all("\\(x", "(&times;")
  ) %>%
  mutate(unit = case_when(
    is.na(unit) | unit == "" | unit == " " ~ "&#45;",
    TRUE ~ unit
  ))

ei_table2 <- ei2_final %>%
  gt() %>%
  fmt_markdown(columns = unit) %>%
  tab_header(
    title = md("*Variable Description for Model-Selected Attributes in the 2-Year Pre- vs. Post-Forest Loss Evaporative Index (EI) Analysis Window*")
  ) %>%
  cols_label(
    hydroatlas_name = "Feature Code",
    description = "Description",
    aggregation = "Aggregation", 
    unit = "Units"
  ) %>%
  
  tab_options(
    table.background.color = "white",
    heading.title.font.size = px(18),
    column_labels.font.weight = "bold",
    table.font.names = "Times New Roman", 
    
    table.border.top.style = "solid",
    table.border.top.width = px(3),
    table.border.top.color = "black",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(3),
    table.border.bottom.color = "black",
    heading.border.bottom.style = "solid",
    heading.border.bottom.width = px(2),
    heading.border.bottom.color = "black",
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "black",
    
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#f9f9f9",
    data_row.padding = px(6)
  ) %>%
  opt_row_striping() %>%
  cols_align(align = "left", columns = everything())

gtsave(
  ei_table2, 
  "D:/Forest_loss/HPC/Plots/Supplemental_plot/EI2_table.png",
  vwidth = 1100,
  zoom = 2
)

### EI5 ### 
ei5_varimp = list("cly_pc_sav", "gauge_lon", "high_prec_dur", "percent_loss_area", "gwt_cm_sav", "ero_kh_sav", "soc_th_sav", "hdi_ix_sav", "pet_mean", "ari_ix_sav", "ele_mt_sav", "cls_cl_smj")

df_ei5 = df %>%
  filter(hydroatlas_name %in% ei5_varimp)

ei5_final <- df_ei5 %>%
  mutate(unit = unit %>%
           str_replace_all("\u00A0", " ") %>%  
           str_replace_all("\u2212", "-") %>%  
           str_replace_all("m3 s-1", "m<sup>3</sup> s<sup>-1</sup>") %>%
           str_replace_all("106 m3", "10<sup>6</sup> m<sup>3</sup>") %>%
           str_replace_all("km2", "km<sup>2</sup>") %>%
           str_replace_all("m/km2", "m/km<sup>2</sup>") %>%
           str_replace_all("°", "&deg;") %>%
           str_replace_all("\\(x", "(&times;")
  ) %>%
  mutate(unit = case_when(
    is.na(unit) | unit == "" | unit == " " ~ "&#45;",
    TRUE ~ unit
  ))

ei_table5 <- ei5_final %>%
  gt() %>%
  fmt_markdown(columns = unit) %>%
  tab_header(
    title = md("*Variable Description for Model-Selected Attributes in the 5-Year Pre- vs. Post-Forest Loss Evaporative Index (EI) Analysis Window*")
  ) %>%
  cols_label(
    hydroatlas_name = "Feature Code",
    description = "Description",
    aggregation = "Aggregation", 
    unit = "Units"
  ) %>%
  
  tab_options(
    table.background.color = "white",
    heading.title.font.size = px(18),
    column_labels.font.weight = "bold",
    table.font.names = "Times New Roman", 
    
    table.border.top.style = "solid",
    table.border.top.width = px(3),
    table.border.top.color = "black",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(3),
    table.border.bottom.color = "black",
    heading.border.bottom.style = "solid",
    heading.border.bottom.width = px(2),
    heading.border.bottom.color = "black",
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "black",
    
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#f9f9f9",
    data_row.padding = px(6)
  ) %>%
  opt_row_striping() %>%
  cols_align(align = "left", columns = everything())

gtsave(
  ei_table5, 
  "D:/Forest_loss/HPC/Plots/Supplemental_plot/EI5_table.png",
  vwidth = 1100,
  zoom = 2
)

### RB1 ###
rb1_varimp = list("cly_pc_sav", "percent_loss_area", "aridity", "moisture_index", "forest_loss_type", "high_prec_freq", "aet_mm_syr", "gauge_lon", "snd_pc_sav", "ari_ix_sav", "gauge_lat", "slt_pc_sav", "pre_mm_syr")

df_rb1 = df %>%
  filter(hydroatlas_name %in% rb1_varimp)

rb1_final <- df_rb1 %>%
  mutate(unit = unit %>%
           str_replace_all("\u00A0", " ") %>%  
           str_replace_all("\u2212", "-") %>%  
           str_replace_all("m3 s-1", "m<sup>3</sup> s<sup>-1</sup>") %>%
           str_replace_all("106 m3", "10<sup>6</sup> m<sup>3</sup>") %>%
           str_replace_all("km2", "km<sup>2</sup>") %>%
           str_replace_all("m/km2", "m/km<sup>2</sup>") %>%
           str_replace_all("°", "&deg;") %>%
           str_replace_all("\\(x", "(&times;")
  ) %>%
  mutate(unit = case_when(
    is.na(unit) | unit == "" | unit == " " ~ "&#45;",
    TRUE ~ unit
  ))

rb_table1 <- rb1_final %>%
  gt() %>%
  fmt_markdown(columns = unit) %>%
  tab_header(
    title = md("*Variable Description for Model-Selected Attributes in the 1-Year Pre- vs. Post-Forest Loss R-B Index (RB) Analysis Window*")
  ) %>%
  cols_label(
    hydroatlas_name = "Feature Code",
    description = "Description",
    aggregation = "Aggregation", 
    unit = "Units"
  ) %>%
  
  tab_options(
    table.background.color = "white",
    heading.title.font.size = px(18),
    column_labels.font.weight = "bold",
    table.font.names = "Times New Roman", 
    
    table.border.top.style = "solid",
    table.border.top.width = px(3),
    table.border.top.color = "black",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(3),
    table.border.bottom.color = "black",
    heading.border.bottom.style = "solid",
    heading.border.bottom.width = px(2),
    heading.border.bottom.color = "black",
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "black",
    
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#f9f9f9",
    data_row.padding = px(6)
  ) %>%
  opt_row_striping() %>%
  cols_align(align = "left", columns = everything())

gtsave(
  rb_table1, 
  "D:/Forest_loss/HPC/Plots/Supplemental_plot/RB1_table.png",
  vwidth = 1100,
  zoom = 2
)

### RB2 ###
rb2_varimp = list("cly_pc_sav", "percent_loss_area", "forest_loss_type", "high_prec_freq", "aet_mm_syr", "gauge_lon", "gauge_lat", "slt_pc_sav", "rdd_mk_sav", "pre_mm_syr")

df_rb2 = df %>%
  filter(hydroatlas_name %in% rb2_varimp)

rb2_final <- df_rb2 %>%
  mutate(unit = unit %>%
           str_replace_all("\u00A0", " ") %>%  
           str_replace_all("\u2212", "-") %>%  
           str_replace_all("m3 s-1", "m<sup>3</sup> s<sup>-1</sup>") %>%
           str_replace_all("106 m3", "10<sup>6</sup> m<sup>3</sup>") %>%
           str_replace_all("km2", "km<sup>2</sup>") %>%
           str_replace_all("m/km2", "m/km<sup>2</sup>") %>%
           str_replace_all("°", "&deg;") %>%
           str_replace_all("\\(x", "(&times;")
  ) %>%
  mutate(unit = case_when(
    is.na(unit) | unit == "" | unit == " " ~ "&#45;",
    TRUE ~ unit
  ))

rb_table2 <- rb2_final %>%
  gt() %>%
  fmt_markdown(columns = unit) %>%
  tab_header(
    title = md("*Variable Description for Model-Selected Attributes in the 2-Year Pre- vs. Post-Forest Loss R-B Index (RB) Analysis Window*")
  ) %>%
  cols_label(
    hydroatlas_name = "Feature Code",
    description = "Description",
    aggregation = "Aggregation", 
    unit = "Units"
  ) %>%
  
  tab_options(
    table.background.color = "white",
    heading.title.font.size = px(18),
    column_labels.font.weight = "bold",
    table.font.names = "Times New Roman", 
    
    table.border.top.style = "solid",
    table.border.top.width = px(3),
    table.border.top.color = "black",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(3),
    table.border.bottom.color = "black",
    heading.border.bottom.style = "solid",
    heading.border.bottom.width = px(2),
    heading.border.bottom.color = "black",
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "black",
    
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#f9f9f9",
    data_row.padding = px(6)
  ) %>%
  opt_row_striping() %>%
  cols_align(align = "left", columns = everything())

gtsave(
  rb_table2, 
  "D:/Forest_loss/HPC/Plots/Supplemental_plot/RB2_table.png",
  vwidth = 1100,
  zoom = 2
)

### RB5 ###
rb5_varimp = list("cly_pc_sav", "percent_loss_area", "aridity", "moisture_index", "forest_loss_type", "high_prec_freq", "aet_mm_syr", "gauge_lon", "snd_pc_sav", "ari_ix_sav", "gauge_lat", "rdd_mk_sav", "pet_mean")

df_rb5 = df %>%
  filter(hydroatlas_name %in% rb5_varimp)

rb5_final <- df_rb5 %>%
  mutate(unit = unit %>%
           str_replace_all("\u00A0", " ") %>%  
           str_replace_all("\u2212", "-") %>%  
           str_replace_all("m3 s-1", "m<sup>3</sup> s<sup>-1</sup>") %>%
           str_replace_all("106 m3", "10<sup>6</sup> m<sup>3</sup>") %>%
           str_replace_all("km2", "km<sup>2</sup>") %>%
           str_replace_all("m/km2", "m/km<sup>2</sup>") %>%
           str_replace_all("°", "&deg;") %>%
           str_replace_all("\\(x", "(&times;")
  ) %>%
  mutate(unit = case_when(
    is.na(unit) | unit == "" | unit == " " ~ "&#45;",
    TRUE ~ unit
  ))

rb_table5 <- rb5_final %>%
  gt() %>%
  fmt_markdown(columns = unit) %>%
  tab_header(
    title = md("*Variable Description for Model-Selected Attributes in the 5-Year Pre- vs. Post-Forest Loss R-B Index (RB) Analysis Window*")
  ) %>%
  cols_label(
    hydroatlas_name = "Feature Code",
    description = "Description",
    aggregation = "Aggregation", 
    unit = "Units"
  ) %>%
  
  tab_options(
    table.background.color = "white",
    heading.title.font.size = px(18),
    column_labels.font.weight = "bold",
    table.font.names = "Times New Roman", 
    
    table.border.top.style = "solid",
    table.border.top.width = px(3),
    table.border.top.color = "black",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(3),
    table.border.bottom.color = "black",
    heading.border.bottom.style = "solid",
    heading.border.bottom.width = px(2),
    heading.border.bottom.color = "black",
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "black",
    
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#f9f9f9",
    data_row.padding = px(6)
  ) %>%
  opt_row_striping() %>%
  cols_align(align = "left", columns = everything())

gtsave(
  rb_table5, 
  "D:/Forest_loss/HPC/Plots/Supplemental_plot/RB5_table.png",
  vwidth = 1100,
  zoom = 2
)
