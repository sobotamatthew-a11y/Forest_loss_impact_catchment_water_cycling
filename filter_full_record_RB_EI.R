#Forest loss study
#Filter attribute/stats delta RB/EI data sets for complete record 
#Remove NAs from calculated EI and RB and constrain EI values between 0 and 1
#03/09/2026

library(dplyr)
library(tidyr)
d1 = read.csv('D:/Forest_loss/Complete_attr_stats_frames/1yr.csv')
d2 = read.csv('D:/Forest_loss/Complete_attr_stats_frames/2yr.csv')
d5 = read.csv('D:/Forest_loss/Complete_attr_stats_frames/5yr.csv')

### EI

#Remove NAs
df_ei1 <- d1 %>%
  filter(!is.na(ei_before1) & !is.na(ei_after1))

df_ei2 <- d2 %>%
  filter(!is.na(ei_before1) & !is.na(ei_after1))

df_ei5 <- d5 %>%
  filter(!is.na(ei_before1) & !is.na(ei_after1))

#Count out of bounds ei rows in each data set
count_out_of_bounds1 <- sum(
  (df_ei1$ei_before1 < 0 | df_ei1$ei_before1 > 1 |
     df_ei1$ei_after1  < 0 | df_ei1$ei_after1  > 1),
  na.rm = TRUE
)

count_out_of_bounds2 <- sum(
  (df_ei2$ei_before1 < 0 | df_ei2$ei_before1 > 1 |
     df_ei2$ei_after1  < 0 | df_ei2$ei_after1  > 1),
  na.rm = TRUE
)

count_out_of_bounds5 <- sum(
  (df_ei5$ei_before1 < 0 | df_ei5$ei_before1 > 1 |
     df_ei5$ei_after1  < 0 | df_ei5$ei_after1  > 1),
  na.rm = TRUE
)

# Remove precip timeseries error records pushing ei outside 0-1
EI_clean1 = df_ei1 %>%
  filter(ei_before1 >= 0 & ei_before1 <=1,
         ei_after1 >= 0 & ei_after1 <=1)

EI_clean2 = df_ei2 %>%
  filter(ei_before1 >= 0 & ei_before1 <=1,
         ei_after1 >= 0 & ei_after1 <=1)

EI_clean5 = df_ei5 %>%
  filter(ei_before1 >= 0 & ei_before1 <=1,
         ei_after1 >= 0 & ei_after1 <=1)

EI_clean$ei_diff1 = EI_clean$ei_before1 - EI_clean$ei_after1

write.csv(EI_clean, "D:/Forest_loss/HPC/rb_ei_032426/ei1.csv")

### RB
RB_clean <- d1 %>%
  filter(!is.na(rb_before1) & !is.na(rb_after1))

#Remove calcuated RB less than 0
RB_clean1 <- RB_clean %>%
  filter(ei_before1 >= 0, 
         ei_after1 >= 0)

RB_clean1$rb_diff1 = RB_clean1$rb_before1 - RB_clean1$rb_after1

write.csv(RB_clean1, "D:/Forest_loss/HPC/rb_ei_032426/rb1.csv")

###################################################################################


