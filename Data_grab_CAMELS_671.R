#Forest loss study
#Extract camels 671 streamflow and precip
#07/29/2025

library(stringr)
library(dataRetrieval)
library(tidyverse)
library(lubridate)
library(dplyr)
library(lares)

#Read in gauge list with loss year and ws area sqkm
gage_loss_yrs=read.csv("D:/Forest_loss/camels_671/loss_yr.csv")
gage_loss_yrs$gauge_id = str_pad(gage_loss_yrs$gauge_id, 8, pad = '0')
gage_loss_yrs = gage_loss_yrs[, -c(1)]

for (k in 1:669)
{
#k = 114

#Read in streamflow data
setwd("D:/Forest_loss/camels_671/streamflow_obs")  
qdata = read.table(paste(gage_loss_yrs$gauge_id[k], "_streamflow_qc.txt", sep = "" ))
colnames(qdata) = c('gauge_id', 'year', 'month', 'day', 'discharge', 'A')
qdata$date <- as.Date(with(qdata, paste(year, month, day, sep = "-")), 
                   format = "%Y-%m-%d")

#Read in precip data
setwd("D:/Forest_loss/camels_671/precip/daymet_txt_output")  
pdata = read.table(paste(gage_loss_yrs$gauge_id[k], ".txt", sep = ""))
pdata = pdata[-1,]
colnames(pdata) = c('date', 'precip')
qdata$date = as.Date(qdata$date)

#Convert streamflow cfs to mm/day
catchment_area_km2 = gage_loss_yrs$area_sq_km[k]
qdata$discharge = qdata$discharge * (1/catchment_area_km2) * 2.446576

#Create data frame of streamflow and precip
precip_discharge = merge(qdata, pdata, by = 'date')
precip_discharge = precip_discharge[,-c(2:5, 7)]
precip_discharge$precip = as.numeric(precip_discharge$precip)

#Initialize the loss year
lossyear = gage_loss_yrs$loss_year[k]
lossstart = as.Date(paste(lossyear, 1, 1, sep = "-"), "%Y-%m-%d")
lossend = as.Date(paste(lossyear, 12, 31, sep = "-"), "%Y-%m-%d")

#Initialize before and after loss year frames
Before = precip_discharge[precip_discharge$date < lossstart,]
After = precip_discharge[precip_discharge$date > lossend,] 

Before = Before %>%
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date))

After = After %>%
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date)) 

#Index before and 2 after periods to calculate RB index and EI

Before1 = Before[Before$year > lossyear - 6, ]
After1 = After[After$year >= lossyear + 1 & After$year < lossyear + 6, ]
After2 = After[After$year >= lossyear + 6 & After$year < lossyear + 11, ]

### RB index calculation ###
RB_before1 = -999
RB_after1 = -999
RB_after2 = -999

RBn_b1=0
RBd_b1=0

if (length(Before1$discharge > 0))
{
  for(j in 2:length(Before1$discharge))
  {
    RBn_b1 = RBn_b1 + abs(Before1$discharge[j]-(Before1$discharge[j-1]))
    RBd_b1 = RBd_b1 + Before1$discharge[j]
  }
  RB_before1=RBn_b1/RBd_b1   
}

RBn_a1=0
RBd_a1=0

if (length(After1$discharge > 0))
{
  for(j in 2:length(After1$discharge))
  {
    RBn_a1 = RBn_a1 + abs(After1$discharge[j]-(After1$discharge[j-1]))
    RBd_a1 = RBd_a1 + After1$discharge[j]
  }
  RB_after1=RBn_a1/RBd_a1    
}

RBn_a2=0
RBd_a2=0

if (length(After2$discharge > 0))
{
  for(j in 2:length(After2$discharge))
  {
    RBn_a2 = RBn_a2 + abs(After2$discharge[j]-(After2$discharge[j-1]))
    RBd_a2 = RBd_a2 + After2$discharge[j]
  }
  RB_after2=RBn_a2/RBd_a2    
}

#gage_loss_yrs$RB_before1[k] = RB_before1

if (length(RB_before1) > 0) {
  gage_loss_yrs$RB_before1[k] <- RB_before1
} else {
  # assign NA or skip
  gage_loss_yrs$RB_before1[k] <- NA
}

#gage_loss_yrs$RB_after1[k] = RB_after1

if (length(RB_after1) > 0) {
  gage_loss_yrs$RB_after1[k] <- RB_after1
} else {
  # assign NA or skip
  gage_loss_yrs$RB_after1[k] <- NA
}

#gage_loss_yrs$RB_after2[k] = RB_after2

if (length(RB_after2) > 0) {
  gage_loss_yrs$RB_after2[k] <- RB_after2
} else {
  # assign NA or skip
  gage_loss_yrs$RB_after2[k] <- NA
}
### Calculate Evaporative Index ### 

gage_loss_yrs$EI_before1[k] = 1 - sum(Before1$discharge)/sum(Before1$precip)
gage_loss_yrs$EI_after1[k] =  1 - sum(After1$discharge)/sum(After1$precip)
gage_loss_yrs$EI_after2[k] =  1 - sum(After2$discharge)/sum(After2$precip)

}

write.csv(gage_loss_yrs,"D:/Forest_loss/Catchment_stats/5yr/ForestLoss_CatchmentStats_671_5yr.csv")
