################################################################################
# Extract precip and streamflow from ROBIN gauge .nc files
# Calculate RB-index and Evaporative index change before vs after forest loss
# Forest loss study
# Last updated 10/28/2025
################################################################################

library(stringr)
library(dataRetrieval)
library(tidyverse)
library(lubridate)
library(dplyr)
library(lares)

#Read in gauge list with loss year and ws area sqkm
gage_loss_yrs=read.csv("D:/Forest_loss/ROBIN/loss_yr.csv")
gage_loss_yrs = gage_loss_yrs[, -c(1)]

for (k in 1:2128)
{
  
  #Read in streamflow data
  setwd("D:/Forest_loss/ROBIN/data")
  qdata = read.csv(paste(gage_loss_yrs$ROBIN_ID[k], ".csv", sep = ""))
  colnames(qdata) = c('robin_id', 'date', 'discharge')
  qdata$date <- as.Date(qdata$date, format = "%Y-%m-%d")
  
  #Read in precip data
  setwd("D:/Forest_loss/ROBIN/precip_data")  
  pdata = read.table(paste(gage_loss_yrs$ROBIN_ID[k], ".txt", sep = ""))
  pdata = pdata[-1,]
  colnames(pdata) = c('lon', 'lat', 'y', 'm', 'd', 'doy', 'date', 'precip')
  pdata$date = as.Date(pdata$date)
  pdata$precip =  as.numeric(pdata$precip)
  
  #Convert streamflow cms to mm/day
  area_km2 = gage_loss_yrs$AREA[k]
  qdata$discharge = qdata$discharge * 86.4 / area_km2
  
  #Create data frame of streamflow and precip
  precip_discharge = merge(qdata, pdata, by = 'date')
  precip_discharge = precip_discharge[,-c(4:9)]
  
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
  
write.csv(gage_loss_yrs,"D:/Forest_loss/Catchment_stats/5yr/ROBIN_5yr.csv")  
