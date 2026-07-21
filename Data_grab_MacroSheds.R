#Forest loss study
#Extract MacroSheds streamflow and precip
#Calculate hydro regulation variables by catchment - 
#RB index and EI

library(stringr)
library(dataRetrieval)
library(tidyverse)
library(lubridate)
library(dplyr)

gage_loss_yrs=read.csv("D:/Forest_loss/MacroSheds/loss_year_extract/complete_loss_yrs.csv")

for (k in 1:12)
{

#k = 1

setwd("D:/Forest_loss/MacroSheds/sites_discharge")  
qdata = read.csv(paste(gage_loss_yrs$site_code[k], ".csv", sep = ""))
qdata$date = as.Date(qdata$date)

setwd("D:/Forest_loss/MacroSheds/sites_precip")  
pdata = read.csv(paste(gage_loss_yrs$site_code[k], ".csv", sep = ""))
pdata$date = as.Date(pdata$date) 
 
#Extract discharge and precip values separately
discharge = qdata[qdata$var == 'discharge',]
discharge = discharge[-c(1:2, 5:6, 8:10)]
colnames(discharge) = c('date', 'site_code', 'discharge')

#Convert streamflow (L/s) to mm/day
#catchment area (convert to m2)
catchment_area_km2 = gage_loss_yrs$ws_area_sqkm[k]
catchment_area_m2 = catchment_area_km2*1000000
  # Formula: mm/day = (L/s * 86,400) / area_m2
  discharge$discharge <- (discharge$discharge * 86400) / catchment_area_m2

precip = pdata[pdata$var == 'precipitation',]
precip = precip[-c(1:2, 5:6, 8:10)]
colnames(precip) = c('date', 'site_code', 'precipitation')

precip_discharge = merge(discharge, precip, by = 'date')
precip_discharge = precip_discharge[-c(4)]
colnames(precip_discharge) = c("date", "site_code", "discharge", "precipitation")

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

gage_loss_yrs$EI_before1[k] = 1 - sum(Before1$discharge)/sum(Before1$precipitation)
gage_loss_yrs$EI_after1[k] =  1 - sum(After1$discharge)/sum(After1$precipitation)
gage_loss_yrs$EI_after2[k] =  1 - sum(After2$discharge)/sum(After2$precipitation)
  
}

write.csv(gage_loss_yrs, 'D:/Forest_loss/Catchment_stats/5yr/MacroSheds_CatchmentStats_5yr.csv')
