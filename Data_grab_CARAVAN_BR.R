#NetCDF Camels Brazil extract
#Forest loss study

library(raster)
library(RNetCDF)
library(ncdf4)
library(tidyverse)
library(lubridate)
library(dplyr)

setwd("D:/Forest_loss/Caravan/timeseries/netcdf/camelsbr")

gage_loss_yrs=read.csv("D:/Forest_loss/Caravan/camels_brazil_loss_yr.csv")
gage_loss_yrs[c("camels", "num")] = do.call(rbind, strsplit(gage_loss_yrs$gauge_id, "_"))

for (k in 1:376)
{
  nc_data = nc_open(paste("camelsbr_",gage_loss_yrs$num[k],".nc",sep=""))
  
  #extract date value array, check units, and dimensions
  date = ncvar_get(nc_data, "date")
  tunits=ncatt_get(nc_data, "date", "units")
  
  #extract streamflow values, check dimensions 
  sflow_array = ncvar_get(nc_data, "streamflow")
  fillvalue = ncatt_get(nc_data, "streamflow", "_FillValue")
  
  #extract precip
  precip_array = ncvar_get(nc_data, "total_precipitation_sum")
  fillvalue2 = ncatt_get(nc_data, "total_precipitation_sum", "_FillValue")
  
  #format extracted date since into y-m-d
  time_obs=as.Date(date, origin = '1981-01-01')
  
  #Make frame to hold all flow and precip time series data
  time_date = as.matrix(expand.grid(time_obs))
  sflow_array_long=as.vector(sflow_array)
  precip_array_long=as.vector(precip_array)
  precip_flow_obs = data.frame(cbind(time_date, sflow_array, precip_array))
  colnames(precip_flow_obs) = c("Date", "streamflow", "precipitation")
  precip_flow_obs$Date = ymd(precip_flow_obs$Date)
  precip_flow_obs$streamflow = as.numeric(precip_flow_obs$streamflow)
  precip_flow_obs$precipitation = as.numeric(precip_flow_obs$precipitation)
  precip_flow_obs = precip_flow_obs[!is.na(precip_flow_obs$streamflow),]
  
  #Initialize the loss year
  lossyear = gage_loss_yrs$loss_year[k]
  lossstart = as.Date(paste(lossyear, 1, 1, sep = "-"), "%Y-%m-%d")
  lossend = as.Date(paste(lossyear, 12, 31, sep = "-"), "%Y-%m-%d")
  
  #Initialize before and after loss year frames
  Before = precip_flow_obs[precip_flow_obs$Date < lossstart,]
  After = precip_flow_obs[precip_flow_obs$Date > lossend,]
  
  Before = Before %>%
    dplyr::mutate(year = lubridate::year(Date), 
                  month = lubridate::month(Date), 
                  day = lubridate::day(Date))
  
  After = After %>%
    dplyr::mutate(year = lubridate::year(Date), 
                  month = lubridate::month(Date), 
                  day = lubridate::day(Date))
  
  Before1 = Before[Before$year > lossyear - 6, ]
  After1 = After[After$year >= lossyear + 1 & After$year < lossyear + 6, ]
  After2 = After[After$year >= lossyear + 6 & After$year < lossyear + 11, ]
  
  #RB index calculation
  RB_before1 = -999
  RB_after1 = -999
  RB_after2 = -999
  
  RBn_b1=0
  RBd_b1=0
  
  if (length(Before1$streamflow > 0))
  {
    for(j in 2:length(Before1$streamflow))
    {
      RBn_b1 = RBn_b1 + abs(Before1$streamflow[j]-(Before1$streamflow[j-1]))
      RBd_b1 = RBd_b1 + Before1$streamflow[j]
    }
    RB_before1=RBn_b1/RBd_b1   
  }
  
  RBn_a1=0
  RBd_a1=0
  
  if (length(After1$streamflow > 0))
  {
    for(j in 2:length(After1$streamflow))
    {
      RBn_a1 = RBn_a1 + abs(After1$streamflow[j]-(After1$streamflow[j-1]))
      RBd_a1 = RBd_a1 + After1$streamflow[j]
    }
    RB_after1=RBn_a1/RBd_a1    
  }
  
  RBn_a2=0
  RBd_a2=0
  
  if (length(After2$streamflow > 0))
  {
    for(j in 2:length(After2$streamflow))
    {
      RBn_a2 = RBn_a2 + abs(After2$streamflow[j]-(After2$streamflow[j-1]))
      RBd_a2 = RBd_a2 + After2$streamflow[j]
    }
    RB_after2=RBn_a2/RBd_a2    
  }
  
  
  gage_loss_yrs$RB_before1[k] = RB_before1
  gage_loss_yrs$RB_after1[k] = RB_after1
  gage_loss_yrs$RB_after2[k] = RB_after2
  
  #Calculate Evaporative Index (1 - Q/P) for Before and After
  #Save those results into gage_loss_yrs
  
  gage_loss_yrs$EI_before1[k] = sum(Before1$streamflow)/sum(Before1$precipitation)
  gage_loss_yrs$EI_after1[k] = sum(After1$streamflow)/sum(After1$precipitation)
  gage_loss_yrs$EI_after2[k] = sum(After2$streamflow)/sum(After2$precipitation)
  
}

write.csv(gage_loss_yrs,"D:/Forest_loss/Catchment_stats/5yr/ForestLoss_CatchmentStats_BR_5yr.csv")
