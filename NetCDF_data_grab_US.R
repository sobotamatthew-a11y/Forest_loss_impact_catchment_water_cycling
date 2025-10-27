################################################################################
# Extract precip and streamflow from gauge .nc file 
# Calculate RB-index and Evaporative index change before vs after forest loss
# Forest loss study
# Last updated 10/27/2025
################################################################################

library(raster)
library(RNetCDF)
library(ncdf4)
library(tidyverse)
library(lubridate)
library(dplyr)

#set working directory to folder containing .nc files for each catchment
setwd("D:/Forest_loss/Caravan/timeseries/netcdf/camels")

#load file with forest loss year for each gauge
gage_loss_yrs=read.csv("d:/Forest_loss/Caravan/camels_us_loss_yr.csv")
lat = read.csv('D:/Forest_loss/Caravan/attributes/camels/attributes_other_camels.csv')
gage_loss_yrs = merge(lat, gage_loss_yrs, by = 'gauge_id')
gage_loss_yrs$num = gsub("[^0-9.-]", "", gage_loss_yrs$gauge_id)

####

#loop through each gauge .nc file and calculate RB-index and Evaporative index before and after forest loss year 
for (k in 1:482)
{
  #read .nc file into R environment
nc_data = nc_open(paste("camels_",gage_loss_yrs$num[k],".nc",sep=""))

#extract date value array, check units, and dimensions
date = ncvar_get(nc_data, "date")
tunits = ncatt_get(nc_data, "date", "units")

#extract streamflow values
sflow_array = ncvar_get(nc_data, "streamflow")

#extract precip
precip_array = ncvar_get(nc_data, "total_precipitation_sum")

#extract temp min & max
min_temp_array = ncvar_get(nc_data, "temperature_2m_min")
max_temp_array = ncvar_get(nc_data, "temperature_2m_max")

#format extracted date since into y-m-d
time_obs=as.Date(date, origin = '1981-01-01')

#Make frame to hold all flow and precip time series data
time_date = as.matrix(expand.grid(time_obs))
sflow_array_long=as.vector(sflow_array)
precip_array_long=as.vector(precip_array)
min_temp_array_long=as.vector(min_temp_array)
max_temp_array_long=as.vector(max_temp_array)
p_flow_temp_obs = data.frame(cbind(time_date, sflow_array, precip_array, min_temp_array, max_temp_array))
colnames(p_flow_temp_obs) = c("Date", "streamflow", "precipitation", "min_temp", "max_temp")
p_flow_temp_obs$Date = ymd(p_flow_temp_obs$Date)
p_flow_temp_obs$streamflow = as.numeric(p_flow_temp_obs$streamflow)
p_flow_temp_obs$precipitation = as.numeric(p_flow_temp_obs$precipitation)
p_flow_temp_obs$min_temp = as.numeric(p_flow_temp_obs$min_temp)
p_flow_temp_obs$max_temp = as.numeric(p_flow_temp_obs$max_temp)
p_flow_temp_obs = p_flow_temp_obs[!is.na(p_flow_temp_obs$streamflow),]
latitude = gage_loss_yrs$gauge_lat[k]

#Initialize the loss year
lossyear = gage_loss_yrs$loss_year[k]
lossstart = as.Date(paste(lossyear, 1, 1, sep = "-"), "%Y-%m-%d")
lossend = as.Date(paste(lossyear, 12, 31, sep = "-"), "%Y-%m-%d")

#Initialize before and after loss year frames
Before = p_flow_temp_obs[p_flow_temp_obs$Date < lossstart,]
After = p_flow_temp_obs[p_flow_temp_obs$Date > lossend,]

Before = Before %>%
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date))

After = After %>%
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date))

# adjust to set different time period frames (1 yr, 2 yr, 3 yr, ect.)
Before1 = Before[Before$year > lossyear - 6, ]
After1 = After[After$year >= lossyear + 1 & After$year < lossyear + 6, ]
After2 = After[After$year >= lossyear + 6 & After$year < lossyear + 11, ]

### RB index calculation ###
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

### Calculate Evaporative Index ### 

gage_loss_yrs$EI_before1[k] = 1 - (sum(Before1$streamflow)/sum(Before1$precipitation))
gage_loss_yrs$EI_after1[k] =  1 - (sum(After1$streamflow)/sum(After1$precipitation))
gage_loss_yrs$EI_after2[k] =  1 - (sum(After2$streamflow)/sum(After2$precipitation))

}

write.csv(gage_loss_yrs,"D:/Forest_loss/Catchment_stats/5yr/ForestLoss_CatchmentStats_US_5yr.csv")

