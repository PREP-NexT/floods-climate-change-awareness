install.packages("raster")
install.packages("ncdf4")
install.packages("tidyverse")
install.packages("rgdal")
library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)
library(purrr)
library(magrittr)
library(tidyverse)

get_data <- function(ncpath, ncname){
  ncfname <- paste(ncpath, ncname, ".nc", sep="")
  ncin <- nc_open(ncfname)
  return (ncin)
}

ncpath <- "/home/climate/hexg/GDFC/Meteorological-Forcings/Yearly/"
ncname <- "PGFv3_tmin_yearly_0.250deg_1948_2016"
ncin <- get_data(ncpath, ncname)
print(ncin)
tmin <- ncvar_get(ncin, "tmin")
# Mean on the third dimension
tmin_yearly_mean <- apply(tmin, c(1,2), median)
# write.csv(tmin_yearly_mean, "~/zhaoye/tmin_yearly_mean.csv")


#streamflow.nc <- "C:/Users/User/Desktop/streamflow.nc"
#streamflow <- brick(streamflow.nc)
shp <- "~/zhaoye/country_mask_0.25deg.nc"
country <- brick(shp)
r <- raster(xmn=-180,xmx=180,ymn=-60,ymx=90,nrow=600,ncol=1440)
r[] <- values(country)[1:864000,]
country <- r
tmin_yearly_mean_raster <- brick(raster(t(tmin_yearly_mean)))
e <- extent(-180,180,-60,90)
extent(country) <- e
country <- setExtent(country, e, keepres=TRUE)
extent(tmin_yearly_mean_raster) <- e
tmin_yearly_mean_raster <- setExtent(tmin_yearly_mean_raster, e, keepres=TRUE)

cshape <- read.csv("~/zhaoye/cshape_country.csv")
cowcode <- cshape$COWCODE
country_name <- cshape$CNTRY_NAME
data <- data.frame(number=1:255,cowcode,country_name)
x = list(cowcode)
n = length(x)

country_cowcode <- function(cowcode) {
  countryname <- country %in% cowcode
  countryname[countryname<0.5]=NA
  a_countryname <- mask(tmin_yearly_mean_raster,countryname)
  countryname_streamflow <- as.data.frame(a_countryname)
  countryname_streamflow <- na.omit(countryname_streamflow)
  countryname_streamflow <- data.matrix(countryname_streamflow)
  countryname_streamflow <- mean(countryname_streamflow)
  return(countryname_streamflow)
}


loop <- function(country_cowcode) {
  for (i in 1:n){
    print(country_cowcode(x[i]))
    if (x == -1){
      break
    }
  }
}
tmin_yearly_mean_country <- lapply(x[[1]],country_cowcode)
data$tmin <- as.numeric(tmin_yearly_mean_country)
write.csv(data, "~/zhaoye/tmin.csv")

