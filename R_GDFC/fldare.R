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

ncpath <- "/home/climate/hexg/GDFC/Hazard-Maps/Fluvial-Risk/"
ncname <- "annual_max_inundation_fraction"
ncin <- get_data(ncpath, ncname)
print(ncin)
fldare <- ncvar_get(ncin, "fldare")
# Mean on the third dimension
fldare_yearly_mean <- apply(fldare, c(1,2), mean)
# write.csv(fldare_yearly_mean, "~/zhaoye/fldare_yearly_mean.csv")


#streamflow.nc <- "C:/Users/User/Desktop/streamflow.nc"
#streamflow <- brick(streamflow.nc)
shp <- "~/zhaoye/country_mask_0.25deg.nc"
country <- brick(shp)
fldare_yearly_mean_raster <- brick(raster(t(fldare_yearly_mean)))
e <- extent(-180,180,-90,90)
extent(country) <- e
country <- setExtent(country, e, keepres=TRUE)
extent(fldare_yearly_mean_raster) <- e
fldare_yearly_mean_raster <- setExtent(fldare_yearly_mean_raster, e, keepres=TRUE)

cshape <- read.csv("~/zhaoye/cshape_country.csv")
cowcode <- cshape$COWCODE
country_name <- cshape$CNTRY_NAME
data <- data.frame(number=1:255,cowcode,country_name)
x = list(cowcode)
n = length(x)

country_cowcode <- function(cowcode) {
  countryname <- country %in% cowcode
  countryname[countryname<0.5]=NA
  a_countryname <- mask(fldare_yearly_mean_raster,countryname)
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
fldare_yearly_mean_country <- lapply(x[[1]],country_cowcode)
data$fldare <- as.numeric(fldare_yearly_mean_country)
write.csv(data, "~/zhaoye/fldare.csv")

annual_max_inundation_fraction
fldare_yearly_mean
fldare_country_mean