rm(list=ls(all=TRUE))
library(ncdf4)

get_data <- function(ncpath, ncname){
  ncfname <- paste(ncpath, ncname, ".nc", sep="")
  ncin <- nc_open(ncfname)
  return (ncin)
}

ncpath <- "/home/climate/hexg/GDFC/Hazard-Maps/Fluvial-Risk/"
ncname <- "annual_max_inundation_fraction"

ncin <- get_data(ncpath, ncname)
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon)
lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)
print(c(nlon,nlat))
print(ncin)
rl_5year <- ncvar_get(ncin, "rl_5year")

ncpath <- "/home/climate/hexg/GDFC/Hydrological-Fluxes-States/Yearly/"
ncname <- "inundation_area_yearly_0.250deg_1950_2016"
ncin <- get_data(ncpath, ncname)
print(ncin)
fldare <- ncvar_get(ncin, "fldare")
# Mean on the third dimension
fldare_yearly_mean <- apply(fldare, c(1,2), mean)
write.csv(fldare_yearly_mean, "~/zhaoye/fldare_yearly_mean.csv")



