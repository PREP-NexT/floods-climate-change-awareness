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
write.csv(fldare_yearly_mean, "~/zhaoye/fldare_yearly_mean.csv")