
#' convert an existing simulation variable into a different one
#' 
#' allows you to provide specific variable or unit conversions to create a new variable 
#' that can be used with \code{\link{plot_var_compare}}, and others. 
#' 
#' @param nc_file the netcdf output file
#' @param convert a function to convert variable based on other variables or offsets
#' @param name the name for the new variable
#' @param unit the units for the new variable
#' @param longname the longname for the new variable
#' 
convert_sim_var <- function(nc_file='output.nc', convert, name, unit, longname=name){
  nc <- nc_open(nc_file, readunlim=TRUE, write=TRUE)
  lon <- nc$dim$lon
  lat <- nc$dim$lon
  z <- nc$dim$z
  time <- nc$dim$time
  missval = 9.96920996838687e36
  var_new <- ncvar_def( "temp_conv", "degC", dim = list(lon,lat,z,time), missval, prec="double")
  nc = ncvar_add(nc, var_new)
  
  ncvar_put(nc, var_new, vals=ncvar_get(nc, 'temp')+2)
  nc_close(nc)
}