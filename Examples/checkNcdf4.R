checkNcdf4 <- function(){
  require(ncdf4)
  nc = nc_open('../Data/output.nc4')
  if(nc[['format']] == "NC_FORMAT_NETCDF4"){
    return(TRUE)
  }
}