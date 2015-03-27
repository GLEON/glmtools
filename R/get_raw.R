#'@title get raw data from GLM simulation
#'@description 
#'Returns the raw data in a GLM simulation nc file. \cr
#'
#'
#'@param file a string with the path to the netcdf output from GLM
#'@param param_name Name of parameter requested (see details for list)
#'@return the raw matrix or vector from the NC file
#'@details 
#' NS\cr
#' hice\cr
#' hsnow\cr
#' hwice\cr
#' precip\cr
#' evap\cr
#' I_0\cr
#' wind\cr
#' Tot_V\cr
#' V\cr
#' salt\cr
#' temp\cr
#' rho\cr
#' rad\cr
#' ext_coef\cr
#' z\cr
#'
#'@keywords methods
#'@author
#'Luke A. Winslow, Jordan S. Read
#'@examples 
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'wind <- get_raw(nc_file, 'wind') #raw wind data
#'@import ncdf
#'@seealso \code{\link{sim_vars}}
#'@export
get_raw <-  function(file='output.nc', param_name){
  glm_nc <- get_glm_nc(file)
  raw_data <- get.var.ncdf(glm_nc, param_name)
  
  close_glm_nc(glm_nc)
  return(raw_data)
}
