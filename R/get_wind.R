#'@title get wind speed from GLM simulation
#'@description 
#'Creates a data.frame with DateTime and wind.  \cr
#'
#'
#'@param file a string with the path to the netcdf output from GLM
#'@return a data.frame with DateTime and wind
#'@keywords methods
#'@author
#'Luke A. Winslow, Jordan S. Read
#'@examples 
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'wind <- get_wind(file = nc_file)
#'@import ncdf
#'
#'@export
get_wind <-  function(file){
  glm_nc <- get_glm_nc(file)
  wind <- get.var.ncdf(glm_nc, "wind")
  time <- get_time(glm_nc)
  
  glm_wind <- data.frame('DateTime'=time, 'wind'=wind)
  close_glm_nc(glm_nc)
  return(glm_wind)
}
