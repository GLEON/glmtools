#'@title get wind speed from GLM simulation
#'@description 
#'Creates a data.frame with DateTime and wind.  \cr
#'
#'
#'@param file a string with the path to the netcdf output from GLM
#'@param ... additional arguments passed to \code{\link{resample_sim}}
#'@return a data.frame with DateTime and wind
#'@keywords methods
#'@author
#'Luke A. Winslow, Jordan S. Read
#'@examples 
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output/output.nc')
#'wind <- get_wind(file = nc_file)
#'
#'@export
get_wind <-  function(file='output.nc', ...){
  
  glm_wind <- get_var(file = file, var_name = 'wind', ...)

  return(glm_wind)
}
