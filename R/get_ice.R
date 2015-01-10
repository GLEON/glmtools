#'@title get ice depth from GLM simulation
#'@description 
#'Creates a data.frame with DateTime and ice.  \cr
#'This function sums the thickness of the clear ice, white ice, 
#'and optionally, the thickness of snow for each timestep in the GLM model.
#'
#'@param file a string with the path to the netcdf output from GLM
#'@param snow.rm a boolean for ignoring snow depth in the calculation of ice thickness
#'@param ... additional arguments passed to \code{\link{resample_sim}}
#'@return a data.frame with DateTime and ice (in meters)
#'@keywords methods
#'@author
#'Luke A. Winslow, Jordan S. Read
#'@examples 
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'ice <- get_ice(nc_file)
#'ice_and_snow <- get_ice(nc_file, snow.rm = FALSE)
#'plot(ice)
#'points(ice_and_snow, col = "red")
#'@export
get_ice <-  function(file, snow.rm = TRUE, ...){
  ice <- get_var(file, var_name = "hice", ...)
  ice[, 2] <- ice[, 2] + get_var(file, var_name = "hwice", ...)[, 2]
  if (!snow.rm){
    ice[, 2] <- ice[, 2] + get_var(file, var_name = "hsnow", ...)[, 2]
  }
  colnames(ice)[2] <- 'ice(m)'

  return(ice)
}
