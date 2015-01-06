#'@title get ice depth from GLM simulation
#'@description 
#'Creates a data.frame with DateTime and ice.  \cr
#'This function sums the thickness of the clear ice, white ice, 
#'and optionally, the thickness of snow for each timestep in the GLM model.
#'
#'@param file a string with the path to the netcdf output from GLM
#'@param snow.rm a boolean for ignoring snow depth in the calculation of ice thickness
#'@return a data.frame with DateTime and ice (in meters)
#'@keywords methods
#'@author
#'Luke A. Winslow, Jordan S. Read
#'@examples 
#'\dontrun{
#'file <- system.file('extdata', 'output.nc', package = 'glmtools')
#'ice <- get_ice(file)
#'ice_and_snow <- get_ice(file, snow.rm = FALSE)
#'plot(ice)
#'points(ice_and_snow, col = "red")
#'}
#'@import ncdf
#'@export
get_ice <-  function(file, snow.rm = TRUE){
  glm_nc <- get_glm_nc(file)
  ice <- get.var.ncdf(glm_nc, "hice") + get.var.ncdf(glm_nc, "hwice")
  if (!snow.rm){
    ice <- ice + get.var.ncdf(glm_nc, "hsnow")
  }
  time <- get_time(glm_nc)
  
  glm_ice <- data.frame('DateTime'=time, 'ice(m)'=ice)
  close_glm_nc(glm_nc)
  return(glm_ice)
}
