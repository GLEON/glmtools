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
#'file = '../test/output.nc'
#'ice <- get_wind(file)
#'@export
get_wind <-  function(file){
  glm_nc <- get_glm_nc(file)
  wind <- ncvar_get(GLMnc, "wind")
  time <- get_time(glm_nc)
  
  glm_wind <- data.frame('DateTime'=time, 'wind'=wind)
  close_glm_nc(glm_nc)
  return(glm_ice)
}
