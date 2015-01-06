#'@title get water temperatures from a GLM simulation
#'@description 
#'Creates a data.frame with DateTime and temperatures (in deg C).  \cr
#'Temperatures that are sampled out of the GLM output are taken relative 
#'to the surface (\code{reference = 'surface'}) or the bottom of the lake 
#'(\code{reference = 'bottom'}).
#'
#'@param file a string with the path to the netcdf output from GLM
#'@param reference a string which specifies the vertical reference ('surface' or 'bottom')
#'@param z_out an optional vector of depths for temperature output (in meters). 
#'If NULL, depths will be determined based on the depth of the lake
#'@param t_out a vector of POSIXct dates for temporal resampling (order is important)
#'@param ... additional arguments passed to \code{resample_sim()}
#'@return a data.frame with DateTime and temperature at depth 
#'@keywords methods
#'@seealso \link{resample_sim}
#'@author
#'Jordan S. Read, Luke A. Winslow
#'@examples 
#'\dontrun{
#'file <- system.file('extdata', 'output.nc', package = 'glmtools')
#'temp_surf <- get_temp(file, reference = 'surface', z_out = c(0,1,2))
#'temp_bot <- get_temp(file, reference = 'bottom', z_out = c(0,1,2))
#'temp_bot <- get_temp(file)
#'
#'#-- get temporal subset--
#'t_out <- seq(as.POSIXct("2011-04-04"), as.POSIXct("2011-06-01"), by = 86400)
#'temp_surf <- get_temp(file, reference = 'surface', z_out = 0, t_out = t_out)
#'plot(temp_surf)
#'}
#'@export
get_temp <-  function(file, reference = 'bottom', z_out = NULL, t_out = NULL, ...){
  glm_temp <- get_var(file, reference, z_out, t_out, var_name = 'temp')
  
  return(glm_temp)
}


