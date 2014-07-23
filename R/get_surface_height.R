#'@title get surface height from GLM simulation
#'@description 
#'Creates a data.frame with DateTime and surface_height.  \cr
#'
#'
#'@param file a string with the path to the netcdf output from GLM
#'@param ice.rm a boolean for including ice thickness in surface height
#'@return a data.frame with DateTime and surface_height (in meters)
#'@keywords methods
#'@author
#'Jordan S. Read, Luke A. Winslow
#'@examples 
#'file = '../test/output.nc'
#'surface <- get_surface_height(file)
#'surface_w_ice <- get_surface_height(file, ice.rm = FALSE)
#'@import ncdf4
#'@export
get_surface_height  <-	function(file, ice.rm = TRUE){
  glm_nc <- get_glm_nc(file)
  NS	<- 	ncvar_get(glm_nc, "NS")
  elev <- ncvar_get(glm_nc, "z")
  time <- get_time(glm_nc)
  close_glm_nc(glm_nc)
  
  surface_height <- vector(mode = "numeric",length = length(NS))
  for (j in 1:length(NS)){
    surface_height[j] <- elev[NS[j],j]
  }
  if (!ice.rm){
    surface_height <- surface_height + get_ice(file)[, 2]
  }
  
  glm_surface <- data.frame('DateTime'=time, 'surface_height'=surface_height)
  
  return(glm_surface)
}
