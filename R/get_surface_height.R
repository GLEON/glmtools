#'@title get surface height from GLM simulation
#'@description 
#'Creates a data.frame with DateTime and surface_height.  \cr
#'
#'
#'@param file a string with the path to the netcdf output from GLM
#'@param ice.rm a boolean for including ice thickness in surface height
#'@param snow.rm a boolean for including snow depth thickness in surface height
#'@return a data.frame with DateTime and surface_height (in meters)
#'@keywords methods
#'@author
#'Jordan S. Read, Luke A. Winslow
#'@examples 
#'\dontrun{
#'file <- system.file('extdata', 'output.nc', package = 'glmtools')
#'surface <- get_surface_height(file)
#'surface_w_ice <- get_surface_height(file, ice.rm = FALSE, snow.rm = FALSE)
#'}
#'@import ncdf
#'@export
get_surface_height  <-	function(file, ice.rm = TRUE, snow.rm = TRUE){
  glm_nc <- get_glm_nc(file)
  NS	<- 	get.var.ncdf(glm_nc, "NS")
  elev <- get.var.ncdf(glm_nc, "z")
  time <- get_time(glm_nc)
  close_glm_nc(glm_nc)
  
  surface_height <- vector(mode = "numeric",length = length(NS))
  for (j in 1:length(NS)){
    surface_height[j] <- elev[NS[j],j]
  }
  if (!ice.rm){
    surface_height <- surface_height + get_ice(file, snow.rm = TRUE)[, 2]
  }
  if (!snow.rm){
    snow <- get_ice(file, snow.rm = TRUE)[, 2] - get_ice(file, snow.rm = TRUE)[, 2]
    surface_height <- surface_height + snow
  }
  
  glm_surface <- data.frame('DateTime'=time, 'surface_height'=surface_height)
  
  return(glm_surface)
}
