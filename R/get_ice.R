#'@title get ice depth from GLM simulation
#'@description 
#'Creates a data.frame with DateTime and ice.  \cr
#'
#'
#'@param file a string with the path to the netcdf output from GLM
#'@return a data.frame with DateTime and ice 
#'@keywords methods
#'@author
#'Luke A. Winslow, Jordan S. Read
#'@examples 
#'file = '../test/output.nc'
#'ice <- get_ice(file)
#'@export
get_ice <-  function(file){
  glm_nc <- get_glm_nc(file)
  ice <- ncvar_get(glm_nc, "hice")+ncvar_get(glm_nc, "hwice")+ncvar_get(glm_nc, "hsnow")
  time <- get_time(glm_nc)
  
  glm_ice <- data.frame('DateTime'=time, 'ice'=ice)
  close_glm_nc(glm_nc)
  return(glm_ice)
}
