#'@title get raw data from GLM simulation
#'@description 
#'Returns the raw data in a GLM simulation nc file. \cr
#'
#'
#'@param file a string with the path to the netcdf output from GLM
#'@param param_name Name of parameter requested
#'@return the raw matrix or vector from the NC file
#'@keywords methods
#'@author
#'Luke A. Winslow, Jordan S. Read
#'@examples 
#'file <- system.file('extdata', 'output.nc', package = 'glmtools')
#'wind <- get_raw(file, 'temp') #raw elevation data
#'
#'@import ncdf4
#'@export
get_raw <-  function(file, param_name){
  glm_nc <- get_glm_nc(file)
  raw_data <- ncvar_get(glm_nc, param_name)
  
  close_glm_nc(glm_nc)
  return(raw_data)
}
