#'@title get evaporation from GLM simulation
#'@description 
#'Creates a data.frame with DateTime and E (mm/day).  \cr
#'
#'@param file a string with the path to the netcdf output from GLM
#'@return a data.frame with DateTime and evaporation (in mm/day)
#'@keywords methods
#'@author
#'Jordan S. Read
#'@examples 
#'file <- system.file('extdata', 'output.nc', package = 'glmtools')
#'evap <- get_evaporation(file)
#'plot(evap)
#'@import ncdf4
#'@export
get_evaporation <-  function(file){
  day_secs <- 86400
  m_to_mm <- 1000
  glm_nc <- get_glm_nc(file)
  evaporation <- ncvar_get(glm_nc, "evap")*day_secs*m_to_mm
  time <- get_time(glm_nc)
  
  glm_evaporation <- data.frame('DateTime'=time, 'evaporation(mm/d)'=evaporation)
  close_glm_nc(glm_nc)
  return(glm_evaporation)
}
