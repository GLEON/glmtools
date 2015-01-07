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
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'evap <- get_evaporation(nc_file)
#'plot(evap)
#'@export
get_evaporation <-  function(file){
  day_secs <- 86400
  m_to_mm <- 1000
  glm_evaporation <- get_var(file, var_name = "evap")
  glm_evaporation[, 2] <- glm_evaporation[, 2]*day_secs*m_to_mm
  
  names(glm_evaporation) <- c('DateTime', 'evaporation(mm/d)')
  
  return(glm_evaporation)
}
