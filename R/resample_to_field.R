#'@title match GLM water temperatures with field observations
#'@param nc_file a string with the path to the netcdf output from GLM
#'@param field_file a string with the path to the field observation file
#'@return a data.frame with DateTime and temperature at depth 
#'@keywords methods
#'@seealso \link{resample_time}, \link{get_temps}
#'@author
#'Jordan S. Read
#'@examples 
#'nc_file <- system.file('extdata', 'output.nc', package = 'glmtools')
#'field_file <- system.file('extdata', 'field_data.tsv', package = 'glmtools')
#'
#'temps <- resample_to_field(nc_file, field_file)
#'@import ncdf4
#'@export
resample_to_field <- function(nc_file, field_file){
  
  field_obs <- read_field_obs(field_file)
  time_info <- get_time_info(file = nc_file)
  start_date <- time_info$startDate
  stop_date <- time_info$stopDate
  # get rid of dates that don't overlap
  # -- cover case w/ no overlap?
  field_obs <- trunc_time(field_obs, start_date, stop_date)
  
  unq_z <- unique(field_obs$Depth) # levels?
  
  return(df)
}