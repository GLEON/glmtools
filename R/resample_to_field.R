#'Match GLM water temperatures with field observations
#'@param nc_file a string with the path to the netcdf output from GLM
#'@param field_file a string with the path to the field observation file
#'@param method 'match' for exact match or 'interp' for temporal interpolation
#'@param precision matching precision (must be 'secs', 'mins','hours', or 'days')
#'@param var_name 
#' Name of variable to look for in field_obs file. 
#' Should match a GLM simulation variable (see output from \code{\link{sim_vars}}).
#'@return validation a data.frame with DateTime and temperature at depth 
#'@keywords methods
#'@seealso \link{resample_sim}, \link{get_temp}
#'@author
#'Jordan S. Read
#'@examples 
#'nc_file <- system.file("extdata", "output.nc", package = "glmtools")
#'field_file <- system.file("extdata", "LakeMendota_field_data_hours.csv", package = "glmtools")
#'temps <- resample_to_field(nc_file, field_file)
#'
#'buoy_file <- system.file('extdata', 'LakeMendota_buoy_data.csv', package = 'glmtools')
#'temps <- resample_to_field(nc_file, buoy_file)
#'@import dplyr
#'@export
resample_to_field <- function(nc_file, field_file, method = 'match', precision = 'hours', var_name = 'temp'){
  
  # read field observations
  field_obs <- read_field_obs(field_file, var_name = var_name)
  
  # Check for duplicates in field file
  if (any(duplicated(field_obs[,1:2]))){
    mssg <- paste0(' see rows ', paste(which(dup_rows), collapse=','))
    append_mssg <- ifelse(sum(dup_rows) < 10, mssg, '')
    stop(paste0('field file has one or more rows with duplicate date and depths.', append_mssg))
  }
  
  # build model variable data.frame
  var_data <- get_var(file = nc_file, reference = 'surface', var_name = var_name,
                      z_out = sort(unique(field_obs$Depth)), t_out = unique(field_obs$DateTime), 
                      method = method, precision = precision)
  
  model.wide = pivot_longer(var_data, cols = starts_with(var_name), names_to = 'Depth', 
                            names_prefix = paste0(var_name,'_'), values_to = var_name, values_drop_na = T) %>% 
    mutate(Depth = as.numeric(Depth))
  
  # join model results to observations
  validation = field_obs %>% left_join(model.wide, by = c("DateTime", "Depth"))
  names(validation)[3:4] = c(paste0('Observed_', var_name), paste0('Modeled_', var_name))
  
  # -- cover case w/ no overlap?
  if (sum(is.na(validation[,4])) == nrow(validation)){stop('no field data overlap with simulation period')}
  
  return(validation)
}
