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
#'@export
resample_to_field <- function(nc_file, field_file, method = 'match', precision = 'hours', var_name='temp'){
  
  field_obs <- read_field_obs(field_file, var_name=var_name)
  
  time_info <- get_time_info(file = nc_file)
  start_date <- time_info$startDate
  stop_date <- time_info$stopDate
  # get rid of dates that don't overlap
  
  field_obs <- trunc_time(field_obs, start_date, stop_date)
  
  dup_rows <- duplicated(field_obs[,1:2])
  if (any(dup_rows)){
    mssg <- paste0(' see rows ', paste(which(dup_rows), collapse=','))
    append_mssg <- ifelse(sum(dup_rows) < 10, mssg, '')
    stop(paste0('field file has one or more rows with duplicate date and depths.', append_mssg))
  }
  
  # -- cover case w/ no overlap?
  if (nrow(field_obs) == 0){stop('no field data overlap with simulation period')}
  
  unq_z <- sort(unique(field_obs$Depth))
  
  # build water temp data.frame
  var_data <- get_var(file = nc_file, reference = 'surface', var_name = var_name,
                     z_out = unq_z, t_out = unique(field_obs$DateTime), 
                     method = method, precision = precision)

  obs_time <- time_precision(field_obs$DateTime, precision) # apples to apples
   # -- may have time value duplication now --
  match_vals <- pivot_match(var_data, time = obs_time, depth = field_obs$Depth, var_name=var_name)
  validation <- data.frame('DateTime' = obs_time, 
                           'Depth' = field_obs$Depth, 
                           'Observed' = field_obs[, var_name],
                           'Modeled' = match_vals)
  
  names(validation)[3:4] = c(paste0('Observed_', var_name), paste0('Modeled_', var_name))
  
  return(validation)
}

pivot_match <- function(df, time, depth, var_name){
  
  
  #time and depth are 1D vectors (long-form tables)
  match_out <- vector(length = length(time))
  
  for (j in 1:length(time)){
    dt_match <- time[j]
    dp_match <- depth[j]
    col_nm <- match(paste0(var_name, '_', dp_match), names(df)) # this is not a numeric test!!! fix this (wtr_9.340 != 9.34)
    match_val <- df[df$DateTime==dt_match, col_nm]
    if (length(match_val) > 1){stop('duplicate date match')}
    match_out[j] <- ifelse(length(match_val)==0,NA,match_val)
  }
  if (any(is.nan(match_out))){warning("some values lost in match")}
  return(match_out)
}

