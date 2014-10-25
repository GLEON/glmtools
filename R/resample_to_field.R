#'@title match GLM water temperatures with field observations
#'@param nc_file a string with the path to the netcdf output from GLM
#'@param field_file a string with the path to the field observation file
#'@param precision matching precision (must be 'secs', 'mins','hours', or 'days')
#'@return validation a data.frame with DateTime and temperature at depth 
#'@keywords methods
#'@seealso \link{resample_sim}, \link{get_temp}
#'@author
#'Jordan S. Read
#'@examples 
#'nc_file <- system.file('extdata', 'output.nc', package = 'glmtools')
#'field_file <- system.file('extdata', 'field_data.tsv', package = 'glmtools')
#'
#'temps <- resample_to_field(nc_file, field_file)
#'@export
resample_to_field <- function(nc_file, field_file, precision = 'days'){
  
  field_obs <- read_field_obs(field_file)
  time_info <- get_time_info(file = nc_file)
  start_date <- time_info$startDate
  stop_date <- time_info$stopDate
  # get rid of dates that don't overlap
  
  field_obs <- trunc_time(field_obs, start_date, stop_date)
  
  # -- cover case w/ no overlap?
  if (nrow(field_obs) == 0){stop('no field data overlap with simulation period')}
  
  unq_z <- unique(field_obs$Depth) # levels?
  
  # build water temp data.frame
  wTemps <- get_temp(file = nc_file, reference = 'surface', z_out = unq_z)
  
  
  wTemps <- trunc_time(wTemps, start_date = min(field_obs$DateTime), stop_date = max(field_obs$DateTime))
  wTemps <- resample_sim(df = wTemps, t_out = unique(field_obs$DateTime), method = 'match', precision)
  obs_time <- time_precision(field_obs$DateTime, precision) # apples to apples
   # -- may have time value duplication now --
  match_vals <- pivot_match(wTemps, time = obs_time, depth = field_obs$Depth)
  validation <- data.frame('DateTime' = obs_time, 
                           'Depth' = field_obs$Depth, 
                           'Observed_wTemp' = field_obs$wTemp,
                           'Modeled_wTemp' = match_vals)
  
  return(validation)
}

pivot_match <- function(df, time, depth){
  
  #time and depth are 1D vectors (long-form tables)
  match_out <- vector(length = length(time))
  
  for (j in 1:length(time)){
    dt_match <- time[j]
    dp_match <- depth[j]
    col_nm <- match(paste0('wtr_', dp_match), names(df)) # this is not a numeric test!!! fix this (wtr_9.340 != 9.34)
    match_val <- df[df$DateTime==dt_match, col_nm]
    if (length(match_val) > 1){stop('duplicate date match')}
    match_out[j] <- match_val
  }
  return(match_out)
}