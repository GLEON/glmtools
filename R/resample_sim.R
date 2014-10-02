#'@title get subset of time from a generic timeseries data.frame
#'@description 
#'resamples the input data.frame to only have rows corresponding to matches between 
#'df$DateTime and t_out. Both df$DateTime and t_out are of type POSIXct, and the 
#'precision of the match is passed in through the \code{precision} argument. 
#'\emph{The order of t_out}, not df$DateTime is retained.
#'
#'@param df a data.frame with DateTime and potentially other columns
#'@param t_out a vector of POSIXct dates for matching to df$DateTime
#'@param method 'match' for exact match or 'interp' for temporal interpolated (not yet supported)
#'@param precision matching precision (must be 'secs', 'mins','hours', or 'days')
#'@return a data.frame with DateTime other original columns, resampled according to t_out
#'@keywords methods
#'@seealso \link{get_temp}, \link{get_wind}, \link{get_surface_height}, \link{get_evaporation}, \link{get_ice}
#'@author
#'Jordan S. Read
#'@examples 
#'file <- system.file('extdata', 'output.nc', package = 'glmtools')
#'temp_surf <- get_temp(file, reference = 'surface', z_out = c(0,1,2))
#'t_out <- as.POSIXct(c("2011-04-01", "2011-06-14", "2011-04-05", "2011-07-28"))
#'temp_out <- resample_sim(df = temp_surf, t_out = t_out, precision = 'day')
#'@export
resample_sim <- function(df, t_out, method = 'match', precision = 'day'){
  
  if (length(unique(t_out)) != length(t_out)){stop('t_out values must be unique')}
  if (is.null(t_out)){
    return(df)
  }
  
  if (method != "match"){stop(paste0('method ', method, ' not currently supported'))}
  
  # wish this could be vectorized, but we need to retain the order of *t_out*, not df
  time <- round(t_out, precision)
  time_compr <- round(df$DateTime, precision)
  idx_out <- vector(length = length(time))
  for (j in 1:length(time)){
    idx_out[j] = match(time[j], time_compr)
  }
  
  idx_out <- idx_out[!is.na(idx_out)]
  
  df <- df[idx_out, ]
  return(df)
  
}

trunc_time <- function(df, start_date, stop_date){
  idx_out <- df$DateTime >= start_date & df$DateTime <= stop_date
  df_out <- df[idx_out, ]
  return(df_out)
}