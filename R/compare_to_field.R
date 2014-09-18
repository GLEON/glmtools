#'@title compare metric for GLM vs field observations
#'@description
#'compare metric for GLM vs field observations, but must have more than 3 matching time points. 
#'This function is only designed to handle calls to physical metrics that return a single value.
#'An example of this behavior is thermocline depth (but not water density for all depth-resolved measurements).
#'@param nc_file a string with the path to the netcdf output from GLM
#'@param field_file a string with the path to the field observation file
#'@param nml_file a string with the path to the nml file (optional)
#'@param metric a string representing a physical metric. 
#'Should be a rLakeAnalyzer function or other valid function.
#'@param as_value a boolean for calculating RMSE (F) or returning all values (T)
#'@param na.rm a boolean for remove NAs for RMSE calculation (only used if as_values == F)
#'@return a RMSE (in native units) for the comparison, or DateTime and all values as a data.frame (if as_values == T)
#'@keywords methods
#'@seealso \link{resample_time}, \link{resample_to_field}
#'@author
#'Jordan S. Read
#'@examples 
#'nc_file <- system.file('extdata', 'output.nc', package = 'glmtools')
#'field_file <- system.file('extdata', 'field_data.tsv', package = 'glmtools')
#'
#'CB_rmse <- compare_to_field(nc_file, field_file, metric = 'center.buoyancy')
#'CB_values <- compare_to_field(nc_file, field_file, 
#'                           metric = 'center.buoyancy', as_value = TRUE)
#'
#'thermo_values <- compare_to_field(nc_file, field_file, 
#'                           metric = 'thermo.depth', as_value = TRUE)
#'
#'\dontrun{
#'# -- an nml file is necessary when functions require hypsographic information
#'values <- compare_to_field(nc_file, field_file, 
#'                           metric = 'schmidt.stability', as_value = TRUE)
#'# -- will fail
#'nml_file <- system.file('extdata', 'glm.nml', package = 'glmtools')
#'values <- compare_to_field(nc_file, field_file, nml_file, 
#'                           metric = 'schmidt.stability', as_value = TRUE)
#'# -- will succeed
#'
#'# -- metrics can only be calculated by functions that are available to this environment
#'values <- compare_to_field(nc_file, field_file, metric = 'calc.fols', as_value = TRUE)
#'# -- will fail
#'}
#'@import ncdf4
#'@import rLakeAnalyzer
#'@export
compare_to_field <- function(nc_file, field_file, nml_file, metric, as_value = FALSE, na.rm = TRUE){
  
  if (missing(nml_file)){
    bthA <- NA
    bthD <- NA
  } else {
    hypso <- get_hypsography(file = nml_file)
    bthA <- hypso$Areas
    bthD <- hypso$Depths
  }
  compare_data <- resample_to_field(nc_file, field_file)
  
  un_dates <- unique(compare_data$DateTime)
  mod_metric <- vector('numeric', length = length(un_dates))
  obs_metric <- vector('numeric', length = length(un_dates))
  
  for (j in 1:length(un_dates)){
    date <- un_dates[j]
    u_i <- compare_data$DateTime == date
    depths <- compare_data$Depth[u_i]
    temp_obs <- compare_data[u_i, 3]
    temp_mod <- compare_data[u_i, 4]
    
    rmv_i <- is.na(temp_obs + temp_mod)
    mod_list <- list(wtr=temp_mod[!rmv_i], depths = depths[!rmv_i], bthA = bthA, bthD = bthD)
    obs_list <- list(wtr=temp_obs[!rmv_i], depths = depths[!rmv_i], bthA = bthA, bthD = bthD)
    use_names <- names(mod_list) %in% names(formals(metric)) # test to only use list elements that are inluded in the function args
    mod_metric[j] <- do.call(get(metric), mod_list[use_names]) 
    obs_metric[j] <- do.call(get(metric), obs_list[use_names]) 
  }
  
  if (as_value){
    compare.df <- data.frame('DateTime' = un_dates, 'obs' = obs_metric, 'mod' = mod_metric)
    return(compare.df)
  } else {
    RMSE <- sqrt(mean((mod_metric-obs_metric)^2 , na.rm = na.rm))
    return(RMSE)
  }
  
}