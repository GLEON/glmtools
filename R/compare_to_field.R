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
#'@param na.rm a boolean for remove NAs (only used if as_values == F)
#'@param ... additional arguments passed to resample_to_field()
#'@return a RMSE (in native units) for the comparison, or DateTime and all values as a data.frame (if as_values == T)
#'@keywords methods
#'@seealso \link{resample_sim}, \link{resample_to_field}
#'@author
#'Jordan S. Read
#'@examples 
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'field_file <- file.path(sim_folder, 'field_data.tsv')
#'
#'thermo_values <- compare_to_field(nc_file, field_file, 
#'                           metric = 'thermo.depth', as_value = TRUE)
#'temp_rmse <- compare_to_field(nc_file, field_file, 
#'                           metric = 'water.temperature', as_value = FALSE)
#'print(paste(temp_rmse,'deg C RMSE'))
#'# function in development
#'buoy_file <- file.path(sim_folder, 'field_data.tsv')
#'temp_rmse <- compare_to_field(nc_file, buoy_file, 
#'                           metric = 'water.temperature', as_value = FALSE, 
#'                           method = 'interp',precision = 'hours')
#'print(paste(temp_rmse,'deg C RMSE'))
#'\dontrun{
#'# -- an nml file is necessary when functions require hypsographic information
#'values <- compare_to_field(nc_file, field_file, 
#'                           metric = 'schmidt.stability', as_value = TRUE)
#'# -- will fail
#'nml_file <- file.path(sim_folder, 'glm2.nml')
#'values <- compare_to_field(nc_file, field_file, nml_file, 
#'                           metric = 'schmidt.stability', as_value = TRUE)
#'# -- will succeed
#'
#'# -- metrics can only be calculated by functions that are available to this environment
#'values <- compare_to_field(nc_file, field_file, metric = 'calc.fols', as_value = TRUE)
#'# -- will fail
#'}
#'@import rLakeAnalyzer
#'@import dplyr
#'
#'@export
compare_to_field <- function(nc_file, field_file, nml_file, metric, 
                             as_value = FALSE, na.rm = TRUE, ...){

  if (missing(nml_file)){
    bthA <- NA
    bthD <- NA
  } else {
    hypso <- get_hypsography(file = nml_file)
    bthA <- hypso$areas
    bthD <- hypso$depths
  }
  compare_data <- resample_to_field(nc_file, field_file, ...)
  
  compare_data = group_by(na.omit(compare_data), DateTime) %>% filter(n() >= 3) %>% ungroup %>% as.data.frame
  
  .compare_to_field(compare_data, bthA, bthD, metric = metric, 
                    as_value = as_value, na.rm = na.rm)

}