#'@title Run example simulation 
#'@param sim_folder the directory where simulation files will be copied/moved to. 
#'Will use a temporary directory if missing
#'@param verbose should operations and output of GLM be shown
#'@keywords methods
#'@seealso \code{\link[GLMr]{run_glm}}
#'@author
#'Jordan S. Read, Luke A. Winslow
#'@examples 
#'run_example_sim()
#'\dontrun{
#'run_example_sim('~/Desktop/GLM_sims')
#'}    
#'@export
run_example_sim = function(sim_folder, verbose = TRUE){
  
  nc_out = 'output'
  if (missing(sim_folder)){
    sim_folder <- tempdir()
    if(verbose){cat('sim_folder argument missing, using temp directory: ', sim_folder,'\n\n')}
  }
  nml_file <- file.path(sim_folder, 'glm2.nml')
  driver_file <- file.path(sim_folder, 'Anvil_driver.csv')
  calibration_tsv <- file.path(sim_folder, 'field_data.tsv')
  calibration_csv <- file.path(sim_folder, 'field_data.csv')
  stage_csv <- file.path(sim_folder, 'field_stage.csv')
  
  buoy_csv <- file.path(sim_folder, 'buoy_data.csv')
  nc_file <- file.path(sim_folder, paste0(nc_out, '.nc'))
  # move glm2.nml to sim_folder

  file.copy(from = system.file('extdata', 'Anvil_driver.csv', package = 'glmtools'),
            to = driver_file)
  if(verbose){cat('driver data file copied to ', driver_file,'\n')}
  
  file.copy(from = system.file('extdata', 'field_data.tsv', package = 'glmtools'), to = calibration_tsv)
  file.copy(from = system.file('extdata', 'field_data.csv', package = 'glmtools'), to = calibration_csv)
  file.copy(from = system.file('extdata', 'buoy_data.csv', package = 'glmtools'), to = buoy_csv)
  file.copy(from = system.file('extdata', 'field_stage.csv', package = 'glmtools'), to = stage_csv)
  
  nml <- read_nml() # read in default nml from GLMr
  nml <- set_nml(nml, arg_list = list('Kw'=0.55, 'lake_name'='Anvil', 
                               'bsn_vals' = 15,
                               'H' = c(510.5363, 511.23299, 511.92967, 512.62636, 513.32304, 514.01973, 514.71641, 515.4131, 516.10979, 516.80647, 517.50316, 518.19984, 518.89653, 519.59321, 520.2899),
                               'A' = c(0, 108.9645, 217.929, 326.8935, 435.858, 544.8225, 653.787, 762.7515, 871.716, 980.6805, 1089.645, 1198.6095, 1307.574, 1416.5385, 1525.503),
                               'start' = '2011-04-01 00:00:00',
                               'stop' = '2011-09-02 00:00:00',
                               'dt' = 3600, 
                               'out_fn' = nc_out,
                               'nsave' = 24, 
                               'csv_point_nlevs' = 0,
                               'num_depths' = 3,
                               'lake_depth' = 9.7536,
                               'the_depths' = c(0, 1.2, 9.7536),
                               'the_temps' = c(12, 10, 7),
                               'the_sals' = c(0, 0, 0),
                               'subdaily' = FALSE,
                               'meteo_fl' = 'Anvil_driver.csv',
                               'cd' = 0.00108,
                               'num_outlet' = 0))
  if(verbose){cat('writing nml file to ', nml_file,'\n')}
  write_nml(glm_nml = nml, file = nml_file)
  
  run_glm(sim_folder = sim_folder, verbose = verbose)
  
  if(verbose){cat('simulation complete. \n*.nc output located in ', nc_file,'\n')}
  
  return(sim_folder)
}