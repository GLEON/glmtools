#'Run example simulation 
#'@param sim_folder the directory where simulation files will be copied/moved to. 
#'Will use a temporary directory if missing
#'@param verbose should operations and output of GLM be shown
#'@keywords methods
#'@seealso \code{\link[GLM3r]{run_glm}}
#'@importFrom GLM3r run_glm
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
  driver_file <- file.path(sim_folder, 'nldas_driver.csv')
  calibration_tsv <- file.path(sim_folder, 'field_data.tsv')
  calibration_csv <- file.path(sim_folder, 'field_data.csv')
  stage_csv <- file.path(sim_folder, 'field_stage.csv')
  ice_snow_csv <- file.path(sim_folder, 'snow_ice_depth_obs.csv')
  ice_dur_csv <- file.path(sim_folder, 'ice_duration_obs.csv')
  
  nc_file <- file.path(sim_folder, paste0(nc_out, '.nc'))
  # move glm2.nml to sim_folder

  file.copy(from = system.file('extdata', 'nldas_driver.csv', package = 'glmtools'), to = driver_file)
  if(verbose){cat('driver data file copied to ', driver_file,'\n')}
  
  file.copy(from = system.file('extdata', 'field_data.tsv', package = 'glmtools'), to = calibration_tsv)
  file.copy(from = system.file('extdata', 'field_data.csv', package = 'glmtools'), to = calibration_csv)
  file.copy(from = system.file('extdata', 'field_stage.csv', package = 'glmtools'), to = stage_csv)
  file.copy(from = system.file('extdata', basename(ice_snow_csv), package = 'glmtools'), to = ice_snow_csv)
  file.copy(from = system.file('extdata', basename(ice_dur_csv), package = 'glmtools'), to = ice_dur_csv)
  
  nml <- read_nml() # read in default nml from GLM3r
  nml <- set_nml(nml, arg_list = list('Kw'=0.331, 'lake_name'='Sparkling', 
                               'bsn_vals' = 15,
                               'H' = c(301.712, 303.018285714286, 304.324571428571, 305.630857142857, 306.937142857143, 308.243428571429, 309.549714285714, 310.856, 312.162285714286, 313.468571428571, 314.774857142857, 316.081142857143, 317.387428571429, 318.693714285714, 320),
                               'A' = c(0, 45545.8263571429, 91091.6527142857, 136637.479071429, 182183.305428571, 227729.131785714, 273274.958142857, 318820.7845, 364366.610857143, 409912.437214286, 455458.263571429, 501004.089928571, 546549.916285714, 592095.742642857, 637641.569),
                               'start' = '2010-04-15 00:00:00',
                               'stop' = '2010-12-30 00:00:00',
                               'dt' = 3600, 
                               'out_fn' = nc_out,
                               'nsave' = 24, 
                               'csv_point_nlevs' = 0,
                               'num_depths' = 3,
                               'lake_depth' = 18.288,
                               'the_depths' = c(0, 0.2, 18.288),
                               'the_temps' = c(3, 4, 4),
                               'the_sals' = c(0, 0, 0),
                               'subdaily' = FALSE,
                               'meteo_fl' = 'nldas_driver.csv',
															 'max_layer_thick' = 3,
															 'sw_factor' = 1.08,
															 'coef_wind_stir' = 0.402,
															 'coef_mix_hyp' = 0.2,
															 'coef_mix_KH' = 0.1,
															 'cd' = 0.0013,
															 'ce' = 0.00132,
															 'sed_temp_mean' = 4.5,
															 'sed_temp_amplitude' = 0.25,
															 'min_layer_thick' = 0.1,
															 'coef_mix_conv' = 0.20,
                               'num_outlet' = 0))
  
  if(verbose){cat('writing nml file to ', nml_file,'\n')}
  write_nml(glm_nml = nml, file = nml_file)
  
  GLM3r::run_glm(sim_folder = sim_folder, verbose = verbose)
  
  if(verbose){cat('simulation complete. \n*.nc output located in ', nc_file,'\n')}
  
  return(sim_folder)
}