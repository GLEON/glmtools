#'@title Calibrates GLM-AED2 variables to improve fit between observed and simulated data
#'
#'@description Starts a calibration run with multiple iterations using a GLM-AED2 setup. At the end a output csv.-file of all iterations and a 
#'diagnostic plot are created.
#'@param var Character vector of valid variable names (see \code{\link{sim_vars}})
#'@param path String with the path to the GLM setup
#'@param field_file CSV or TSV field data (see \link{resample_to_field}for format)
#'@param nml_file String of the glm-namelist file, default is 'glm3.nml'
#'@param calib_setup Data frame containing information regarding the calibration (see \link{get_calib_setup})
#'@param glmcmd String containing the desired glm run command, default is GLM3r
#'@param first.attempt Boolean, if TRUE a nml-template will be created, set it to FALSE after your first calibration run; default is TRUE
#'@param period List that provides a start and a stop date for the simulation
#'@param scaling Boolean, if TRUE variable values will be scaled on the space (0,10), recommended for CMA-ES, default is TRUE
#'@param verbose should operations and output of GLM be shown. Default is TRUE. 
#'@param method String of the optimization method, default is 'CMA-ES' (Hansen 2009), alternatively you can also use 'Nelder-Mead'
#'@param metric String of the calibration fit metric, default is RMSE
#'@param target.fit Double of your preferred fit, calibration will stop after reaching that; default is 1.5
#'@param target.iter Double of maximum amount of iterations, default is 150
#'@param plotting Boolean, if TRUE plots all results as heat maps
#'@param output Character array of the output folder path 
#'@keywords methods
#'@seealso \code{\link{get_calib_setup}}, \code{\link{get_calib_periods}}, \code{\link{get_calib_init_validation}}
#'@author
#'Robert Ladwig, Tadhg Moore
#'@examples
#'calib_setup <- get_calib_setup()
#'print(calib_setup)
#'
#'#Example calibration
#'#Copy files into temporary directory
#'sim_folder <- tempdir() #simulation path
#'glmtools_folder = system.file('extdata', package = 'glmtools')
#'
#'file.copy(list.files(glmtools_folder,full.names = TRUE), sim_folder, overwrite = TRUE)
#'
#'field_file <- file.path(sim_folder, 'LakeMendota_field_data_hours.csv')
#'nml_file <- file.path(sim_folder, 'glm3.nml')
#'driver_file <- file.path(sim_folder, 'LakeMendota_NLDAS.csv')
#'period = get_calib_periods(nml_file = nml_file, ratio = 1)
#'output = file.path(sim_folder, 'output/output.nc')
#'
#'var = 'temp' # variable to apply the calibration procedure
#'calibrate_sim(var = var, path = sim_folder,
#'              nml_file = nml_file, calib_setup = calib_setup,
#'              glmcmd = NULL,
#'              first.attempt = TRUE, period = period, method = 'CMA-ES',
#'              scaling = TRUE, #scaling should be TRUE for CMA-ES
#'              verbose = FALSE,
#'              metric = 'RMSE',plotting = TRUE,
#'              target.fit = 1.5,
#'              target.iter = 50, output = output, field_file = field_file)
#'@import adagio
#'@import GLM3r 
#'@import ggplot2
#'@export
calibrate_sim <- function(var = 'temp',
                          path,
                          field_file,
                          nml_file = 'glm3.nml',
                          calib_setup = NULL,
                          glmcmd = NULL,
                          first.attempt = TRUE,
                          period = NULL,
                          scaling = TRUE,
                          verbose = TRUE,
                          method = 'CMA-ES',
                          metric = 'RMSE',
                          target.fit = 1.5,
                          target.iter = 100,
                          plotting = TRUE,
                          output){
  
  if (first.attempt){
    if (file.exists(paste0(path,'/calib_results_',metric,'_',var,'.csv'))){
      file.remove(paste0(path,'/calib_results_',metric,'_',var,'.csv'))
    }
    if (file.exists(paste0(path,'/calib_par_',var,'.csv'))){
      file.remove(paste0(path,'/calib_par_',var,'.csv'))
    }
  } 
  
  if(!file.exists('glm4.nml')){
    # file.copy(nml_file, 'glm4.nml')
    file.copy(nml_file, paste0(path,'/glm4.nml'))
  } else if (first.attempt){
    file.copy(paste0(path,'/glm4.nml'), nml_file, overwrite = TRUE)
  }
  
  if (is.null(calib_setup)){
    calib_setup <- get_calib_setup()
  }
  pars <<- as.character(calib_setup$pars)
  ub <<- calib_setup$ub
  lb <<- calib_setup$lb
  variable <<- var
  
  
  if (scaling){
    init.val <- (calib_setup$x0 - lb) *10 /(ub-lb) 
  }
  
  if (!is.null(period)){
    nml <- read_nml(nml_file)
    nml <- set_nml(nml, arg_list = period$calibration)
    write_nml(nml,nml_file)
  }
  
  # path <<- path
  obs <<- read_field_obs(field_file)
  calib_GLM(var, ub, lb, init.val, obs, method, glmcmd,
                 metric, target.fit, target.iter, nml_file, path, scaling, verbose)
  
  # loads all iterations
  results <- read.csv(paste0(path,'/calib_results_RMSE_temp.csv'))
  results$DateTime <- as.POSIXct(results$DateTime)
  g1 <- ggplot(results, aes(DateTime, RMSE)) +
    geom_point() +
    geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x)) +
    theme_bw() +
    theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_datetime();
  if (plotting == TRUE){
  ggsave(file=paste0(path,'/optim_',method,'_',var,'.png'), g1, dpi = 300,width = 384,height = 216, units = 'mm')
  }
  
  g1
  
  # compares simulated with observed data
  
  temp_rmse1 <- compare_to_field(output, field_file = field_file, 
                                 metric = 'water.temperature', as_value = FALSE, precision= 'hours')
  if (plotting == TRUE){
    plot_var_compare(nc_file = output, field_file = field_file,var_name = 'temp', precision = 'hours', fig_path = paste0(path,'/calib_',method,'_',var,'_',metric,round(temp_rmse1,2),'.png'))
  } else {
    plot_var_compare(nc_file = output, field_file = field_file,var_name = 'temp', precision = 'hours')
  }
  
  
  # check the model fit during the validation period
  init.temps <- read_nml(nml_file)$init_profiles$the_temps
  get_calib_init_validation(nml_file= nml_file, output = output)
  nml <- read_nml(nml_file)
  nml <- set_nml(nml, arg_list = period$validation)
  write_nml(nml,nml_file)
  
  run_glm(sim_folder = path, verbose = verbose)
  temp_rmse2 <- compare_to_field(output, field_file = field_file, 
                                 metric = 'water.temperature', as_value = FALSE, precision= 'hours')
  if (plotting == TRUE){
  plot_var_compare(nc_file = output, field_file = field_file,var_name = 'temp', precision = 'hours', fig_path = paste0(path,'/valid_',method,'_',var,'_',metric,round(temp_rmse2,2),'.png'))
  } else {
    plot_var_compare(nc_file = output, field_file = field_file,var_name = 'temp', precision = 'hours')
  }
  
  
  # check the model fit during the whole time period
  nml <- read_nml(nml_file)
  total.list <- period$total
  total.list[['the_temps']] <- init.temps
  nml <- set_nml(nml, arg_list =total.list)
  write_nml(nml,nml_file)
  
  run_glm(sim_folder = path)
  temp_rmse3 <- compare_to_field(output, field_file = field_file, 
                                 metric = 'water.temperature', as_value = FALSE, precision= 'hours')
  if (plotting == TRUE){
  plot_var_compare(nc_file = output, field_file = field_file,var_name = 'temp', precision = 'hours', fig_path = paste0(path,'/total_',method,'_',var,'_',metric,round(temp_rmse3,2),'.png'))
  } else {
    plot_var_compare(nc_file = output, field_file = field_file,var_name = 'temp', precision = 'hours')
  }
  
  # print a matrix of our constrained variable space, the initial value and the calibrated value
  calibrated_results <- cbind(calib_setup, 'calibrated' =round(c(results$wind_factor[1], 
                                                                 results$wind_factor[1],
                                                                 results$ch[1],
                                                                 results$sed_temp_mean[1],
                                                                 results$sed_temp_mean.1[1],
                                                                 results$coef_mix_hyp[1],
                                                                 results$Kw[1]),4))
  
  print(paste('calibration:',round(temp_rmse1,2),'deg C RMSE'))
  print(paste('validation:',round(temp_rmse2,2),'deg C RMSE'))
  print(paste('total time period:',round(temp_rmse3,2),'deg C RMSE'))
  return(print(calibrated_results))
  
}

