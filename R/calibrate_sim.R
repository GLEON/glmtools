#'@title Calibrates GLM-AED2 variables to improve fit between observed and simulated data
#'
#'@description Starts a calibration run with multiple iterations using a GLM-AED2 setup. At the end a output csv.-file of all iterations and a 
#'diagnostic plot are created.
#'@param var Character vector of valid variable names (see \code{\link{sim_vars}})
#'@param path String with the path to the GLM setup
#'@param obs CSV or TSV field data (see \link{resample_to_field}for format)
#'@param nml.file String of the glm-namelist file, default is 'glm3.nml'
#'@param calib_setup Data frame containing information regarding the calibration (see \link{get_calibration_setup})
#'@param glmcmd String containing the desired glm run command, default is GLM3r
#'@param first.attempt Boolean, if TRUE a nml-template will be created, set it to FALSE after your first calibration run; default is TRUE
#'@param period List that provides a start and a stop date for the simulation
#'@param scaling Boolean, if TRUE variable values will be scaled on the space (0,10), recommended for CMA-ES, default is TRUE
#'@param method String of the optimization method, default is 'CMA-ES' (Hansen 2009), alternatively you can also use 'Nelder-Mead'
#'@param metric String of the calibration fit metric, default is RMSE
#'@param target.fit Double of your preferred fit, calibration will stop after reaching that; default is 1.5
#'@param target.iter Double of maximum amount of iterations, default is 150
#'@keywords methods
#'@seealso \code{\link{get_calib_setup, get_calib_periods, get_calib_init_validation}}
#'@author
#'Robert Ladwig, Tadhg Moore
#'
#'@examples
#'calib_setup <- get_calibration_setup()
#'print(calib_setup)
#'
#'#Essential variables
#'var = 'temp' # variable to apply the calibration procedure
#'path = getwd() # simulation path
#'obs = read_field_obs('bcs/sparkling_lter_temp.csv') # observed field data
#'nml.file = 'glm3.nml' # name of nml-file, if different than the default version the system command needs to be changed as well
#'glmcmd = "glm" # command to be used, default applies GLM3r
#'#Optional variables
#'first.attempt = TRUE # if TRUE, deletes all local csv-files that stores the outcome of previous calibration runs
#'period = list('start' = '2011-01-01 12:00:00', 'stop' = '2011-12-31 12:00:00'), or use get_calib_periods.R to create calibration/validation periods, i.e. period = get_calib_periods(nml = nml.file, ratio = 1)$calibration
#'scaling = TRUE # scaling should be TRUE for CMA-ES
#'method = 'CMA-ES' # Choose the optimization method, either Covariance Matrix Adaption - Evolution Strategy ('CMA-ES') or Nelder-Mead
#'metric = 'RMSE' # Root-mean square error
#'target.fit = 1.5 # refers to a target fit of 1.5 degrees Celsius (algorithm stops after reaching 1.5 degree Celsius, set target.fit = -Inf to run until it reaches target.iter only)
#'target.iter = 150 # refers to a maximum run of 150 calibration iterations (algorithm stops after doing 150 function evaluations)
#'
#'calibrate_sim(var, path, obs, nml.file, calib_setup, glmcmd, first.attempt, period, scaling, method, metric, target.fit, target.iter)
#'
#'@import adagio
#'@import GLM3r 
#'@importFrom hydroGOF NSE 
#'@import ggplot2
#'@export
calibrate_sim <- function(var = 'temp',
                          path,
                          obs,
                          nml.file = 'glm3.nml',
                          calib_setup = NULL,
                          glmcmd = NULL,
                          first.attempt = TRUE,
                          period = NULL,
                          scaling = 'TRUE',
                          method = 'CMA-ES',
                          metric = 'RMSE',
                          target.fit = 1.5,
                          target.iter = 100,
                          plotting = TRUE,
                          output){
  
  if (first.attempt){
    if (file.exists(paste0('calib_results_',metric,'_',var,'.csv'))){
      file.remove(paste0('calib_results_',metric,'_',var,'.csv'))
    }
    if (file.exists(paste0('calib_par_',var,'.csv'))){
      file.remove(paste0('calib_par_',var,'.csv'))
    }
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
    nml <- read_nml(nml.file)
    nml <- set_nml(nml, arg_list = period$calibration)
    write_nml(nml,nml.file)
  }
  
  path <<- path
  
  calib_GLM(var, ub, lb, init.val, obs, method, glmcmd,
                 metric, target.fit, target.iter, nml.file,path)
  
  # loads all iterations
  results <- read.csv('calib_results_RMSE_temp.csv')
  results$DateTime <- as.POSIXct(results$DateTime)
  g1 <- ggplot(results, aes(DateTime, RMSE)) +
    geom_point() +
    geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x)) +
    theme_bw() +
    theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_datetime()
  ggsave(file=paste0('optim_',method,'_',var,'.png'), g1, dpi = 300,width = 384,height = 216, units = 'mm')
  
  # compares simulated with observed data
  temp_rmse1 <- compare_to_field(out_file, field_file = 'bcs/ME_observed.csv', 
                                 metric = 'water.temperature', as_value = FALSE, precision= 'hours')
  plot_var_compare(nc_file = out_file, field_file = 'bcs/ME_observed.csv',var_name = 'temp', precision = 'hours', fig_path = paste0('calib_',method,'_',var,'_',metric,round(temp_rmse1,2),'.png'))
  
  
  
  # check the model fit during the validation period
  init.temps <- read_nml(nml.file)$init_profiles$the_temps
  get_calib_init_validation(nml_file= nml.file, output = out_file)
  nml <- read_nml(nml.file)
  nml <- set_nml(nml, arg_list = period$validation)
  write_nml(nml,nml.file)
  
  run_glm()
  temp_rmse2 <- compare_to_field(out_file, field_file = 'bcs/ME_observed.csv', 
                                 metric = 'water.temperature', as_value = FALSE, precision= 'hours')
  plot_var_compare(nc_file = out_file, field_file = 'bcs/ME_observed.csv',var_name = 'temp', precision = 'hours', fig_path = paste0('valid_',method,'_',var,'_',metric,round(temp_rmse2,2),'.png'))
  
  
  
  # check the model fit during the whole time period
  nml <- read_nml(nml.file)
  total.list <- period$total
  total.list[['the_temps']] <- init.temps
  nml <- set_nml(nml, arg_list =total.list)
  write_nml(nml,nml.file)
  
  run_glm()
  temp_rmse3 <- compare_to_field(out_file, field_file = 'bcs/ME_observed.csv', 
                                 metric = 'water.temperature', as_value = FALSE, precision= 'hours')
  plot_var_compare(nc_file = out_file, field_file = 'bcs/ME_observed.csv',var_name = 'temp', precision = 'hours', fig_path = paste0('total_',method,'_',var,'_',metric,round(temp_rmse3,2),'.png'))
  
  
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

