#'Calibrates GLM-AED2 variables to improve fit between observed and simulated data
#'
#'Starts a calibration run with multiple iterations using a GLM-AED2 setup. At the end a output csv.-file of all iterations and a 
#'diagnostic plot are created.
#'@param var Character vector of valid variable names (see \code{\link{sim_vars}})
#'@param path String with the path to the GLM setup
#'@param obs CSV or TSV field data (see \link{resample_to_field}for format)
#'@param nml.file String of the glm-namelist file, default is 'glm3.nml'
#'@param calib_setup Data frame containing information regarding the calibration (see \link{get_calibration_setup})
#'@param glmcmd String containing the desired glm run command, default is GLM3r
#'@param first.attempt Boolean, if TRUE a nml-template will be created, set it to FALSE after your first calibration run; default is TRUE
#'@param scaling Boolean, if TRUE variable values will be scaled on the space (0,10), recommended for CMA-ES, default is TRUE
#'@param method String of the optimization method, default is CMA-ES (Hansen 2009)
#'@param metric String of the calibration fit metric, default is RMSE
#'@param target.fit Double of your preferred fit, calibration will stop after reaching that; default is 1.5
#'@param target.iter Double of maximum amount of iterations, default is 150
#'@keywords methods
#'@seealso \code{\link{get_calibration_setup}}}
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
#'first.attempt = FALSE # if TRUE, copy glm3 and create glm4. declare F for subsequent runs
#'scaling = TRUE # scaling should be TRUE for CMA-ES
#'method = 'CMA-ES' # Covariance Matrix Adaption - Evolution Strategy
#'metric = 'RMSE' # Root-mean square error
#'target.fit = 1.5 # refers to a target fit of 1.5 degrees Celsius
#'target.iter = 150 # refers to a maximum run of 150 calibration iterations
#'
#'calibrate_sim(var, path, obs, nml.file, calib_setup, glmcmd, first.attempt, scaling, method, metric, target.fit, target.iter)
#'
#'@import adagio
#'@import GLM3r
#'@import hydroGOF 
#'@import ggplot2
#'@export
calibrate_sim <- function(var = 'temp',
                          path,
                          obs,
                          nml.file = 'glm3.nml',
                          calib_setup = NULL,
                          glmcmd = NULL,
                          first.attempt = TRUE,
                          scaling = 'TRUE',
                          method = 'CMA-ES',
                          metric = 'RMSE',
                          target.fit = 1.5,
                          target.iter = 100){
  
  if (first.attempt){
    file.copy('glm3.nml', 'glm4.nml', overwrite = TRUE)
    file.copy('aed2/aed2.nml', 'aed2/aed4.nml', overwrite = TRUE)
  } 
  file.copy('glm4.nml', 'glm3.nml', overwrite = TRUE)
  file.copy('aed2/aed4.nml', 'aed2/aed2.nml', overwrite = TRUE)
  
  
  if (is.null(calib_setup)){
    calib_setup <- get_calibration_setup()
  }
  pars <<- as.character(calib_setup$par)
  ub <<- calib_setup$ub
  lb <<- calib_setup$lb
  variable <<- var
  
  
  if (scaling){
    init.val <- (calib_setup$x0 - lb) *10 /(ub-lb) 
  }
  
  calib_GLM(var, ub, lb, init.val, obs, method, glmcmd,
                 metric, target.fit, target.iter, nml.file,path)
}

