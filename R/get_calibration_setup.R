#'Creates an example setup for a calibration run
#'
#'Creates dataframe containing the parameters for calibration (string) and their respective lower boundaries, upper boundaries 
#'and initial values
#'@keywords methods
#'@seealso \code{\link{calibrate_sim}}}
#'@author
#'Robert Ladwig, Tadhg Moore
#'
#'@examples
#'calib_setup <- get_calibration_setup()
#'
get_calibration_setup <- function(){
  setup <- data.frame('pars' = as.character(c('sw_factor','lw_factor','ch','sed_temp_mean','sed_temp_mean')),
                      'lb' = c(0.7,0.7,5e-4,3,8),
                      'ub' = c(2,2,0.002,8,20),
                      'x0' = c(1,1,0.0013,5,15))
  return(setup)
}