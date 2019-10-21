#'@title Creates an example setup for a calibration run
#'
#'@description Creates dataframe containing the parameters for calibration (string) and their respective lower boundaries, upper boundaries 
#'and initial values
#'@keywords methods
#'@seealso \code{\link{calibrate_sim}}}
#'@author
#'Robert Ladwig, Tadhg Moore
#'
#'@examples
#'calib_setup <- get_calibration_setup()
#'@export
get_calib_setup <- function(){
  setup <- data.frame('pars' = as.character(c('wind_factor','lw_factor','Kw','sed_temp_mean','sed_temp_mean')),
                      'lb' = c(0.7,0.7,0.1,3.,8.),
                      'ub' = c(2.,2.,0.8,8.,20.),
                      'x0' = c(1.,1.,0.5,5.,15.))
  return(setup)
}