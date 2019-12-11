#'@title Splits time period into calibration and validation period
#'
#'@description Reads in nml-file and splits the time period in a calibration and a validation time period to avoid over-fitting
#'@param nml String of the GLM namelist file, in most cases this is 'glm3.nml'
#'@param ratio Double of the ratio between calibration and validation, default is 1 which refers to calibration:validation 1:1, 2 would
#' mean that calibration:validation would be 2:1
#'@keywords methods
#'@seealso \code{\link{get_calib_setup}, \link{calibrate_sim}, \link{get_calib_init_validation}}
#'@author
#'Robert Ladwig
#'
#'@examples
#'calib_periods <- get_calib_periods(system.file("extdata/", "glm3.nml", package = "glmtools"), ratio = 1)
#'@export
get_calib_periods <- function(nml, ratio = 1){
  start = as.POSIXct(get_nml_value(glm_nml = read_nml(nml),arg_name = 'start'), format = '%Y-%m-%d %H:%M:%S')
  stop = as.POSIXct(get_nml_value(glm_nml = read_nml(nml),arg_name = 'stop'), format = '%Y-%m-%d %H:%M:%S')
  
  if (!is.na(format(strptime(start,'%Y-%m-%d %H:%M:%S'),'%H:%M:%S')) && format(strptime(start,'%Y-%m-%d %H:%M:%S'),'%H:%M:%S') == format(strptime(stop,'%Y-%m-%d %H:%M:%S'),'%H:%M:%S')){
    hourstamp <- format(strptime(start,'%Y-%m-%d %H:%M:%S'),'%H:%M:%S') 
  } else {
    hourstamp <- format(strptime(as.character("2000-01-01 00:00:00", format = '%Y-%m-%d %H:%M:%S'), '%Y-%m-%d %H:%M:%S'),'%H:%M:%S') 
  }
  
  total <-  list('start' = as.character(start), 'stop' = as.character(stop))
  
  dur <- stop-start
  interval <- dur - dur/(ratio +1) 
  
  start1 <- start
  stop1 <- checkHourFormat(timest = start1 + interval, hourst = hourstamp)
  
  calib <- list('start' = as.character(start1), 'stop' = as.character(stop1))
  
  start2 <- stop1 + 24 * 3600
  stop2 <- stop
  
  valid <- list('start' = as.character(start2), 'stop' = as.character(stop2))
  
  return(list("total" = total,"calibration" = calib,"validation" = valid))
}
