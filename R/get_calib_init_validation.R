#'@title Get last values from a calibration period to be used for the validation
#'
#'@description Reads in nml-file and output from calibration to get initial values for the calibration
#'@param nml.file String of the GLM namelist file, in most cases this is 'glm3.nml'
#'@param output String of the file path in which the output.nc file is stored (created by calibration period)
#'@keywords methods
#'@seealso \code{\link{calibrate_sim, get_calib_periods}}
#'@author
#'Robert Ladwig
#'
#'@examples
#'initvalues <- get_calib_init_validation(nml = 'glm3.nml', output = 'output.nc')
#'@export
get_calib_init_validation <- function(nml_file, output){
  nml <- read_nml(nml_file)
  temps <- get_var(file = output, reference = 'surface', var_name = 'temp', z_out = nml$init_profiles$the_depths, t_out = nml$time$stop)
  nml <- set_nml(nml, arg_list = list('the_temps' = as.double(temps[-1])))
  write_nml(nml,nml_file)
  return()
}
