#'@title plot meterological drivers from a csv file
#'@param nml_file a string with the path to the nml file for the simulation 
#'@param fig_path F if plot to screen, string path if save plot as .png
#'@keywords methods
#'@seealso \link{read_nml}
#'@author
#'Jordan S. Read, Luke A. Winslow
#'@examples 
#'nml_file <- system.file('extdata', 'glm.nml', package = 'glmtools')
#'
#'plot_meteo(nml_file)
#'@import ncdf4
#'@export
plot_meteo <- function(nml_file, fig_path = F){
  glm_nml <- read_nml(nml_file)
  met_file <- get_nml_value(glm_nml,'meteo_fl') 
  met_path <- file.path(dirname(nml_file),met_file)
  
  if (!file.exists(met_path)){stop(paste0("nml_file points to a meteo file that doesn't exist:\n",met_path))}
  
  
}