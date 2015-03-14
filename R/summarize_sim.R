#'@title creates GLM simulation summary outputs
#'
#'@param file a string with the path to the netcdf output from GLM
#'@param sim_outputs a character vector of outputs. 
#'@param fig_path FALSE if plot to screen, string path if save plot as .png
#'See \code{get_}, timeseries (as in temp, evaporation, etc)
#'@keywords methods
#'@author
#'Jordan S. Read, Luke A. Winslow
#'@import rLakeAnalyzer
#'@seealso \link{get_temp}, \link{get_surface_height}, \link{get_evaporation}
#'@examples
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'summarize_sim(nc_file)
#'
#'summarize_sim(nc_file, sim_outputs = c('temp','surface_height','wind'))
#'@export
summarize_sim <- function(...){
   
  warning('function is deprecated. Use plot_var.')
  return(NULL)
}
