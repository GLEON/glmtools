#'Plot water temperatures from a GLM simulation
#'@param file a string with the path to the netcdf output from GLM
#'@param reference a string for 'surface' or 'bottom'
#'@param fig_path F if plot to screen, string path if save plot as .png
#' @param col_lim range for heatmap (in units of the variable)
#'@param ... additional arguments passed to \code{par()}
#'@keywords methods
#'@seealso \code{\link{get_temp}}, \code{\link{plot_var}}
#'@note
#'\code{plot_temp} calls \code{\link{plot_var}} specifically for the \code{var_name = 'temp'}. 
#'\code{\link{plot_var}} uses the \code{\link[graphics]{layout}} function and so is restricted to a full page display.
#'@author
#'Jordan S. Read, Luke A. Winslow
#'@examples 
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'plot_temp(file = nc_file, fig_path = FALSE)
#'plot_temp(file = nc_file, fig_path = 'test_figure.png', height = 3, reference = 'surface')
#'@export
plot_temp <- function(file='output.nc', fig_path = FALSE, reference = 'surface', col_lim, ...){

  plot_var(file, var_name = 'temp', fig_path, reference, col_lim, ...)
  
}
