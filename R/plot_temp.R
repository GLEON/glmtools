#'@title plot water temperatures from a GLM simulation
#'@param file a string with the path to the netcdf output from GLM
#'@param reference a string for 'surface' or 'bottom'
#'@param num_cells number of vertical cells to use for heatmap
#'@param fig_path F if plot to screen, string path if save plot as .png
#'@param add F if create new figure, T if add to existing
#'@keywords methods
#'@seealso \code{\link{get_temp}}
#'@author
#'Jordan S. Read, Luke A. Winslow
#'@examples 
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'plot_temp(file = nc_file, fig_path = FALSE)
#'\dontrun{
#'plot_temp(file = nc_file, fig_path = '../test_figure.png')
#'}
#'@export
plot_temp <- function(file, reference = 'surface', num_cells = 100, fig_path = F, add = F){

  plot_var(file, var_name = 'temp', col_lim = c(0,36), 
           reference, num_cells, fig_path, add, bar_title = 'Temperature (\u00B0C)')
  
}
