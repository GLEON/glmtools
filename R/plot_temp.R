#'Plot water temperatures from a GLM simulation
#'@param nc_file a string with the path to the netcdf output from GLM
#'@param fig_path Logical; F if plot to screen, string path if save plot as .png
#'@param reference String; 'surface' or 'bottom. Only used for heatmap plots.
#'@param legend.title Vector string; Default (`NULL`) will use variable and units from netcdf file
#'@param interval Positive number indicating the depth interval in meters to interpolate output data. Must be less than max depth of lake. Default = 0.5 m. 
#'@param text.size Integer; Default is 12. Higher values will increase text size in plot.
#'@param show.legend Logical; TRUE to show legend (default), FALSE to hide legend
#'@param legend.position String; Legend position. Default is 'right'. Options: 'left','right','top','bottom'
#'@param plot.title Vector string; Default is no title. 
#'@param ... additional arguments passed to \code{ggsave()}
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
#'plot_temp(nc_file = nc_file, fig_path = FALSE)
#'plot_temp(nc_file = nc_file, fig_path = 'test_figure.png', height = 3, reference = 'surface')
#'@export
plot_temp <- function(nc_file='output.nc',fig_path = FALSE, reference='surface', legend.title = NULL, 
                      interval = 0.5, text.size = 12, show.legend = TRUE, 
                      legend.position = 'right', plot.title = NULL){
  
  .Deprecated('plot_var',msg = 'Deprecated. Use `plot_var`, where default is var_name = `temp`')
  
  plot_var(nc_file, var_name = 'temp', fig_path = FALSE, reference, legend.title, 
           interval, text.size, show.legend, 
           legend.position, plot.title)
  
}
