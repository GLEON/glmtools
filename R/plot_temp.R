#'Deprecated. Plot water temperatures from a GLM simulation
#'@param nc_file a string with the path to the netcdf output from GLM
#'@param fig_path Default is NULL (only plots to screen). Enter string path to save as output file. File type can be anything supported by \code{\link[ggplot2:ggsave]{ggplot2:ggsave}}. See examples. 
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
#'nc_file <- system.file("extdata", "output.nc", package = "glmtools")
#'plot_temp(nc_file = nc_file, fig_path = NULL)
#'@export
plot_temp <- function(nc_file='output.nc',fig_path = NULL, reference='surface', legend.title = NULL, 
                      interval = 0.5, text.size = 12, show.legend = TRUE, 
                      legend.position = 'right', plot.title = NULL){
  
  .Deprecated('plot_var',msg = 'Deprecated. Use `plot_var`, where default is var_name = `temp`')
  
  plot_var_nc(nc_file = nc_file, var_name = 'temp', fig_path = fig_path, reference = reference, legend.title = legend.title, 
           interval = interval, text.size = text.size, show.legend = show.legend, 
           legend.position = legend.position, plot.title = plot.title)
  
}
