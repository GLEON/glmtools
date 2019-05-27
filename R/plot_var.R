#'Plot variables from a GLM simulation
#'@param file a string with the path to the netcdf output from GLM
#'@param var_name a character vector of valid variable names (see \code{\link{sim_vars}})
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
#'@seealso \code{\link{get_temp}}, \code{\link{sim_var_longname}}, 
#'\code{\link{sim_vars}}, \code{\link{plot_temp}},  \code{\link{get_var}}
#'@note
#'\code{plot_var} uses the \code{\link[graphics]{layout}} function and so is restricted to a full page display.
#'When creating a heatmap, the output produced by \code{plot_var} is actually a combination of two plots; 
#'one is a \code{\link[graphics]{.filled.contour}} plot and the other is a legend.
#'@author
#'Jordan S. Read, Luke A. Winslow, Hilary A. Dugan
#'
#'@examples
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'vars <- sim_vars(file = nc_file)
#'plot_var(nc_file, 'u_mean')
#'#Plotting two variables
#'plot_var(nc_file, var_name = c('temp','wind'), show.legend = F, text.size = 14, plot.title = c('My Lake: Temp','My Lake: Wind'))
#'#Saving plot
#'plot_var(file,var_name = c('temp', 'OXY_oxy'),fig_path = '~/figtest.png', width = 6, height = 2, units = 'in')
#'
#'\dontrun{
#'# need to specify a valid .nc file here: 
#'plot_var(file = fabm_sim_nc.nc,
#'var_name = 'aed_oxygen_oxy', 
#'fig_path = 'aed_out.png')
#'}
#'@importFrom grDevices dev.off
#'@importFrom gridExtra grid.arrange
#'@export
plot_var <- function(file='output.nc', var_name, fig_path = FALSE, reference='surface', legend.title = NULL, 
                     interval = 0.5, text.size = 12, show.legend = TRUE, 
                     legend.position = 'right', plot.title = NULL,...) {
  
  heatmaps <- .is_heatmap(file, var_name)
  num_divs <- length(var_name)
  
  # is_multiplot = ifelse(num_divs > 1, TRUE, FALSE)
  is_heatmap = any(heatmaps)

  # iterate through plots
  h = list() #for ggplots
  for (j in 1:num_divs){
    if (heatmaps[j]){
        h[[j]] = .plot_nc_heatmap(file = file, var_name = var_name[j], reference = reference,
                                  legend.title = legend.title[j], interval=interval, text.size = text.size, 
                                  show.legend = show.legend, legend.position = legend.position, 
                                  plot.title = plot.title[j])
    } else {
      h[[j]] = .plot_nc_timeseries(file = file, var_name = var_name[j], 
                                   plot.title = plot.title[j], text.size = text.size)
                                   
      if(is_heatmap) .plot_null() # to fill up the colormap div
    }
  }
  grid.arrange(grobs = h, ncol = 1)
  
  # Saving plot 
  if (is.character(fig_path)){
    ggsave(filename = fig_path,...)
  } 
}

