#'Plot variables from a GLM simulation
#'@param file a string with the path to the netcdf output from GLM
#'@param var_name a character vector of valid variable names (see \code{\link{sim_vars}})
#'@param fig_path F if plot to screen, string path if save plot as .png
#'@param reference 'surface' or 'bottom'. Only used for heatmap plots.
#' @param col_lim range for heatmap (in units of the variable)
#'@param ... additional arguments passed to \code{par()}
#'@keywords methods
#'@seealso \code{\link{get_temp}}, \code{\link{sim_var_longname}}, 
#'\code{\link{sim_vars}}, \code{\link{plot_temp}},  \code{\link{get_var}}
#'@note
#'\code{plot_var} uses the \code{\link[graphics]{layout}} function and so is restricted to a full page display.
#'When creating a heatmap, the output produced by \code{plot_var} is actually a combination of two plots; 
#'one is a \code{\link[graphics]{.filled.contour}} plot and the other is a legend.
#'@author
#'Jordan S. Read, Luke A. Winslow
#'@examples
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'vars <- sim_vars(file = nc_file)
#'plot_var(nc_file, 'u_mean')
#'\dontrun{
#'# need to specify a valid .nc file here: 
#'plot_var(file = fabm_sim_nc.nc,
#'var_name = 'aed_oxygen_oxy', 
#'fig_path = 'aed_out.png')
#'}
#'@importFrom grDevices dev.off
#'@export
plot_var <- function(file='output.nc', var_name, fig_path = FALSE, reference='surface', col_lim, ...){
  
  heatmaps <- .is_heatmap(file, var_name)
  num_divs <- length(var_name)
  
  is_multiplot = ifelse(num_divs > 1, TRUE, FALSE)
  is_heatmap = any(heatmaps)
  
  def.par <- par(no.readonly = TRUE)
  gen_default_fig(filename = fig_path, num_divs=num_divs, ...)
  
  # -- set up plot layout
  .stacked_layout(is_heatmap, num_divs)
  
  # iterate through plots
  for (j in 1:num_divs){
    if (heatmaps[j]){
      .plot_nc_heatmap(file, var_name[j], reference, col_lim=col_lim)
    } else {
      .plot_nc_timeseries(file, var_name[j])
      if(is_heatmap) .plot_null() # to fill up the colormap div
    }
  }

  if (is.character(fig_path)){
    fg <- dev.off()
  } else {
    # -- reset plot config to defaults
    par(def.par)
    layout(1)
  }
}
