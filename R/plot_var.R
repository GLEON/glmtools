#'@title plot variables from a GLM simulation
#'@param file a string with the path to the netcdf output from GLM
#'@param var_name a character vector of valid variable names (see \code{\link{sim_vars}})
#'@param fig_path F if plot to screen, string path if save plot as .png
#'@param reference 'surface' or 'bottom'. Only used for heatmap plots.
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
#'@export
plot_var <- function(file, var_name, fig_path = F, reference='surface', ...){
  
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
      .plot_heatmap(file, var_name[j], reference)
    } else {
      .plot_timeseries(file, var_name[j])
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

.plot_heatmap <- function(file, var_name, reference, num_cells=100){
  
  surface <- get_surface_height(file)
  max_depth <- max(surface[, 2])
  min_depth <- 0
  z_out <- seq(min_depth, max_depth,length.out = num_cells)
  variable_df <- get_var(file, z_out = z_out, var_name = var_name, reference = reference)
  
  palette <- colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red"), 
                              bias = 1, space = "rgb")
  
  col_lim <- range(variable_df[, -1], na.rm = TRUE)
  
  bar_title <- .unit_label(file, var_name)
  
  
  col_subs <- head(pretty(col_lim, 6), -1)
  levels <- sort(unique(c(col_subs, pretty(col_lim, 15))))
  colors <- palette(n = length(levels)-1)
  dates <- variable_df[, 1]
  matrix_var <- data.matrix(variable_df[, -1])
  xaxis <- get_xaxis(dates)
  yaxis <- get_yaxis_2D(z_out, reference)
  plot_layout(xaxis, yaxis, add=T)
  .filled.contour(x = dates, y = z_out, z =matrix_var,
                  levels= levels,
                  col=colors)
  
  axis_layout(xaxis, yaxis) #doing this after heatmap so the axis are on top
  
  color_key(levels, colors, subs=col_subs, col_label = bar_title)
  
}

.plot_timeseries <- function(file, var_name){
  
  ylab = .unit_label(file, var_name)
  variable_df <- get_var(file, var_name=var_name)
  xaxis <- get_xaxis(variable_df[,1])
  yaxis <- get_yaxis(variable_df[,2], title = ylab)
  plot_layout(xaxis, yaxis, add=T)
  points(variable_df)
  axis_layout(xaxis, yaxis)
}
