#'@title plot variable from a GLM simulation
#'@param file a string with the path to the netcdf output from GLM
#'@param var_name a string for a valid variable name (see \code{\link{sim_vars}})
#'@param col_lim a numeric array of length 2 that specifies the min and max value for color bar
#'@param reference a string for 'surface' or 'bottom'
#'@param num_cells number of vertical cells to use for heatmap
#'@param fig_path F if plot to screen, string path if save plot as .png
#'@param add F if create new figure, T if add to existing
#'@param bar_title NULL if use \code{\link{sim_var_longname}}, or specify as a string to name plot variable
#'@keywords methods
#'@seealso \code{\link{get_temp}}, \code{\link{sim_var_longname}}, \code{\link{sim_vars}}
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
#'col_lim=c(0,400),
#'bar_title = 'Dissolved Oxygen', 
#'fig_path = 'aed_out.png')
#'}
#'@export
plot_var <- function(file, var_name, col_lim, reference = 'surface', num_cells = 100, 
										 fig_path = F, add = F){
  
  heatmaps <- .is_heatmap(file, var_name)
  num_divs <- length(var_name)
  
  is_multiplot = ifelse(num_divs > 1, TRUE, FALSE)
  is_heatmap = any(heatmaps)
  
  # -- set up plot layout
  def.par <- par(no.readonly = TRUE)
  .stacked_layout(is_heatmap, num_divs)
  
  if (is.character(fig_path)){
    gen_default_fig(file_name = fig_path) 
  }
  
  # iterate through plots
  for (j in 1:num_divs){
    if (heatmaps[j]){
      .plot_heatmap(file, var_name[j], col_lim, reference, num_cells)
    } else {
      .plot_timeseries(file, var_name[j])
      if(is_heatmap) .plot_null() # to fill up the colormap div
    }
  }

  if (is.character(fig_path)){
    dev.off()
  } 
  
  if (!add) par(def.par)
  
}

.plot_heatmap <- function(file, var_name, col_lim, reference, num_cells, bar_title){
  
  surface <- get_surface_height(file)
  max_depth <- max(surface[, 2])
  min_depth <- 0
  z_out <- seq(min_depth, max_depth,length.out = num_cells)
  variable_df <- get_var(file, reference = reference, z_out, var_name=var_name)
  
  palette <- colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red"), 
                              bias = 1, space = "rgb")
  
  if (missing(col_lim)) col_lim <- range(variable_df[, -1], na.rm = TRUE)
  bar_title <- sim_var_longname(file, var_name) 
  
  levels <- seq(col_lim[1], col_lim[2], by = diff(col_lim)/15)
  col_subs <- levels
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
  longname <- sim_var_longname(file, var_name) 
  units <- sim_var_units(file, var_name)
  ylab <- paste0(longname, " (", units, ")")
  variable_df <- get_var(file, var_name=var_name)
  plot(variable_df, ylab = ylab)
}
