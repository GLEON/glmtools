#'@title plot variable from a GLM simulation
#'@param file a string with the path to the netcdf output from GLM
#'@param var_name a string for a valid variable name
#'@param col_lim a numeric array of length 2 that specifies the min and max value for color bar
#'@param reference a string for 'surface' or 'bottom'
#'@param num_cells number of vertical cells to use for heatmap
#'@param fig_path F if plot to screen, string path if save plot as .png
#'@param add F if create new figure, T if add to existing
#'@param bar_title NULL if use var_name, or specify as a string to name plot variable
#'@keywords methods
#'@seealso \link{get_temp}
#'@author
#'Jordan S. Read, Luke A. Winslow
#'@examples
#'\dontrun{
#'plot_var(file = fabm_sim_nc,
#'var_name = 'aed_oxygen_oxy', 
#'col_lim=c(0,400),
#'bar_title = 'Dissolved Oxygen', 
#'fig_path = 'aed_out.png')
#'}
#'@export
plot_var <- function(file, var_name, col_lim, reference = 'surface', num_cells = 100, 
										 fig_path = F, add = F, bar_title = NULL){
  
  surface <- get_surface_height(file)
  max_depth <- max(surface[, 2])
  min_depth <- 0
  z_out <- seq(min_depth, max_depth,length.out = num_cells)
  variable_df <- get_var(file, reference = reference, z_out, var_name=var_name)
  if (ncol(variable_df) == 2){
    stop('plot_var() not yet supported for 1D output variables')
  }
  palette <- colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red"), 
                              bias = 1, space = "rgb")
  levels <- seq(col_lim[1], col_lim[2], by = diff(col_lim)/15)
  #col_subs <- unique(floor(seq(col_lim[1], col_lim[2]-1, length.out = 15)))
  col_subs <- levels
  colors <- palette(n = length(levels)-1)
  dates <- variable_df[, 1]
  matrix_var <- data.matrix(variable_df[, -1])
  xaxis <- get_xaxis(dates)
  yaxis <- get_yaxis_2D(z_out, reference)
  
  if (is.character(fig_path)){
    gen_default_fig(file_name = fig_path) 
  }
  
  
  plot_layout(xaxis, yaxis, add)
  .filled.contour(x = dates, y = z_out, z =matrix_var,
                  levels= levels,
                  col=colors)
  
  axis_layout(xaxis, yaxis) #doing this after heatmap so the axis are on top
  
  if (is.null(bar_title)){
    bar_title = var_name
  }
  color_key(levels, colors, subs=col_subs, col_label = bar_title)
  if (is.character(fig_path)){
    dev.off()
  } else {
    #layout(as.matrix(1))
  }
}

