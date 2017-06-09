
.plot_nc_heatmap <- function(file, var_name, reference, num_cells=100, palette, ...){
  
  surface <- get_surface_height(file)
  max_depth <- max(surface[, 2])
  min_depth <- 0
  z_out <- seq(min_depth, max_depth,length.out = num_cells)
  data <- get_var(file, z_out = z_out, var_name = var_name, reference = reference)
  title = .unit_label(file, var_name)
  .plot_df_heatmap(data, title, num_cells, palette, ...)
}

#' @importFrom graphics .filled.contour
#' @importFrom grDevices colorRampPalette 
#' @importFrom utils head
.plot_df_heatmap <- function(data, bar_title, num_cells, palette, title_prefix=NULL, overlays=NULL, xaxis=NULL, col_lim){
  
  z_out <- rLakeAnalyzer::get.offsets(data)
  reference = ifelse(substr(names(data)[2],1,3) == 'elv', 'bottom', 'surface')
  
  if (missing(col_lim))
    col_lim = range(data[, -1], na.rm = TRUE)
  if (missing(palette))
    palette <- colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red"), 
                                bias = 1, space = "rgb")
  
  col_subs <- head(pretty(col_lim, 6), -1)
  levels <- sort(unique(c(col_subs, pretty(col_lim, 15))))
  colors <- palette(n = length(levels)-1)
  dates <- data[, 1]
  matrix_var <- data.matrix(data[, -1])
  if(is.null(xaxis)){
  	xaxis <- get_xaxis(dates)
  }
  
  yaxis <- get_yaxis_2D(z_out, reference, prefix=title_prefix)
  plot_layout(xaxis, yaxis, add=TRUE)
  .filled.contour(x = dates, y = z_out, z =matrix_var,
                  levels= levels,
                  col=colors)
  overlays # will plot any overlay functions
  axis_layout(xaxis, yaxis) #doing this after heatmap so the axis are on top
  
  color_key(levels, colors, subs=col_subs, col_label = bar_title)
}

#' @importFrom graphics points
.plot_nc_timeseries <- function(file, var_name){
  
  ylab = .unit_label(file, var_name)
  variable_df <- get_var(file, var_name=var_name)
  xaxis <- get_xaxis(variable_df[,1])
  yaxis <- get_yaxis(variable_df[,2], title = ylab)
  plot_layout(xaxis, yaxis, add=TRUE)
  points(variable_df)
  axis_layout(xaxis, yaxis)
}