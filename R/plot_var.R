#'@title plot water temperatures from a GLM simulation
#'@param file a string with the path to the netcdf output from GLM
#'@param var_name a string for a valid variable name
#'@param col_lim a numeric array of length 2 that specifies the min and max value for color bar
#'@param reference a string for 'surface' or 'bottom'
#'@param num_cells number of vertical cells to use for heatmap
#'@param fig_path F if plot to screen, string path if save plot as .png
#'@param add F if create new figure, T if add to existing
#'@keywords methods
#'@seealso \link{get_temp}
#'@author
#'Jordan S. Read, Luke A. Winslow
#'@export
plot_var <- function(file, var_name, col_lim, reference = 'surface', num_cells = 100, fig_path = F, add = F){
  
  surface <- get_surface_height(file)
  max_depth <- max(surface[, 2])
  min_depth <- 0
  z_out <- seq(min_depth, max_depth,length.out = num_cells)
  temp <- get_var(file, reference = reference, z_out, var_name=var_name)
  palette <- colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red"), 
                              bias = 1, space = "rgb")
  levels <- seq(col_lim[1], col_lim[2], by = 1)
  col_subs <- unique(floor(seq(col_lim[1], col_lim[2], length.out = 15)))
  colors <- palette(n = length(levels)-1)
  dates <- temp[, 1]
  wtr_temps <- data.matrix(temp[, -1])
  xaxis <- get_xaxis(dates)
  yaxis <- get_yaxis_2D(z_out, reference)
  
  if (is.character(fig_path)){
    gen_default_fig(file_name = fig_path) 
  }
  
  
  plot_layout(xaxis, yaxis, add)
  .filled.contour(x = dates, y = z_out, z = wtr_temps,
                  levels= levels,
                  col=colors)
  
  axis_layout(xaxis, yaxis) #doing this after heatmap so the axis are on top
  color_key(levels, colors, subs=col_subs, col_label = var_name)
  if (is.character(fig_path)){
    dev.off()
  } else {
    layout(as.matrix(1))
  }
}

colbar_layout <- function(nrow = 1){
  # ensures all colorbar plots use same x scaling for divs
  mx <- matrix(c(rep(1,5),2),nrow=1)
  panels <- mx
  if (nrow > 2){
    for (i in 2:nrow){
      panels <- rbind(panels,mx+(i-1)*2)
    }
  }
  
  layout(panels)
  
}

plot_layout <- function(xaxis, yaxis, add, data = NA){
  
  if (!add){
    panels <- colbar_layout()
  }
  
  
  plot(data, xlim = xaxis$lim,
       ylim=yaxis$lim,
       xlab=xaxis$x_lab, ylab=' ',
       frame=FALSE,axes=F,xaxs="i",yaxs="i")
  
  
}

