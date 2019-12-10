#' @importFrom tidyr gather
#' @import ggplot2
#' @import dplyr
#' @importFrom akima interp
#' @importFrom akima interp2xyz

.interpolate2grid <- function(xyzData, xcol = 1, ycol = 2, zcol = 3) {
  # Interpolate field or modeled data to grid 
  # xcol, ycol, and zcol and column numbers from data.frame
  # The spreads of x and y must be within four orders of magnitude of each other for interp to work
  # Therefore must scale data to be within similar magnitude to numeric dates (1e6)
  gridData <-interp2xyz(interp(x = as.numeric(xyzData[,xcol]), y=xyzData[,ycol]*1e6, z=xyzData[,zcol], duplicate="mean", linear = T,
                               xo = as.numeric(seq(min(xyzData[,xcol]), max(xyzData[,xcol]), by = 'day')),
                               yo = 1e6*seq(min(xyzData[,ycol]), max(xyzData[,ycol]), by = 1)), data.frame=TRUE) %>%
    dplyr::mutate(x =  as.POSIXct(x, origin = '1970-01-01', tz = Sys.timezone())) %>%
    dplyr::mutate(y = y/1e6) %>%
    dplyr::arrange(x,y)
  
  return(gridData)
}


.plot_nc_heatmap <- function(file, var_name, reference, legend.title , interval,
                             text.size, show.legend, legend.position, plot.title,
                             color.palette, color.direction, zlim) {
  
  surface <- get_surface_height(file)
  max_depth <- max(surface[, 2])
  min_depth <- 0
  z_out <- seq(min_depth, max_depth,by = interval) # Set plotting interval
  # Get data from .nc file
  data = get_var(file, var_name = var_name, z_out = z_out, reference = reference)
  # Get units
  units = sim_var_units(file, var_name = var_name)
  
  if (reference == 'surface'){
    names.df = data.frame(names = names(data)[-1], depth.numeric = z_out, stringsAsFactors = F)
    # ylabel = 'Depth (m)'
  }
  if (reference == 'bottom'){
    names.df = data.frame(names = names(data)[-1], depth.numeric = rev(z_out), stringsAsFactors = F)
    # ylabel = 'Elevation (m)'
  }
  dataLong = gather(data = data,key = depth, value = var, -DateTime) %>%
    left_join(names.df, by = c('depth' = 'names')) 
  names(dataLong)[3] = var_name
  
  if(is.null(legend.title)) {
    legend.title = .unit_label(file, var_name)
  }
  .plot_df_heatmap(dataLong, var_name, legend.title, text.size, show.legend, legend.position, plot.title,
                   color.palette, color.direction, zlim)
}


.plot_df_heatmap <- function(dataLong, var_name, legend.title, text.size, 
                             show.legend, legend.position, plot.title,
                             color.palette, color.direction, zlim) {
  
  h1 = ggplot(data = dataLong, aes(DateTime, depth.numeric)) +
    geom_raster(aes_string(fill = var_name), interpolate = F, hjust = 0.5, vjust = 0.5, show.legend = show.legend) +
    scale_y_reverse(expand = c(0.01,0.01)) +
    scale_x_datetime(expand = c(0.01,0.01)) +
    scale_fill_distiller(limits = zlim, palette = color.palette, direction = color.direction, na.value = "grey90") +
    # scale_fill_viridis_c(alpha = 0.95, option = 'plasma') +
    ylab('Depth (m)') + xlab('Date') +
    labs(fill = legend.title, title = plot.title) +
    theme_bw(base_size = text.size) +
    theme(legend.position = legend.position)
  
  return(h1)
}

.plot_nc_timeseries <- function(file, var_name, text.size, plot.title){
  
  # Get data from .nc file
  data = get_var(file, var_name = var_name) %>% 
    mutate(var_name = as.numeric(get(var_name)))
  
  ylab.title = .unit_label(file, var_name)
  
  h1 = ggplot(data = data, aes(x = DateTime, y = var_name)) + geom_point(alpha = 0.8) +
    xlab('Date') + ylab(ylab.title) +
    scale_y_continuous(expand = c(0.01,0.01)) +
    scale_x_datetime(expand = c(0.01,0.01)) +
    labs(title = plot.title) +
    theme_bw(base_size = text.size)
}

.unit_label <- function(file, var_name){
  longname <- sim_var_longname(file, var_name) 
  titlename <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", longname, perl=TRUE)
  units <- sim_var_units(file, var_name)
  unit_label <- paste0(titlename, " (", units, ")")
  return(unit_label)
}

