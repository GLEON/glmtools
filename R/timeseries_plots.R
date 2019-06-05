#' @importFrom tidyr gather
#' @import ggplot2
#' @import dplyr

.plot_nc_heatmap <- function(file, var_name, reference, legend.title , interval,
                             text.size, show.legend, legend.position, plot.title,
                             color.palette, color.direction) {
  
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
                   color.palette, color.direction)
}


.plot_df_heatmap <- function(dataLong, var_name, legend.title, text.size, 
                             show.legend, legend.position, plot.title,
                             color.palette, color.direction, ylabel) {
                             
                             # num_cells, palette, title_prefix=NULL, overlays=NULL, xaxis=NULL, col_lim){
  
  
  h1 = ggplot(data = dataLong, aes(DateTime, depth.numeric)) +
  geom_raster(aes_string(fill = var_name), interpolate = F, hjust = 0.5, vjust = 0.5, show.legend = show.legend) +
  scale_y_reverse(expand = c(0.01,0.01)) +
  scale_x_datetime(expand = c(0.01,0.01)) +
  scale_fill_distiller(palette = color.palette, direction = color.direction, na.value = "grey90") +
  # scale_fill_viridis_c(alpha = 0.95, option = 'plasma') +
  ylab('Depth (m)') + xlab('Date') +
  labs(fill = legend.title, title = plot.title) +
  theme_bw(base_size = text.size) +
  theme(legend.position = legend.position)
  
  return(h1)
}

#' @importFrom graphics points
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
