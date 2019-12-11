#' Plot matching heatmaps for modeled and observed temp
#' @param nc_file Netcdf model output file
#' @param field_file CSV or TSV field data file (see \link{resample_to_field} for format)
#' @param var_name a character vector of valid variable names (see \code{\link{sim_vars}})
#' @param fig_path Default is NULL (only plots to screen). Enter string path to save as output file. File type can be anything supported by \code{\link[ggplot2:ggsave]{ggplot2:ggsave}}. See examples. 
#' @param resample sample the model output to the same time points as the observations?
#' @param precision the time interval of the output.nc file and the field file must match (options: 'secs', 'mins','hours', or 'days')
#' @param conversion conversion multiplier to adjust model output to field data units
#' @param legend.title Vector string; Default (`NULL`) will use variable and units from netcdf file
#' @param interval Positive number indicating the depth interval in meters to interpolate output data. Must be less than max depth of lake. Default = 0.5 m. 
#' @param method String; 'match' for exact match or 'interp' for temporal interpolation
#' @param text.size Integer; Default is 12. Higher values will increase text size in plot.
#' @param color.palette See \code{\link[ggplot2:scale_color_distiller]{ggplot2:scale_color_distiller}} . If a string, will use that named palette. Default is 'RdYlBu'. If a number, will index into the list of palettes of appropriate. 
#' Palettes available include: Diverging:
#' BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn. Spectral. Qualitative: Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3. Sequential:
#' Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd.
#' @param color.direction Sets the order of colors in the scale. If 1, colors are as output by brewer.pal. If -1, the order of colors is reversed (default).
#' @param obs.color Color of observation points. For options see vignette("ggplot2-specs")
#' @param obs.alpha Alpha transparency of observation points. If set to 0, no points will appear. For options see vignette("ggplot2-specs")
#' @param obs.shape Shape of observation points. For options see vignette("ggplot2-specs")
#' @param obs.size Size of observation points. For options see \code{\link[vignette("ggplot2-specs")]{vignette("ggplot2-specs")}}
#' @param shiftPalette See values argument in \code{\link[ggplot2:scale_color_distiller]{ggplot2:scale_color_distiller}}. Default is c(0,1). To shift pallete lower. Use c(0,0.2,1).
#' @param zlim Color palette limits for z-variable. Default is maximum range of variable. Set as c(value,value). 
#' @param \dots additional arguments passed to \code{ggsave()}
#'
#' @seealso Internally uses \link{get_var} and \link{resample_to_field}
#'
#'
#'@examples
#'nc_file <- system.file("extdata", "output.nc", package = "glmtools")
#'field_file <- system.file("extdata", "LakeMendota_field_data.csv", package = "glmtools")
#'
#'plot_var_compare(nc_file, field_file, 'temp', resample=FALSE) ##makes a plot
#'
#'#Change clor palette, custom legend title, save figure:
#'plot_var_compare(nc_file, field_file, var_name = 'temp', resample = F, legend.title = 'Temp (*C)',
#'color.palette = 'PuBuGn', color.direction = 1,fig_path = '~/Downloads/figtest.png', width = 6, height = 8, units = 'in')
#'
#'@importFrom gridExtra grid.arrange
#'@author
#'Jordan S. Read, Luke A. Winslow, Hilary A. Dugan
#'@export
plot_var_compare = function(nc_file, field_file, var_name = 'temp', fig_path = NULL, resample = TRUE,
                            precision = 'hours', conversion = NULL,
                            legend.title = NULL, interval = 1,method = 'match', text.size = 12,
                            color.palette = 'RdYlBu', color.direction = -1, 
                            obs.color = 'white', obs.alpha = 0.6, obs.shape = 16, obs.size = 1, shiftPalette = NULL, zlim = NULL, ...) {
  
  heatmaps <- .is_heatmap(nc_file, var_name)
  if (!heatmaps){
    warning('plot_var_compare not implemented for 1D variables')
    return()
  }
  
  surface <- get_surface_height(nc_file)
  max_depth <- max(surface[, 2])
  min_depth <- 0
  z_out <- seq(min_depth, max_depth,by = interval) # Set plotting interval
  modeled_var = get_var(nc_file, var_name, reference='surface',z_out = z_out)

  # Resample 
  data = resample_to_field(nc_file, field_file, var_name=var_name, method = method, precision = precision) %>% 
    mutate(type = as.factor('Observed'))
  dataClean = data %>% dplyr::filter_all(all_vars(!is.na(.)))
  
  # Akima interpolation of observed data (Gridded Bivariate Interpolation for Irregular Data)
  observed_df <- .interpolate2grid(dataClean, xcol = 1, ycol = 2, zcol = 3) %>% 
    rename(DateTime = x, Depth=y, var=z)

  # Should modeled data be resampled to match resolution of field data?
  if(resample == TRUE) {
    # Akima interpolation of observed data (Gridded Bivariate Interpolation for Irregular Data)
    model_df <- .interpolate2grid(dataClean, xcol = 1, ycol = 2, zcol = 4)
  	names(model_df) = c('DateTime','Depth','var')

  } else {
  	model_df = modeled_var
  	names.df = data.frame(names = names(model_df)[-1], Depth = z_out, stringsAsFactors = F)
  	model_df = gather(data = model_df,key = depth, value = var,-DateTime) %>%
  	  left_join(names.df, by = c('depth' = 'names')) %>%
  	  arrange(DateTime, Depth)
  }

  if(is.null(legend.title)) {
    legend.title = .unit_label(nc_file, var_name)
  }

  if (var_name != 'temp' & is.null(conversion)) {
    h1 = ggplot(data = observed_df, aes(x = DateTime, y = Depth)) +
      geom_raster(aes(fill = var), interpolate = F) +
      geom_point(data = data, aes(x = DateTime, y = Depth), color = obs.color, alpha = obs.alpha, shape = obs.shape, size = obs.size) +
      scale_y_reverse(expand = c(0.01,0.01)) +
      scale_x_datetime(expand = c(0.01,0.01), limits = c(min(observed_df$DateTime), max(observed_df$DateTime))) +
      scale_fill_distiller(palette = color.palette, direction = color.direction, na.value = "grey90", limits = zlim) +
      ylab('Depth (m)') + xlab('Date') +
      labs(fill = legend.title, title = 'Observed') +
      theme_bw(base_size = text.size)
  
    h2 = ggplot(data = model_df, aes(DateTime, Depth)) +
      geom_raster(aes(fill = var), interpolate = F) +
      scale_y_reverse(expand = c(0.01,0.01)) +
      scale_x_datetime(expand = c(0.01,0.01), limits = c(min(observed_df$x), max(observed_df$x))) +
      scale_fill_distiller(palette = color.palette, direction = color.direction, na.value = "grey90", limits = zlim) +
      ylab('Depth (m)') + xlab('Date') +
      labs(fill = legend.title, title = 'Modeled') +
      theme_bw(base_size = text.size)
  
    h3 = grid.arrange(h1,h2)
  }
  
  if (var_name == 'temp' | !is.null(conversion)) {
    if (!is.null(conversion))  {model_df = model_df %>% mutate(var = var * conversion)}

  dfCombine = mutate(observed_df, type = 'Observed') %>% 
    bind_rows(mutate(model_df, type = 'Modeled')) %>% 
    mutate(type = factor(type, levels=c('Observed','Modeled')))
  
  h3 = ggplot(data = dfCombine, aes(DateTime, Depth)) +
    geom_raster(aes(fill = var), interpolate = F) +
    geom_point(data = data, aes(x = DateTime, y = Depth), color = obs.color, alpha = obs.alpha, shape = obs.shape, size = obs.size) +
    scale_y_reverse(expand = c(0.01,0.01)) +
    scale_x_datetime(expand = c(0.01,0.01), limits = c(min(dfCombine$DateTime), max(dfCombine$DateTime))) +
    scale_fill_distiller(palette = color.palette, direction = color.direction, na.value = "grey90", values = shiftPalette, limits = zlim) +
    ylab('Depth (m)') + xlab('Date') +
    facet_wrap(type ~ ., ncol = 1) + 
    labs(fill = legend.title) +
    theme_bw(base_size = text.size) 
  
  print(h3)
  }
    
  # Saving plot 
  if (!is.null(fig_path)){
    ggsave(plot = h3, filename = fig_path,...)
  } 
  return(h3) #return as ggplot object 
}
