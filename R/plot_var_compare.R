#' Plot matching heatmaps for modeled and observed temp
#' @param nc_file Netcdf model output file
#' @param field_file CSV or TSV field data file (see \link{resample_to_field} for format)
#' @param var_name a character vector of valid variable names (see \code{\link{sim_vars}})
#' @param fig_path F if plot to screen, string path if save plot as .png
#' @param resample sample the model output to the same time points as the observations?
#' @param legend.title Vector string; Default (`NULL`) will use variable and units from netcdf file
#' @param interval Positive number indicating the depth interval in meters to interpolate output data. Must be less than max depth of lake. Default = 0.5 m. 
#' @param method String; 'match' for exact match or 'interp' for temporal interpolation
#' @param text.size Integer; Default is 12. Higher values will increase text size in plot.
#' @param color.palette See \code{\link[ggplot2:scale_color_distiller]{ggplot2:scale_color_distiller}} . If a string, will use that named palette. If a number, will index into the list of palettes of appropriate. 
#' Palettes available include: Diverging:
#' BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn. Spectral. Qualitative: Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3. Sequential:
#' Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd.
#' @param color.direction Sets the order of colors in the scale. If 1, the default, colors are as output by brewer.pal. If -1, the order of colors is reversed.
#' @param \dots additional arguments passed to \code{ggsave()}
#'
#' @seealso Internally uses \link{get_var} and \link{resample_to_field}
#'
#'
#'@examples
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'nml_file <- file.path(sim_folder, 'glm2.nml')
#'field_file <- file.path(sim_folder, 'field_data.tsv')
#'
#'run_glm(sim_folder)
#'
#'plot_var_compare(nc_file, field_file, 'temp', resample=FALSE) ##makes a plot
#'
#'#Change clor palette, custom legend title, save figure:
#'plot_var_compare(nc_file, field_file, var_name = 'temp', resample = F, legend.title = 'Temp (*C)',
#'color.palette = 'PuBuGn', color.direction = 1,fig_path = '~/Downloads/figtest.png', width = 6, height = 8, units = 'in')
#'
#'@importFrom akima interp
#'@importFrom akima interp2xyz
#'@importFrom gridExtra grid.arrange
#'@author
#'Jordan S. Read, Luke A. Winslow, Hilary A. Dugan
#'@export
plot_var_compare = function(nc_file, field_file, var_name = 'temp', fig_path = FALSE, resample = TRUE, 
                            legend.title = NULL, interval = 1,method = 'match', text.size = 12,
                            color.palette = 'RdYlBu', color.direction = -1, ...) {
  
  heatmaps <- .is_heatmap(nc_file, var_name)
  if (!heatmaps){
    warning('plot_var_compare not implemented for 1D variables')
    return()
  }
  
  surface <- get_surface_height(nc_file)
  max_depth <- max(surface[, 2])
  min_depth <- 0
  z_out <- seq(min_depth, max_depth,by = interval) # Set plotting interval
  mod_temp = get_var(nc_file, var_name, reference='surface',z_out = z_out)

  data = resample_to_field(nc_file, field_file, var_name=var_name, method = method) 
  
  # Akima interpolation of observed data
  dataClean = data %>% dplyr::filter_all(all_vars(!is.na(.)))

  df_akima <-interp2xyz(interp(x=as.numeric(dataClean$DateTime), y=dataClean$Depth*1e6, z=dataClean$Observed_temp, duplicate="mean", linear = T,
                               # xo=seq.POSIXt(min(dataClean$DateTime), max(dataClean$DateTime), by = 'day'),
                               xo = as.numeric(seq(min(dataClean$DateTime), max(dataClean$DateTime), by = 'day')),
                               yo = 1e6*seq(min(dataClean$Depth), max(dataClean$Depth), by = 1)), data.frame=TRUE) %>%
    dplyr::mutate(x =  as.POSIXct(x, origin = '1970-01-01', tz = Sys.timezone())) %>%
    dplyr::mutate(y = y/1e6) %>%
    dplyr::arrange(x,y)

  # Resample modeled data?
  if(resample == TRUE) {
  	model_df <-interp2xyz(interp(x=as.numeric(dataClean$DateTime), y=dataClean$Depth*1e6, z=dataClean$Modeled_temp, duplicate="mean", linear = T,
  	                             # xo=seq.POSIXt(min(dataClean$DateTime), max(dataClean$DateTime), by = 'day'),
  	                             xo = as.numeric(seq(min(dataClean$DateTime), max(dataClean$DateTime), by = 'day')),
  	                             yo = 1e6*seq(min(dataClean$Depth), max(dataClean$Depth), by = 1)), data.frame=TRUE) %>%
  	  dplyr::mutate(x =  as.POSIXct(x, origin = '1970-01-01', tz = Sys.timezone())) %>%
  	  dplyr::mutate(y = y/1e6) %>%
  	  dplyr::arrange(x,y)
  	names(model_df) = c('DateTime','Depth','var')

  } else {
  	model_df = mod_temp
  	names.df = data.frame(names = names(model_df)[-1], Depth = z_out, stringsAsFactors = F)
  	model_df = gather(data = model_df,key = depth, value = var,-DateTime) %>%
  	  left_join(names.df, by = c('depth' = 'names')) %>%
  	  arrange(DateTime, Depth)
  }

  if(is.null(legend.title)) {
    legend.title = .unit_label(nc_file, var_name)
  }

  h1 = ggplot(data = df_akima, aes(x = x, y = y)) +
    geom_raster(aes(fill = z), interpolate = F) +
    geom_point(data = data, aes(x = DateTime, y = Depth), color = 'white', alpha = 0.6) +
    scale_y_reverse(expand = c(0.01,0.01)) +
    scale_x_datetime(expand = c(0.01,0.01), limits = c(min(df_akima$x), max(df_akima$x))) +
    scale_fill_distiller(palette = color.palette, direction = color.direction, na.value = "grey90") +
    ylab('Depth (m)') + xlab('Date') +
    labs(fill = legend.title, title = 'Observed') +
    theme_bw(base_size = text.size)

  h2 = ggplot(data = model_df, aes(DateTime, Depth)) +
    geom_raster(aes(fill = var), interpolate = F) +
    scale_y_reverse(expand = c(0.01,0.01)) +
    scale_x_datetime(expand = c(0.01,0.01), limits = c(min(df_akima$x), max(df_akima$x))) +
    scale_fill_distiller(palette = color.palette, direction = color.direction, na.value = "grey90") +
    ylab('Depth (m)') + xlab('Date') +
    labs(fill = legend.title, title = 'Modeled') +
    theme_bw(base_size = text.size)

  h3 = grid.arrange(h1,h2)

  # Saving plot
  if (is.character(fig_path)){
    ggsave(plot = h3, filename = fig_path,...)
  }

}
