#'Plot variables from a data frame
#'@param data a dataframe directly from GLM output or field data. First column must be date/dateTime (Date or POSIXct format). 
#'Second column is depth. Subsequent columns are variable data. Format can be wide or long. If long format, can support multiple variables. 
#'@param var_name a character vector of the variable names
#'@param interpolate Logical; FALSE = do not inteprolate data. TRUE = Interpolate data to daily timestep and 1 m depth interval
#'@param fig_path Default is NULL (only plots to screen). Enter string path to save as output file. File type can be anything supported by \code{\link[ggplot2:ggsave]{ggplot2:ggsave}}. See examples. 
#'@param legend.title Vector string; Default (`NULL`) will use variable and units from netcdf file
#'@param interval Positive number indicating the depth interval in meters to interpolate output data. Must be less than max depth of lake. Default = 0.5 m. 
#'@param text.size Integer; Default is 12. Higher values will increase text size in plot.
#'@param show.legend Logical; TRUE to show legend (default), FALSE to hide legend
#'@param legend.position String; Legend position. Default is 'right'. Options: 'left','right','top','bottom'
#'@param plot.title Vector string; Default is no title. 
#'@param color.palette See \code{\link[ggplot2:scale_color_distiller]{ggplot2:scale_color_distiller}} . If a string, will use that named palette. Default is 'RdYlBu'. If a number, will index into the list of palettes of appropriate. 
#' Palettes available include: Diverging:
#' BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn. Spectral. Qualitative: Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3. Sequential:
#' Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd.
#'@param color.direction Sets the order of colors in the scale. If 1, colors are as output by brewer.pal. If -1, the order of colors is reversed (default).
#'@param reference String; 'surface' or 'bottom'. surface = Depths are referenced from the surface, bottom = Depths are referenced from the bottom (elevations)
#'@param depths Logical; TRUE = Depths are provided. FALSE = Elevations are provided. 
#'@param ... additional arguments passed to \code{\link[ggplot2:ggsave]{ggplot2:ggsave}} 
#'@keywords methods
#'@seealso \code{\link{get_var}}, \code{\link{sim_var_longname}}, 
#'\code{\link{sim_vars}}, \code{\link{plot_var}}
#'@note
#'\code{plot_var} uses the \code{\link[graphics]{layout}} function and so is restricted to a full page display.
#'When creating a heatmap, the output produced by \code{plot_var} is actually a combination of two plots; 
#'one is a \code{\link[graphics]{.filled.contour}} plot and the other is a legend.
#'@author
#'Jordan S. Read, Luke A. Winslow, Hilary A. Dugan
#'
#'@examples
#'nc_file <- system.file("extdata", "output.nc", package = "glmtools")
#'data = get_var(nc_file,'temp', reference = 'surface') 
#'plot_var_df(data, var_name = 'temp', interpolate = F, legend.title = 'Temp (degC)')
#'#Saving plot
#'plot_var(data, var_name = 'temp',fig_path = '~/figtest.png', width = 6, height = 2, units = 'in')
#'
#'\dontrun{
#'# need to specify a valid .nc file here: 
#'plot_var(file = fabm_sim_nc.nc,
#'var_name = 'aed_oxygen_oxy', 
#'fig_path = 'aed_out.png')
#'}
#'@importFrom gridExtra grid.arrange
#'@export
plot_var_df <- function(data, var_name, interpolate = F, fig_path = NULL, 
                     legend.title = var_name, text.size = 12, show.legend = TRUE, 
                     legend.position = 'right', plot.title = NULL, 
                     color.palette = 'RdYlBu', color.direction = -1,
                     reference = 'surface', ...) {
  
  # Determine data format
  if (lapply(data, class)[1] == 'Date') {
    data[,1] = as.POSIXct(paste(pull(data[,1]), '00:00:00'))
  }
  
  
  # Determine number of variables
  num_divs <- length(var_name) 
  
  # Determine if wide or long data frame 
  if (num_divs == 1 & ncol(data) > 3) {
    wide = TRUE
  
    # Convert to long
    z_out = parse_number(names(data)[-1])
    if (reference == 'surface'){
      names.df = data.frame(names = names(data)[-1], depth.numeric = z_out, stringsAsFactors = F)
      # ylabel = 'Depth (m)'
    }
    if (reference == 'bottom'){
      names.df = data.frame(names = names(data)[-1], depth.numeric = rev(z_out), stringsAsFactors = F)
      # ylabel = 'Elevation (m)'
    }
    dataLong = gather(data = data,key = depth, value = var,-DateTime) %>%
      left_join(names.df, by = c('depth' = 'names')) %>% 
      arrange(get(names(data[1]))) %>% 
      rename('DateTime' = 1)
    names(dataLong)[3] = var_name
    
  } else {
    wide = FALSE
    
    if (is.numeric(pull(data[,2])) == FALSE) {
      z_out = parse_number(as.character(pull(data[,2])))
      dataLong = data %>% mutate(depth.numeric = z_out) %>% 
        rename('DateTime' = 1)
    } else {
      dataLong = data %>% mutate(depth.numeric = .[[2]]) %>% 
        rename('DateTime' = 1)
    }
  }
  

  # iterate through plots
  h = list() #for ggplots
  for (j in 1:num_divs) {
    plotdata = dataLong 
    
    if (interpolate == T) {
      # Akima interpolation of observed data
      dataClean = as_tibble(dataLong) %>% dplyr::filter_all(all_vars(!is.na(.)))
      
      df_akima <-interp2xyz(interp(x=as.numeric(dataClean$DateTime), y=dataClean$depth.numeric*1e6, z=pull(dataClean[,var_name[j]]), duplicate="mean", linear = T,
                                   xo = as.numeric(seq(min(dataClean$DateTime), max(dataClean$DateTime), by = 'day')),
                                   yo = 1e6*seq(min(dataClean$depth.numeric), max(dataClean$depth.numeric), by = 1)), data.frame=TRUE) %>%
        dplyr::mutate(x =  as.POSIXct(x, origin = '1970-01-01', tz = Sys.timezone())) %>%
        dplyr::mutate(y = y/1e6) %>%
        dplyr::arrange(x,y)
      names(df_akima) = c('DateTime','depth.numeric',var_name[j])
      plotdata = df_akima
    }
    
    h[[j]] = .plot_df_heatmap(plotdata,var_name[j], legend.title[j], text.size, show.legend, legend.position, plot.title[j],
                                   color.palette, color.direction)
  }
  
  grid.arrange(grobs = h, ncol = 1)
  
  
  # Saving plot 
  if (!is.null(fig_path)){
    ggsave(filename = fig_path,...)
  } 
}

