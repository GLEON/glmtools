#'Plot meterological drivers from a csv file
#'@inheritParams read_nml
#'@param met_file Options: 1) file path to glm2.nml file. Date limits pulled from .nml file
#'2) file path to .csv file. csv file in wide format. First column is Date, additional columns are meteorological paramters. 
#'3) data.frame object in wide format. First column is Date, additional columns are meteorological paramters. 
#'@param xmin Optional, start date for xaxis. Formatted as same date format as meteo file. 
#'@param xmax Optional, end date for xaxis. Formatted as same date format as meteo file. 
#'@param fig_path Logical; F if plot to screen, string path if save plot as .png
#'@param ... additional arguments passed to \code{\link[ggplot2:ggsave]{ggplot2:ggsave}} 
#'@keywords methods
#'@seealso \code{\link{read_nml}}
#'@importFrom utils read.csv
#'@importFrom tools file_ext
#'@importFrom readr read_csv
#'@author
#'Jordan S. Read, Luke A. Winslow
#'@examples 
#'met_file <- nml_template_path()
#'
#'plot_meteo(met_file)
#'@export
plot_meteo <- function(met_file, xmin = NA, xmax = NA, fig_path = FALSE, ...){
  
  if (is.character(met_file)){
    if (file_ext(met_file) == 'nml'){
      nml_path <- nml_path_norm(met_file)
      glm_nml <- read_nml(nml_path)
      met_file_name <- get_nml_value(glm_nml,'meteo_fl') 
      met_path <- file.path(dirname(met_file),met_file_name)
    
      if (!file.exists(met_path)){stop(paste0("met_file points to a meteo file that doesn't exist:\n",met_path))}
      
      meteo <- read_csv(file = met_path) 
      
      # Get date limits for x-axis
      # to do: code this to use the timefmt variable instead of assuming timefmt == 2
      timefmt <- get_nml_value(glm_nml, 'timefmt')
      if (timefmt != 2){
        warning(paste0('time format ', timefmt, ' is not currently supported. 
                   Entire driver dataset will be plotted'))
      } else {
        xmin <- as.POSIXct(get_nml_value(glm_nml, 'start'))
        xmax <- as.POSIXct(get_nml_value(glm_nml, 'stop'))
      }
    } 
    
    if (file_ext(met_file) == 'csv'){
      meteo <- read_csv(file = met_path) 
    }
  }
  
  if (!is.character(met_file)){
    meteo = met_file
  } 
  
  meteoLong = meteo %>% gather(key = 'parameter',value = 'value', -1)
  
  if (is.na(xmin)){
    xmin = pull(meteoLong[1,1])
  }
  
  firstcol = names(meteoLong)[1]
  p1 = ggplot(meteoLong, aes(x = get(firstcol), y = value)) + geom_point(alpha = 0.8, size = 0.5) +
    facet_grid(vars(parameter), scales = 'free_y') +
    xlim(xmin,xmax) +
    xlab('Date') + ylab('') +
    theme_bw() 
  
  print(p1)
  
  # Saving plot 
  if (is.character(fig_path)){
    ggsave(plot = p1, filename = fig_path,...)
  }   
    

}


