#'@title plot meterological drivers from a csv file
#'@param nml_file a string with the path to the nml file for the simulation 
#'@param fig_path FALSE if plot to screen, string path if save plot as .png
#'@keywords methods
#'@seealso \link{read_nml}
#'@author
#'Jordan S. Read, Luke A. Winslow
#'@examples 
#'nml_file <- nml_template_path()
#'
#'plot_meteo(nml_file)
#'@export
plot_meteo <- function(nml_file, fig_path = FALSE){
  glm_nml <- read_nml(nml_file)
  met_file <- get_nml_value(glm_nml,'meteo_fl') 
  met_path <- file.path(dirname(nml_file),met_file)

  if (is.character(fig_path)){
    stop('figure save not currently supported. Use fig_path = FALSE')
  }
  if (!file.exists(met_path)){stop(paste0("nml_file points to a meteo file that doesn't exist:\n",met_path))}
  
  meteo <- read.csv(file = met_path)
  dates <- as.POSIXct(meteo$time) # to do: get timezone from lat/lon
  
  meteo <- meteo[, -1] # pop off the dateTime col
  
  # to do: code this to use the timefmt variable instead of assuming timefmt == 2
  timefmt <- get_nml_value(glm_nml, 'timefmt')
  if (timefmt != 2){
    warning(paste0('time format ', timefmt, ' is not currently supported. 
                   Entire driver dataset will be plotted'))
  } else {
    start_dt <- as.POSIXct(get_nml_value(glm_nml, 'start'))
    stop_dt <- as.POSIXct(get_nml_value(glm_nml, 'stop'))
    use_i <- dates >= start_dt & dates <= stop_dt
    dates <- dates[use_i]
    meteo <- meteo[use_i, ]
  }
  
  panels <- matrix(seq(1,ncol(meteo)))
  layout(panels)
  par(oma = c(0,0,0,0), mar = c(1,3,.3,0), mgp=c(1,0,0), tck = 0.02)
  for (i in 1:ncol(meteo)){
    plot(dates, meteo[[i]], xlab = '', ylab = names(meteo[i]))
  }
  
  
}