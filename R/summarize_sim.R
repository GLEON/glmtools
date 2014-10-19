#'@title creates GLM simulation summary outputs
#'
#'@param file a string with the path to the netcdf output from GLM
#'@param fig_path FALSE if plot to screen, string path if save plot as .png
#'@param sim_outputs a character vector of outputs. 
#'See \code{get_}, timeseries (as in temp, evaporation, etc)
#'@keywords methods
#'@author
#'Jordan S. Read, Luke A. Winslow
#'@import rLakeAnalyzer
#'@seealso \link{get_temp}, \link{get_surface_height}, \link{get_evaporation}
#'@examples
#'nc_file <- system.file('extdata', 'output.nc', package = 'glmtools')
#'summarize_sim(nc_file)
#'@export
summarize_sim <- function(file, fig_path = FALSE, sim_outputs = c('temp', 'evaporation')){
  
  num_metrics = length(sim_outputs)
  colbar_layout(num_metrics)
  
  for (i in 1:num_metrics){
    plot_fcn <- paste0('plot_', sim_outputs[i])
    tryCatch(
      {
        # if it already has a plot function, use that
        do.call(get(plot_fcn), list('file' = file, 'add' = TRUE))
        
      }, error = function(err) {
        get_fcn <- paste0('get_', sim_outputs[i])
        ts <- do.call(get(get_fcn), list('file' = file))
        xaxis <- get_xaxis(ts$DateTime)
        yaxis <- get_yaxis(data = ts[, 2], 'title' = sim_outputs[i])
        plot_layout(xaxis, yaxis, add = T)
        plot(ts)
        #plot_generic(file = file, )
        plot(0,NA, axes = F, ylim = c(0,1), xlim = c(0,1), ylab='',xlab='') # fill up colorbar null space
      }
    )
  }
  
  return(FALSE)
}
