#'Run diagnostics on model results vs observations
#'@param nc_file a string with the path to the netcdf output from GLM
#'@param field_file a string with the path to the field observation file
#'@param metrics a string or vector of strings representing physical metrics. 
#'Should be a rLakeAnalyzer function or other valid function.
#'@param fig_path Default is NULL (only plots to screen). Enter string path to save as output file. File type can be anything supported by \code{\link[ggplot2:ggsave]{ggplot2:ggsave}}. See examples. 
#'@param report default as FALSE, so no stats are returned. If TRUE, return summary stats for each metric used.
#'@param ... additional arguments passed to \code{\link[ggplot2:ggsave]{ggplot2:ggsave}} 
#'@return A report or figures comparing modeled results to observations. 
#'@keywords methods
#'@seealso \link{compare_to_field}, \link{resample_to_field}, \link{read_nml}, \link{sim_metrics}
#'@author
#'Jordan S. Read, Hilary A. Dugan
#'@examples 
#'\dontrun {
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output/output.nc')
#'nml_file <- file.path(sim_folder, 'glm3.nml')
#'field_file <- file.path(sim_folder, 'LakeMendota_field_data_hours.csv')
#'
#
#' #  create a multiple metric diagnostic fig within R:
#'validate_sim(nc_file, field_file, nml_file = nml_file,
#'                           metrics = c('thermo.depth', 'schmidt.stability'), 
#'                           fig_path = NULL)      
#'
#' # write the fig out to file:
#'validate_sim(nc_file, field_file, nml_file = nml_file, fig_path = 'test_fig.png',
#'                           metrics = c('thermo.depth', 'schmidt.stability'))   
#'                   
#' # return the diagnostic info but avoid the plot:                                   
#'validate_sim(nc_file, field_file, nml_file = nml_file, report = TRUE,
#'                           metrics = c('thermo.depth', 'schmidt.stability'))     
#'}
#'@importFrom patchwork wrap_plots     
#'@export
validate_sim <- function(nc_file, field_file, nml_file, metrics, fig_path = NULL, 
                         report = FALSE, ...){
  warning('function in development')
  # need to check is.missing for fig_path...
  num_metrics <- length(metrics)

  h = list() #for ggplots
  # build plot accordingly
  
  for (i in 1:num_metrics){
    vals <- compare_to_field(nc_file, field_file, nml_file, metric = metrics[i], as_value = TRUE)
    
    h[[i]] = ggplot(vals) + geom_point(aes(x = obs, y = mod)) +
      xlab('Observed') + ylab('Modeled') +
      labs(title = metrics[i]) +
      geom_abline() +
      theme_bw()
  }
  
  # Saving plot 
  if (!is.null(fig_path)){
    ggsave(plot = wrap_plots(h,ncol = 1), filename = fig_path,...)
  } 
  
  print(wrap_plots(h,ncol = 1))


  if (report){
    ret_list <- vector("list", length(metrics))
    names(ret_list) <- metrics
    for (i in 1:num_metrics){
      ret_list[[i]] <- compare_to_field(nc_file, field_file, nml_file, metric = metrics[i], as_value = FALSE)
    }
    return(ret_list)
  }

}