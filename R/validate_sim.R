#'@title run diagnostics on model results vs observations
#'@param nc_file a string with the path to the netcdf output from GLM
#'@param field_file a string with the path to the field observation file
#'@param metrics a string or vector of strings representing physical metrics. 
#'Should be a rLakeAnalyzer function or other valid function.
#'@param fig_path FALSE if plot to screen, string path if save plot as .png. 
#'If argument is not used, plotting is skipped
#'@param report default as FALSE, so no stats are returned. If TRUE, return 
#'summary stats for each metric used.
#'@param ... additional arguments passed to compare_to_field (such as na.rm or nml_file)
#'@return a report or figures comparing modeled results to observations
#'@keywords methods
#'@seealso \link{compare_to_field}, \link{resample_to_field}, \link{read_nml}, \link{sim_metrics}
#'@author
#'Jordan S. Read
#'@examples 
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'nml_file <- file.path(sim_folder, 'glm2.nml')
#'field_file <- file.path(sim_folder, 'field_data.tsv')
#'
#
#' #  create a multiple metric diagnostic fig within R:
#'validate_sim(nc_file, field_file, nml_file = nml_file,
#'                           metrics = c('thermo.depth', 'schmidt.stability'), 
#'                           fig_path = FALSE)      
#'
#' # write the fig out to file:
#'validate_sim(nc_file, field_file, nml_file = nml_file, fig_path = '../test_fig.png',
#'                           metrics = c('thermo.depth', 'schmidt.stability'))   
#'                   
#' # return the diagnostic info but avoid the plot:                                   
#'validate_sim(nc_file, field_file, nml_file = nml_file, report = TRUE,
#'                           metrics = c('thermo.depth', 'schmidt.stability'))                                                       
#'@export
validate_sim <- function(nc_file, field_file, metrics, fig_path, report = F, ...){
  warning('function in development')
  # need to check is.missing for fig_path...
  num_metrics <- length(metrics)
  
  if (!missing(fig_path)){
    if (is.character(fig_path)){
      gen_default_fig(file_name = fig_path, fig_w = 2, fig_h = num_metrics*2, ps = 10, 
                      l.mar = 0.5, r.mar = 0.1, b.mar = .4, t.mar = .1) 
    }
    panels <- matrix(seq_len(num_metrics), nrow = num_metrics) # vertical panels
    layout(panels)
    # build plot accordingly 
    for (i in 1:num_metrics){
      vals <- compare_to_field(nc_file, field_file, metric = metrics[i], as_value = TRUE, ...)
      plot_one2one(x = vals[, 2], y = vals[, 3], xlab = 'Observed', ylab = 'Modeled', main = metrics[i])
    }
    if (is.character(fig_path)){
      dev.off()
    }
  }
  
  if (report){
    ret_list <- vector("list", length(metrics))
    names(ret_list) <- metrics
    for (i in 1:num_metrics){
      ret_list[[i]] <- compare_to_field(nc_file, field_file, metric = metrics[i], as_value = FALSE, ...)
    }
    return(ret_list)
  }
  
  
}