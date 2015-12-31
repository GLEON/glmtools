#'@title Plot matching heatmaps for modeled and observed temp
#'@param nc_file Netcdf model output file
#'@param field_file CSV or TSV field data file (see \link{resample_to_field} for format)
#' @param fig_path F if plot to screen, string path if save plot as .png
#'@param \dots additional arguments passed to \code{\link{resample_to_field}}
#'
#'@seealso Internally uses \code{\link{plot_var_compare}}, \code{\link{get_temp}} and \code{\link{resample_to_field}}
#'
#'@author Luke Winslow
#'
#'@examples
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'nml_file <- file.path(sim_folder, 'glm2.nml')
#'field_file <- file.path(sim_folder, 'field_data.tsv')
#'
#'plot_temp_compare(nc_file, field_file) ##makes a plot!
#'@export
plot_temp_compare = function(nc_file, field_file, fig_path=FALSE, ...){
	
	plot_var_compare(nc_file, field_file, var_name='temp', fig_path=fig_path, ...)
  
}
