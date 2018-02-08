#'Plot simulated and observed stage data
#'
#'@param nc_file NetCDF model output file
#'@param field_file CSV or TSV field data file (see \link{resample_to_field} for format)
#'@param ... Additional paramters supplied to the \code{plot} function
#'
#'@examples
#'
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'
#'field_file <- file.path(sim_folder, 'field_stage.csv')
#'
#'plot_compare_stage(nc_file, field_file) ##makes a plot!
#'
#'
#'@export
plot_compare_stage = function(nc_file, field_file, ...){
	
	#get modeled stage prepped
	stage_mod = get_surface_height(nc_file, ice.rm = FALSE, snow.rm = FALSE)
	
	minmax = range(stage_mod[,2])
	
	#read-in field stage 
	stage_obs = read_field_stage(field_file)
	
	minmax = range(minmax, stage_obs[,2])
	
	plot(stage_mod, type='l', xlab='', ylab='Stage', ylim=minmax, ...)
	points(stage_obs)
}
