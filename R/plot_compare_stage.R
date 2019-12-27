#'Plot simulated and observed stage data
#'
#'@param nc_file NetCDF model output file
#'@param field_file CSV or TSV field data file (see \link{resample_to_field} for format)
#'@param fig_path Default is NULL (only plots to screen). Enter string path to save as output file. File type can be anything supported by \code{\link[ggplot2:ggsave]{ggplot2:ggsave}}. See examples. 
#'@param \dots additional arguments passed to \code{ggsave()}
#'
#'@examples
#'
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'
#'field_file <- file.path(sim_folder, 'LakeMendota_stage_USGS05428000.csv')
#'
#'plot_compare_stage(nc_file, field_file) ##makes a plot!
#'
#'
#'@export
plot_compare_stage = function(nc_file, field_file, fig_path = NULL, ...){
	
	#get modeled stage prepped
	stage_mod = get_surface_height(nc_file, ice.rm = FALSE, snow.rm = FALSE) %>% 
	  mutate(group = 'Modeled')
	names(stage_mod) = c('DateTime','Stage','Group')
	

	#read-in field stage 
	stage_obs = read_field_stage(field_file) %>% 
	  mutate(group = 'Observed')
	names(stage_obs) = c('DateTime','Stage','Group')
	
	# Bind modeled and observed data
	stage_all = bind_rows(stage_mod,stage_obs)
	
	h3 = ggplot(stage_all) + geom_path(aes(x = DateTime, y = Stage, color = Group)) +
	  geom_point(aes(x = DateTime, y = Stage, color = Group)) +
	  scale_color_manual(values = c('black','lightblue4')) +
	  theme_bw() + theme(legend.title = element_blank()) 
	
	# Saving plot 
	if (!is.null(fig_path)){
	  ggsave(plot = h3, filename = fig_path,...)
	} 
}
