#'@title Plot validation and model temperature profiles
#'@param nc_file a string with the path to the netcdf output from GLM
#'@param field_file a string with the path to the field observation file
#'@param fig_path FALSE if plot to screen, string path if save plot as .png. 
#'If argument is not used, plotting is skipped
#'@param ... optional arguments passed to \code{resample_to_field}
#'@keywords methods
#'@seealso \link{validate_sim}, \link{resample_to_field}
#'@author
#'Luke A. Winslow, Jordan S. Read
#'@examples 
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'nml_file <- file.path(sim_folder, 'glm2.nml')
#'field_file <- file.path(sim_folder, 'field_data.tsv')
#'
#' #  create a multiple metric diagnostic fig within R:
#'plot_validate_profiles(nc_file, field_file, fig_path = FALSE, method = 'interp')                        
#'@export
plot_validate_profiles <- function(nc_file, field_file, fig_path = FALSE, ...){
  valid_fig_path(fig_path)
  
  start_par = par(no.readonly = TRUE)
  
	if (is.character(fig_path)){
		#gen_default_fig(file_name = fig_path, fig_w = 2, fig_h = num_metrics*2, ps = 10, 
		#								l.mar = 0.5, r.mar = 0.1, b.mar = .4, t.mar = .1) 
		stop('Saving figures to file not yet supported in plot_validate_profiles')
	}

	#get validation data
	temp_val = read_field_obs(field_file)
  
	#load temperature from nc
	mod_and_obs <- resample_to_field(nc_file, field_file, ...)
  
	u_dates = unique(mod_and_obs$DateTime)
	
	for(i in 1:length(u_dates)){
		
		val_indx = mod_and_obs$DateTime == u_dates[i]
		
		plot(mod_and_obs$Modeled_wTemp[val_indx], mod_and_obs$Depth[val_indx], type='l', 
				 xlim=range(c(mod_and_obs$Modeled_wTemp[val_indx], mod_and_obs$Observed_wTemp[val_indx]), na.rm=TRUE),
				 ylab='Depth (m)', xlab='Temp (degC)', ylim=c(max(mod_and_obs$Depth[val_indx], na.rm = T),0),
				 main=strptime(u_dates[i], '%Y-%m-%d'))
		
		points(mod_and_obs$Observed_wTemp[val_indx], mod_and_obs$Depth[val_indx], pch=20)
		
	}
  par(start_par)
}
