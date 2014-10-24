#'@title Plot validation and model temperature profiles
#'@param nc_file a string with the path to the netcdf output from GLM
#'@param field_file a string with the path to the field observation file
#'@param fig_path FALSE if plot to screen, string path if save plot as .png. 
#'If argument is not used, plotting is skipped
#'@keywords methods
#'@seealso \link{validate_sim}
#'@author
#'Luke A. Winslow, Jordan S. Read
#'@examples 
#'nc_file <- system.file('extdata', 'output.nc', package = 'glmtools')
#'nml_file <- system.file('extdata', 'glm.nml', package = 'glmtools')
#'field_file <- system.file('extdata', 'field_data.tsv', package = 'glmtools')
#'
#
#' #  create a multiple metric diagnostic fig within R:
#'plot_validate_profiles(nc_file, field_file, fig_path = FALSE)
#'                           
#'@export
plot_validate_profiles <- function(nc_file, field_file, fig_path = FALSE){
	
	if (is.character(fig_path)){
		#gen_default_fig(file_name = fig_path, fig_w = 2, fig_h = num_metrics*2, ps = 10, 
		#								l.mar = 0.5, r.mar = 0.1, b.mar = .4, t.mar = .1) 
		stop('Saving figures to file not yet supported in plot_validate_profiles')
	}
	
	#load temperature from nc
	temp = get_temp(nc_file, reference='surface')
	mod_dates = range(temp$DateTime)
	
	#get validation data
	temp_val = read_field_obs(field_file)
	
	u_dates = unique(temp_val$DateTime)
	
	for(i in 1:length(u_dates)){
		
		if(u_dates[i] < mod_dates[1] || u_dates[i] > mod_dates[2]){
			next #Skip val data that aren't within model window
		}
		
		#grab the nearest water temp for that date/time
		dt = abs(temp$DateTime - u_dates[i])
		min.idx = which.min(dt)
		
		if(as.double(dt[min.idx], units='days') > 1){
			warning('Nearest modeled date > 1 day away for field date: ', u_dates[i])
		}
		
		mod_depths = get.offsets(temp[min.idx,])
		mod_temp = as.numeric(temp[min.idx,-1])
		
		val_indx = temp_val$DateTime == u_dates[i]
		
		
		plot(mod_temp, mod_depths, type='l')#, 
				 #xlim=range(c(mod_temp, temp_val[val_indx, 'wTemp']), na.rm=TRUE),
				 #ylab='Depth (m)', xlab='Temp (degC)', ylim=rev(range(mod_depths)))
		
		val_indx = temp_val$DateTime == u_dates[i]
		points(temp_val[val_indx, 'wTemp'], temp_val[val_indx, 'Depth'], pch=20)
		
	}
}
