#'Plot validation and model temperature profiles for all unique dates
#'@param nc_file a string with the path to the netcdf output from GLM
#'@param field_file a string with the path to the field observation file
#'@param fig_path Default is NULL (only plots to screen). Enter string path to save as output file. File type can be anything supported by \code{\link[ggplot2:ggsave]{ggplot2:ggsave}}. See examples. 
#'If argument is not used, plotting is skipped
#' @param \dots additional arguments passed to \code{ggsave()}
#'@keywords methods
#'@seealso \link{validate_sim}, \link{resample_to_field}
#'@author
#'Luke A. Winslow, Jordan S. Read, Hilary A. Dugan
#'@examples 
#'nc_file <- system.file("extdata", "output.nc", package = "glmtools")
#'field_file <- system.file("extdata", "LakeMendota_field_data.csv", package = "glmtools")
#'
#' #  create a multiple metric diagnostic fig within R:
#'plot_validate_profiles(nc_file, field_file, fig_path = NULL, method = 'interp')             
#'@export

plot_validate_profiles <- function(nc_file, field_file, fig_path = NULL, ...){

	#get validation data
	temp_val = read_field_obs(field_file)
  
	#load temperature from nc
	mod_and_obs <- resample_to_field(nc_file, field_file, ...) %>% 
	  arrange(DateTime, Depth)
	
	mod_and_obs_long = mod_and_obs %>% gather(Group, Temp, -DateTime, -Depth)
	
	h3 = ggplot(mod_and_obs_long) + geom_path(aes(y = .data$Depth, x = .data$Temp, color = .data$Group)) +
	  geom_point(aes(y = .data$Depth, x = .data$Temp, fill = .data$Group, color = .data$Group)) +
	  scale_color_manual(values = c('black','lightblue4')) +
	  scale_y_reverse() +
	  theme_bw() + theme(legend.title = element_blank()) +
	  facet_wrap(~as.Date(DateTime))
	
	# Saving plot 
	if (!is.null(fig_path)){
	  ggsave(plot = h3, filename = fig_path,...)
	} 
	
	return(h3) #return as ggplot object 

}
