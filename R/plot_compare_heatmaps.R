#'@title Plot matching heatmaps for modeled and observed temp
#'@param nc_file Netcdf model output file
#'@param field_file CSV or TSV field data file (see \link{resample_to_field} for format)
#'@param ... additional arguments passed to \code{\link{resample_to_field}}
#'
#'@seealso Internally uses \link{get_temp} and \link{resample_to_field}
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
#'
#'@importFrom akima interp
#'@export
plot_temp_compare = function(nc_file, field_file, ...){
	
	start_par = par(no.readonly = TRUE)
	#Create layout
	
	mod_temp = get_temp(nc_file, reference='surface')
	mod_depths = get.offsets(mod_temp)
	
	data = resample_to_field(nc_file, field_file, ...)
	
	#Pivot observed into table
	
	x = as.numeric(as.POSIXct(data$DateTime))
	#minx = min(x)
	#x = x - minx
	y = data$Depth
	z = data$Observed_wTemp
	
	full_x = sort(unique(as.numeric(as.POSIXct(mod_temp$DateTime))))
	full_y = sort(unique(mod_depths))
	
	
	#Added a scaling factor to Y. Interp won't interpolate if X and Y are on vastly different scales.
	# I don't use Y from here later, so it doesn't matter what the mangitude of the values is.
	interped = interp(x, y*1e6, z, full_x, full_y*1e6)
	
	palette <- colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red"), 
															bias = 1, space = "rgb")
	colors = palette(15)
	levels = seq(0, 36, by=36/15)
	
	par(mfrow=c(2,1), mar=c(1.1, 4.1, 1.1, 1.1), oma=c(3, 0, 0, 0))
	plot(range(as.POSIXct(mod_temp$DateTime)), c(NA, NA), ylim=rev(range(full_y)), 
			 xlim=range(as.POSIXct(mod_temp$DateTime)), xaxt='n', xlab='', ylab='Observed Temp (C)')
	
	.filled.contour(as.POSIXct(full_x, origin='1970-01-01'), full_y, interped$z,
								 levels=levels, col=colors)
	points(x,y)
	
	plot(range(as.POSIXct(mod_temp$DateTime)), c(NA, NA), ylim=rev(range(full_y)), 
			 xlim=range(as.POSIXct(mod_temp$DateTime)), xlab='', ylab='Modeled Temp (C)')
	
	.filled.contour(as.POSIXct(full_x, origin='1970-01-01'), full_y, as.matrix(mod_temp[,-1]),
									levels=levels, col=colors)
	
	
	par(start_par)#set PAR back to what it started at

}
