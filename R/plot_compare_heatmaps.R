#'@title Plot matching heatmaps for modeled and observed temp
#'@param nc_file Netcdf model output file
#'@param field_file CSV or TSV field data file (see \link{resample_to_field} for format)
#'
#'@seealso Internally uses \link{get_temp} and \link{resample_to_field}
#'
#'@author Luke Winslow
#'
#'@examples
#'nc_file <- system.file('extdata', 'output.nc', package = 'glmtools')
#'field_file <- system.file('extdata', 'field_data.tsv', package = 'glmtools')
#'
#'plot_temp_compare(nc_file, field_file) ##makes a plot!
#'
#'@importFrom akima interp
#'@export
plot_temp_compare = function(nc_file, field_file, ...){
	
	start_par = par()
	#Create layout
	
	mod_temp = get_temp(nc_file, reference='surface')
	mod_depths = get.offsets(mod_temp)
	
	data = resample_to_field(nc_file, field_file, ...)
	
	#Pivot observed into table
	
	x = as.numeric(as.Date(data$DateTime))
	#minx = min(x)
	#x = x - minx
	y = data$Depth
	z = data$Observed_wTemp
	
	full_x = sort(unique(as.numeric(as.Date(mod_temp$DateTime))))
	full_y = sort(unique(mod_depths))
	
	interped = interp(x, y, z, full_x, full_y)
	
	palette <- colorRampPalette(c("violet","blue","cyan", "green3", "yellow", "orange", "red"), 
															bias = 1, space = "rgb")
	colors = palette(15)
	levels = seq(0, 36, by=36/15)
	
	par(mfrow=c(2,1), mar=c(1.1, 4.1, 1.1, 1.1), oma=c(3, 0, 0, 0))
	plot(range(as.Date(mod_temp$DateTime)), c(NA, NA), ylim=rev(range(full_y)), 
			 xlim=range(as.Date(mod_temp$DateTime)), xaxt='n', xlab='', ylab='Observed Temp (C)')
	
	.filled.contour(as.Date(full_x, origin='1970-01-01'), full_y, interped$z,
								 levels=levels, col=colors)
	points(x,y)
	
	plot(range(as.Date(mod_temp$DateTime)), c(NA, NA), ylim=rev(range(full_y)), 
			 xlim=range(as.Date(mod_temp$DateTime)), xlab='', ylab='Modeled Temp (C)')
	
	.filled.contour(as.Date(full_x, origin='1970-01-01'), full_y, as.matrix(mod_temp[,-1]),
									levels=levels, col=colors)
	
	
	par(start_par)#set PAR back to what it started at

}
