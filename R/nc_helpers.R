
get_glm_nc  <-  function(file){
	require(ncdf4)
	glm_nc	<- 	nc_open(file)
	return(glm_nc)
}

close_glm_nc <- function(glm_nc){
  nc_close(glm_nc)
}
################################################################################
#
################################################################################
# Summary: Returns the converted time vector in R format
get_time  <-  function(glm_nc){
  require(ncdf4)
	hours_since  <-   ncvar_get(glm_nc, "time")
	time_info <- get_time_info(glm_nc)

	time <- time_info$startDate + time_info$time_unit * hours_since * 60*60*24

	return(time)
	
}


# Summary: Determines the epoch and timestep to properly convert the model date/time
#
# Input:
#	GLMnc:	The ncdf file object reference, from nc_open

get_time_info <- function(glm_nc){
  require(ncdf4)
	day_secs = 86400
  time_unit <- 3600/day_secs

	#The units attribute on the time variable has basically the info we need
	time_units <- ncatt_get(glm_nc,'time','units')$value

	#It is written in prose instead of machine-readable format. Check to makes sure
	# it says "hours since ", then we know the timestep is hours. As far as I know, 
	# this never changes
	tiCheck <- regexpr('(hours since) (.*)' ,time_units, perl=TRUE)

	#make sure the unit string is as expected. I think 
	# the timestep is always in hours
	if(attr(tiCheck,'capture.start')[1] < 0 || attr(tiCheck,'capture.start')[2] < 0){
		stop('Unexpected time unit in NetCDF file')
	}

	# Get the epoch from the unit string
	epoch <- substr(time_units, attr(tiCheck,'capture.start')[2], attr(tiCheck,'capture.start')[2] + attr(tiCheck,'capture.length')[2])

	#get the length of the time data, will use this later
	tLen <- glm_nc$dim[["time"]][["len"]]
	
  time_info  <-  data.frame("time_unit"=time_unit)
  time_info  <-  cbind(time_info,"startDate"=as.POSIXct(epoch))

	#End date/time 
	endT <- time_info$startDate + ncvar_get(GLMnc,'time',start=tLen, count=1) * time_unit * day_secs

  time_info  <-  cbind(time_info,"stopDate"=endT)
	return(time_info)
}


