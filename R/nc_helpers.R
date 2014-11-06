
get_glm_nc  <-  function(file){
	glm_nc	<- 	open.ncdf(file, readunlim=TRUE)
	return(glm_nc)
}

close_glm_nc <- function(glm_nc){
  close.ncdf(glm_nc)
}


# Summary: Returns the converted time vector in R format
get_time  <-  function(glm_nc){
	hours_since  <-   get.var.ncdf(glm_nc, "time")
	time_info <- get_time_info(glm_nc)

	time <- time_info$startDate + time_info$time_unit * hours_since * 60*60*24

	return(time)
	
}

get_time_info <- function(glm_nc, file = NULL){
  
	day_secs = 86400
  time_unit <- 3600/day_secs

	#The units attribute on the time variable has basically the info we need
  if (missing(glm_nc)){
    glm_nc <- get_glm_nc(file)
  }
	time_units <- att.get.ncdf(glm_nc,'time','units')$value

	
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
  start_date <- coerce_date(epoch)
  time_info  <-  cbind(time_info,"startDate"=start_date)

	#End date/time 
	endT <- time_info$startDate + get.var.ncdf(glm_nc, 'time', start=tLen, count=1) * time_unit * day_secs

  time_info  <-  cbind(time_info,"stopDate"=endT[1])
	if (missing(glm_nc)){
	  close_glm_nc(glm_nc)
	}
	return(time_info)
}


