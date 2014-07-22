#'@title get ice depth from GLM simulation
#'@description 
#'Creates a data.frame with DateTime and ice.  \cr
#'
#'
#'@param file a string with the path to the netcdf output from GLM
#'@param reference a string which specifies the vertical reference ('surface' or 'bottom')
#'@param z_out a vector of depths for temperature output (in meters)
#'@return a data.frame with DateTime and temperature at depth 
#'@keywords methods
#'@author
#'Jordan S. Read, Luke A. Winslow
#'@examples 
#'file = '../test/output.nc'
#'temp_surf <- get_temp(file,reference='surface',z_out=c(0,1,2))
#'temp_bot <- get_temp(file,reference='bot',z_out=c(0,1,2))
#'@export
get_temp <-  function(file, reference='bottom', z_out){
  if (reference!='bottom' & reference!='surface'){
    stop('reference input must be either "surface" or "bottom"')
  }
  
  glm_nc <- get_glm_nc(file)
  
  tallest_layer <- ncvar_get(glm_nc, "NS") #The last useful index
  elev <- ncvar_get(glm_nc, "z" )
  temp <- ncvar_get(glm_nc, "temp")
  time <- get_time(glm_nc)
  if (reference=='surface') {
    elev_surf = get_surface_height(file)
  } else {
    elevs_out = z_out
  }
  nc_close(glm_nc)
  
  max_i <- max(tallest_layer)
  # rows are layers, columns are time..  
  if (length(dim(elev))==2){
    elev	<-	elev[1:max_i, ] 
    temp 	<-	temp[1:max_i, ]
  } else {
    if (dim(elev)==0){stop('empty nc file')}
    else {
      elev	<-	elev[1:max_i]
      temp 	<-	temp[1:max_i]
    }
  }
  
  

  num_step	<-	length(time)
  num_dep	<-  length(z_out)
  
  temp_out <- matrix(nrow=num_step,ncol=num_dep) # pre-populated w/ NAs
  if (is.null(nrow(temp))){ # handle single depth layer of model
    for (tme in 1:num_step){
      if (reference == 'surface') elevs_out <- elev_surf[tme, 2] - z_out
      temp_out[tme, ] <- depth_resample(elevs=elev[tme], temps=temp[tme], elevs_out)
    }
  } else { 
    for (tme in 1:num_step){
      if (reference == 'surface') elevs_out <- elev_surf[tme, 2] - z_out
      temp_out[tme, ] <- depth_resample(elevs=elev[, tme], temps=temp[, tme], elevs_out)
    }
  }
  glm_temp <- data.frame(time)
  glm_temp <- cbind(glm_temp,temp_out)
  frameNms	<-	letters[seq( from = 1, to = num_dep )]
  frameNms[1] <- "DateTime"
  
  for (z in 1:num_dep){
    out_head <- ifelse(reference=='surface', 'wtr_', 'elv_')
    frameNms[z+1]  <- paste(c(out_head,as.character(z_out[z])),collapse="")
  }
  names(glm_temp)<- frameNms

  return(glm_temp)
}


depth_resample <- function(elevs, temps, elevs_out){
  #strip out NAs
  rmv_i <- temps>=1e30 | elevs>=1e30
  elevs <- elevs[!rmv_i]
  temps <- temps[!rmv_i]
  num_z <- length(elevs)
  layer_mids <- c(elevs[1]/2, elevs[1:num_z-1] + diff(elevs)/2)
  temps_re <- c(temps[1], temps, tail(temps,1))
  elevs_re <- c(0, layer_mids, tail(elevs, 1))
  
  temps <- approx(x = elevs_re, y = temps_re, xout = elevs_out)$y
  return(temps)
}

