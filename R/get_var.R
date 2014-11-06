
#'@export
get_var <-  function(file, reference = 'bottom', z_out = NULL, t_out = NULL, var_name, ...){
  
  
  if (is.null(z_out)){
    mx_lyrs <- 20
    z_lyrs <- seq(0,mx_lyrs-1) # default layers
    z_out = (max(get_surface_height(file)[, 2], na.rm = TRUE)/tail(z_lyrs,1)) * z_lyrs
  }
  glm_nc <- get_glm_nc(file)
  
  tallest_layer <- get.var.ncdf(glm_nc, "NS") #The last useful index
  elev <- get.var.ncdf(glm_nc, "z" )
  temp <- get.var.ncdf(glm_nc, var_name)
  time <- get_time(glm_nc)
  
  if (length(dim(temp)) == 1){
    # is 1D
    variable_df <- data.frame('DateTime' = time, 'variable' = temp)
    return(variable_df)
  }
  
  if (reference!='bottom' & reference!='surface'){
    stop('reference input must be either "surface" or "bottom"')
  }
  
  
  
  close_glm_nc(glm_nc)
  
  if (reference=='surface') {
    elev_surf = get_surface_height(file)
  } else {
    elevs_out = z_out
  }
  
  max_i <- max(tallest_layer)
  # rows are layers, columns are time..  
  if (length(dim(elev))==2){
    elev  <-  elev[1:max_i, ] 
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
  
  for (tme in 1:num_step){
    if (reference == 'surface') elevs_out <- elev_surf[tme, 2] - z_out
    if (is.null(nrow(temp))){ # handle single depth layer of model
      temp_out[tme, ] <- resample_depth(elevs=elev[tme], temps=temp[tme], elevs_out)
    } else {
      temp_out[tme, ] <- resample_depth(elevs=elev[, tme], temps=temp[, tme], elevs_out)
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
  
  glm_var <- resample_sim(glm_temp, t_out, ...)
  
  return(glm_var)
}



resample_depth <- function(elevs, temps, elevs_out){
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

