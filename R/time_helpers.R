get_UTM_offset <- function(){
  # local date comparison for daylight savings. Uses this to find UTM offset, which will be used as tz for POSIXct
  summer <- data.frame(NH = as.POSIXct("2011-06-01 12:00:00"), SH = as.POSIXct("2011-12-01 12:00:00"))
  dst <- c(NA, FALSE, TRUE)[as.POSIXlt(c(summer[,1], summer[,2]))$isdst + 2]
  
  use_i <- which(!dst)[1]
  UTM <- data.frame(NH = as.POSIXct("2011-06-01 12:00:00",tz = "GMT"), SH = as.POSIXct("2011-12-01 12:00:00", tz = "GMT"))
  
  if (length(use_i) == 0 | is.na(use_i)){ return("")}
  UTM_dif <- as.numeric(summer[,use_i] - UTM[,use_i]) # in hours
  sym <- ifelse(UTM_dif < 0, '-','+')
  tz <- paste0("Etc/GMT",sym, as.character(UTM_dif))
  return(tz)
}

coerce_date <- function(dates){
  # for non-POSIX dates
  if (!"POSIXct" %in% class(dates) || attr(dates,'tzone') == ""){
    # strip off POSIXct zone and replace w/ GMT offset
    dates <- as.POSIXct(as.character(dates), tz = get_UTM_offset())
  }

  return(dates)
}


time_precision <- function(t_out, precision){
  un_cnt <- length(unique(t_out))
  if (!(precision %in% c('secs', 'mins','hours', 'days'))){
    stop(paste(precision,'not supported for time matching'))
  }
  t_out <- round(t_out, precision)
  
  if (un_cnt > length(unique(t_out))){
    warning(paste(precision,'precision resulted in duplicate date values'))
  }
  t_out <- as.POSIXct(t_out)
  return(t_out)
}

df_interp <- function(df, t_out){
  t_srt <- sort(t_out) # get it in order for approx
  n_dep <- ncol(df) - 1 
  df_out <- matrix(ncol = n_dep, nrow = length(t_srt))
  for (i in 1:n_dep){
    if (sum(!is.na(df[,(i+1)])) >= 2){
      df_out[, i] <- approx(x = as.numeric(df$DateTime), 
                            y = df[,(i+1)], 
                            xout = as.numeric(t_srt), 
                            method = 'linear')$y
    } else {
      df_out[, i] <- NA
    }
    
  }
  
  row_na= function(x){all(is.na(x))}
  na_i <- apply(df_out, MARGIN = 1, FUN = row_na)
  df_out <- data.frame(t_srt, df_out)
  names(df_out) <- names(df)
  return(df_out[!na_i, ])
}
trunc_time <- function(df, start_date, stop_date){
  
  srt_dt <- sort(unique(df[, 1]))
  
  if (attr(srt_dt, 'tzone') != attr(start_date, 'tzone')){
    warning('Input data and model output have different time zones')
  }
  
  df_step <- diff(as.numeric(srt_dt[1:2]))
 	t_unit <- get_prec_time(df_step)
  unit_s <- get_sec_unit(t_unit)
  
  # truncate *inclusive*
  idx_out <- df$DateTime >= trunc(start_date, t_unit) & df$DateTime <= trunc(stop_date+unit_s, t_unit)
  df_out <- df[idx_out, ]
  return(df_out)
}

get_prec_time <- function(time_secs){
	
	if(is.na(time_secs)){
		return('days')
	}
	
  if (time_secs >= 3600 & time_secs < 86400){
    prec = 'hours'
  } else if (time_secs >= 60 & time_secs < 3600){
    prec = 'minutes'
  } else if (time_secs < 60){
    prec = 'seconds'
  } else {
    prec = 'days'
  }
  return(prec)
}

get_sec_unit <- function(unit){
  # gotta be a POSIXct method for this...
  if (unit == 'hours'){
    secs = 3600
  } else if (unit == 'minutes'){
    secs = 60
  } else if (unit == 'seconds'){
    secs = 1
  } else if (unit == 'days'){
    secs = 86400
  } else {stop(paste(unit, 'not recognized'))}
  return(secs)
}