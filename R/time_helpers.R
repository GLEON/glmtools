get_UTM_offset <- function(){
  # local date comparison for daylight savings. Uses this to find UTM offset, which will be used as tz for POSIXct
  summer <- data.frame(NH = as.POSIXct("2011-06-01 12:00:00"), SH = as.POSIXct("2011-12-01 12:00:00"))
  dst <- c(NA, FALSE, TRUE)[as.POSIXlt(c(summer[,1], summer[,2]))$isdst + 2]
  
  use_i <- which(!dst)[1]
  UTM <- data.frame(NH = as.POSIXct("2011-06-01 12:00:00",tz = "GMT"), SH = as.POSIXct("2011-12-01 12:00:00", tz = "GMT"))
  
  if (length(use_i) == 0){ return("")}
  UTM_dif <- as.numeric(summer[,use_i] - UTM[,use_i]) # in hours
  sym <- ifelse(UTM_dif < 0, '-','+')
  tz <- paste0("Etc/GMT",sym, as.character(UTM_dif))
  return(tz)
}