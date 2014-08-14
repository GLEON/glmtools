get_yaxis <- function(z_out){
  
  

  lim <- c(max(z_out), 0)
  rng <- lim[1]-lim[2]

  if (rng < 1){
    spc <- .25
  } else if (rng < 2){
    spc <- .5
  } else if (rng < 5){
    spc <- 1
  } else if (rng < 10){
    spc <- 2
  } else {
    spc <- 5
  }
  ticks <- seq(0, lim[1]+spc, spc)
  yaxis <- list('lim'=lim, 'ticks'=ticks)
  return(yaxis) 
}

color_key <- function(levels, colors, subs, ps){
  # plotting to a 1 x 1 space
  if (!all(subs %in% levels)) stop('selected values must be included in levels')
  
  spc_pol_rat <- 0.1 # ratio between spaces and bars
  num_poly <- length(subs)
  num_spc <- num_poly - 1
  total_height <- num_poly + spc_pol_rat * num_spc
  
  poly_h <- 1/total_height
  spc_h <- 0.1 * poly_h
  
  for (i in 1:num_poly){
    col <- colors[levels==subs[i]]
    b <- (i-1)*(poly_h+spc_h)
    t <- b+poly_h
    m <- mean(c(b,t))
    polygon(c(0,.8,.8,0),c(b,b,t,t),col = col)
    text(.8,m,as.character(subs[i]), ps = ps, pos= 4)
  }
}

get_xaxis <- function(dates){
  

  start_time = min(dates) #earliest date
  end_time = max(dates) #latest date
  
  vis_time = pretty(dates) # pretty vector to specify tick mark location 
  sec.end_time = as.numeric(end_time) # show time as seconds
  sec.start_time = as.numeric(start_time) # show time as seconds
  tt = sec.end_time - sec.start_time # time range of data frame; used to specify time axis
  
  # specify x axis format based upon time range of data 
  time_form = c()
  if(tt < 1.1*60) { # if time is less than 1 hr units are seconds
    time_form <- "%S"
  } else if (tt < 1.1*60*60) { # if time is less than 1.1 hours units are min:sec
    time_form <- "%M:%S"
  } else if (tt < 60*60*24*2) {# if time is less than 2 days units are hour:min
    time_form <- "%H:%M"
  } else if (tt < 60*60*24*7*8.9) {# if time is less than 2 months units are ex. Jul 25 10:15
    time_form <- "%d %b %H:%M"
  } else {    
    time_form <- "%b %Y"
  }
  
  # specify x axis labels based upon time range of data 
  x_lab = c()
  if(tt < 1.1*60) { # if time is less than 1 minutes units are seconds
    x_lab  <- "Seconds"
  } else if (tt < 1.1*60*60) { # if time is less than 1.1 hours units are min:sec
    x_lab <- "Minutes"
  } else if (tt < 60*60*24*2) {# if time is less than 2 days units are hour:min
    x_lab <- "Hours"
  } else if (tt < 60*60*24*7) { # if time is less than 7 days units are Jul 25 10:15
    x_lab <- " "
  } else if (tt < 60*60*24*7*8.9) {# if time is less than 2 months units are ex. Jul 25 
    x_lab <- " "
  } else if (tt < 60*60*24*7*4.4*12) { # if time is less than 12 months units are Jun, Jul, Aug  
    x_lab <- " "
  } else if (tt < 60*60*24*7*4.4*12*1.1){ # if time is more than 12.1 years units are Jul 2013
    x_lab <- " "
  }
  
  return(list('time_form' = time_form, 'x_lab' = x_lab, 'lim' = c(start_time, end_time), 'vis_time' = vis_time))
}