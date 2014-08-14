gen_default_fig <- function(file_name){
  fig_w <- 4
  fig_h <- 2.
  png(filename = file_name,
      width = fig_w, height = fig_h, units = "in", res=450)
  
  l.mar	<-	0.35
  r.mar	<-	0#v.spc
  t.mar	<-	0.05
  b.mar	<-	0.2
  
  point_size <- 12
  
  par(mai=c(b.mar,0, t.mar, 0),omi=c(0, l.mar, 0, r.mar),ps = point_size, mgp = c(1.4,.3,0))
  
  
}


get_yaxis <- function(z_out, reference){
  
  if (length(z_out) < 2){stop('z_out must be larger than 1 for heatmap plots')}
  
  if (reference == 'surface'){
    lim <- c(max(z_out), 0)
    title <- 'Depth (m)'
  } else {
    lim <- c(0, max(z_out))
    title <- 'Elevation (m)'
  }
  
  rng <- abs(lim[1]-lim[2])

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
  ticks <- seq(0, max(lim) + spc, spc)
  yaxis <- list('lim'=lim, 'ticks'=ticks, 'title' = title)
  return(yaxis) 
}

color_key <- function(levels, colors, subs, ps){
  # add feau plot
  plot(NA, xlim = c(0,1),
       ylim=c(0,1),
       xlab="", ylab="",
       frame=FALSE,axes=F,xaxs="i",yaxs="i")
  
  spc_pol_rat <- 0.2 # ratio between spaces and bars
  old_mai <- par()$mai
  par(mai=c(old_mai[1],0, old_mai[3],0))
  p_start <- 0.2
  p_wid <- 0.5
  if (missing(ps)){
    ps <- round(par()$ps*0.7)
  }
  # plotting to a 1 x 1 space
  if (!all(subs %in% levels)) stop('selected values must be included in levels')
  
  
  num_poly <- length(subs)
  num_spc <- num_poly - 1
  total_height <- num_poly + spc_pol_rat * num_spc
  
  poly_h <- 1/total_height
  spc_h <- spc_pol_rat * poly_h
  
  for (i in 1:num_poly){
    col <- colors[levels==subs[i]]
    b <- (i-1)*(poly_h+spc_h)
    t <- b+poly_h
    m <- mean(c(b,t))-0.12*(t-b) # vertical fudge factor for text
    polygon(c(p_start,p_wid,p_wid,p_start),c(b,b,t,t),col = col)
    text(p_wid,m,as.character(subs[i]), ps = ps, pos= 4)
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