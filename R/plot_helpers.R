gen_default_fig <- function(file_name, fig_w = 4, fig_h = 2, ps = 12, l.mar = 0.35,
                            r.mar = 0, t.mar = 0.05, b.mar = 0.2, res = 200){
  png(filename = file_name,
      width = fig_w, height = fig_h, units = "in", res = res)
  
  
  par(mai=c(b.mar,0, t.mar, 0),omi=c(0, l.mar, 0, r.mar),ps = ps, mgp = c(1.4,.3,0))
  
  
}

plot_one2one <- function(x, y, ...){
  
  min_val <- min(c(x,y), na.rm = TRUE)
  max_val <- max(c(x,y), na.rm = TRUE)
  
  plot(x, y, xlim = c(min_val, max_val), ylim = c(min_val, max_val), ...)
  abline(0, 1, lty = 2, col = 'black')
}


axis_layout <- function(xaxis, yaxis){
  # x axis
  axis(side = 1, labels=format(xaxis$vis_time, xaxis$time_form), at = xaxis$vis_time, tck = -0.01, pos = yaxis$lim[1])
  axis(side = 3, labels=NA, at = xaxis$lim, tck = 0)
  axis(side = 2, at = yaxis$ticks, tck = -0.01, pos = xaxis$lim[1])
  ol_par <- par()$mgp
  par(mgp=c(0,1.5,0))
  axis(side = 2, at = mean(yaxis$lim), tck = 0,  labels=yaxis$title)
  par(mgp=ol_par)
  axis(side = 4, labels=NA, at = yaxis$lim, tck = 0)
}

get_yaxis <- function(data, title){

  lim <- c(min(data), max(data)*1.1)

  
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
  
  start_tck <- floor(min(lim)/spc) * spc
  ticks <- seq(start_tck, max(lim) + spc, spc)
  yaxis <- list('lim'=lim, 'ticks'=ticks, 'title' = title)
  return(yaxis) 
}

get_yaxis_2D <- function(z_out, reference){
  
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

color_key <- function(levels, colors, subs, ps, col_label = 'Temperature (\u00B0C)'){
  # add feau plot
  plot(NA, xlim = c(0,1),
       ylim=c(0,1),
       xlab="", ylab="",
       frame=FALSE,axes=F,xaxs="i",yaxs="i")
  old_mgp <- par()$mgp
  old_mai <- par()$mai
  par(mai=c(old_mai[1],0, old_mai[3], .2), mgp = c(0,0,0))
  axis(side = 4, at = 0.5, tck = NA, labels= col_label, lwd = 0.0)#(\xB0 C)
  spc_pol_rat <- 0.2 # ratio between spaces and bars
  
  p_start <- 0.1
  p_wid <- 0.35
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
    polygon(c(p_start,p_wid,p_wid,p_start),c(b,b,t,t),col = col, border = NA)
    text(p_wid-.05,m,as.character(subs[i]), ps = ps, pos= 4)
  }
  par(mai = old_mai, mgp = old_mgp)
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

.simple_layout <- function(nrow = 1){
  panels  <- matrix(c(1),nrow=nrow)
  layout(panels)
}

colbar_layout <- function(nrow = 1){
	# ensures all colorbar plots use same x scaling for divs
	mx <- matrix(c(rep(1,5),2),nrow=1)
	panels <- mx
	if (nrow > 1){
		for (i in 2:nrow){
			panels <- rbind(panels,mx+(i-1)*2)
		}
	}
	
	layout(panels)
	
}

valid_fig_path <- function(fig_path){
  
  if (!is.character(fig_path) & !identical(fig_path, FALSE)){
    stop(fig_path,' is not a valid input for fig_path')
  }
  
}
plot_layout <- function(xaxis=NULL, yaxis=NULL, add, data = NA){
	
	if (!add){
		panels <- colbar_layout()
	}
	
	
	plot(data, xlim = xaxis$lim,
			 ylim=yaxis$lim,
			 xlab=xaxis$x_lab, ylab=' ',
			 frame=FALSE,axes=F,xaxs="i",yaxs="i")
	
	
}

.stacked_layout <- function(is_heatmap, num_divs){
  if(num_divs == 1 & !is_heatmap) return()
  
  if(is_heatmap){
    colbar_layout(num_divs)
  } else {
    .simple_layout(num_divs)
  }

}