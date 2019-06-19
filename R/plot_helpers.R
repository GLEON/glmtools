#' @importFrom grDevices png
gen_default_fig <- function(filename=FALSE, width = 4, height, ps = 11, res = 200, units = "in",
                            mai = c(0.2,0,0.05,0),
                            omi = c(0.1, 0.5, 0, 0), 
                            mgp = c(1.4,.3,0),
                            num_divs = 1, ...){
  
  if ((is.character(filename))){
    valid_fig_path(filename)
    if (missing(height)){
      height = 2*num_divs
    }
    png(filename, width = width, height = height, units = units, res = res)
  }
  par(mai = mai,omi = omi, ps = ps, mgp = mgp, ...)
}

#' @importFrom graphics abline
plot_one2one <- function(x, y, ...){
  
  min_val <- min(c(x,y), na.rm = TRUE)
  max_val <- max(c(x,y), na.rm = TRUE)
  
  plot(x, y, xlim = c(min_val, max_val), ylim = c(min_val, max_val), ...)
  abline(0, 1, lty = 2, col = 'black')
}

#' @importFrom graphics axis par
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

get_yaxis <- function(data, title, lim = NULL){
  if (is.null(lim)){
    mn <- min(data, na.rm = TRUE)
    mx <- max(data, na.rm = TRUE)
    axBuff <- diff(c(mn, mx))*0.1
    lim <- c(mn-axBuff, mx+axBuff)
  }

  ticks <- pretty(data)
  yaxis <- list('lim'=lim, 'ticks'=ticks, 'title' = title)
  return(yaxis) 
}

get_yaxis_2D <- function(z_out, reference, prefix=NULL, suffix=NULL){
  
  if (length(z_out) < 2){stop('z_out must be larger than 1 for heatmap plots')}
  
  if (reference == 'surface'){
    lim <- c(max(z_out), 0)
    title <- paste(prefix,' Depth (m) ',suffix, sep='')
  } else {
    lim <- c(0, max(z_out))
    title <- paste(prefix,' Elevation (m) ',suffix, sep='')
  }
  
  yaxis <- get_yaxis(data = z_out, title = title, lim = lim)
  return(yaxis) 
}

#' @importFrom graphics polygon text
color_key <- function(levels, colors, subs, cex = 0.75, col_label){
  # add feau plot
  plot(NA, xlim = c(0,1),
       ylim=c(0,1),
       xlab="", ylab="",
       frame=FALSE,axes=FALSE,xaxs="i",yaxs="i")
  old_mgp <- par()$mgp
  old_mai <- par()$mai
  par(mai=c(old_mai[1],0, old_mai[3], 0), mgp = c(-1,-1,0))
  axis(side = 4, at = 0.5, tck = NA, labels= col_label, lwd = 0.0)#(\xB0 C)
  spc_pol_rat <- 0.2 # ratio between spaces and bars
  
  p_start <- 0.1
  p_wid <- 0.55

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
    m <- mean(c(b,t))
    polygon(c(p_start,p_wid,p_wid,p_start),c(b,b,t,t),col = col, border = NA)
    text(p_wid+0.025,m,as.character(subs[i]), cex = cex, adj = c(0.5, 1), srt = 90)
  }
  par(mai = old_mai, mgp = old_mgp)
}

get_xaxis <- function(dates){
  

  start_time = min(dates) #earliest date
  end_time = max(dates) #latest date
  
  vis_time = c(start_time-86400, pretty(dates), end_time+86400) # pretty vector to specify tick mark location 
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

#' @importFrom graphics layout 
.simple_layout <- function(nrow = 1){
  panels  <- matrix(seq_len(nrow),nrow=nrow)
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
			 frame=FALSE,axes=FALSE,xaxs="i",yaxs="i")
	
	
}

.stacked_layout <- function(is_heatmap, num_divs){
  if(num_divs == 1 & !is_heatmap) return()
  
  if(is_heatmap){
    colbar_layout(num_divs)
  } else {
    .simple_layout(num_divs)
  }

}

#' @importFrom graphics plot 
.plot_null <- function(){
  plot(NA, ylim=c(0,1),xlim=c(0,1), axes=FALSE,ylab="",xlab="")
}

.unit_label <- function(file, var_name){
  longname <- sim_var_longname(file, var_name) 
  titlename <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", longname, perl=TRUE)
  units <- sim_var_units(file, var_name)
  unit_label <- paste0(titlename, " (", units, ")")
  return(unit_label)
}

