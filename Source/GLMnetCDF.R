# methods for handling GLM's netCDF output
filledContour <-	function(x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
    ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
    levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
    col = color.palette(length(levels) - 1), plot.title, plot.axes, 
    key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
    axes = TRUE, frame.plot = axes, ...) 
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    on.exit(par(par.orig))
    w <- (3 + mar.orig[2L]) * par("csi") * 2.54
    layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
    par(las = las)
    mar <- mar.orig
    mar[4L] <- mar[2L]
    mar[2L] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
        yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col, border= NA)
    if (missing(key.axes)) {
        if (axes) 
            axis(4)
    }
    else key.axes
    box()
    if (!missing(key.title)) 
        key.title
    mar <- mar.orig
    mar[4L] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1L || ncol(z) <= 1L) 
        stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
        storage.mode(z) <- "double"
    .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), 
        col = col))
    if (missing(plot.axes)) {
        if (axes) {
            title(main = "", xlab = "", ylab = "")
            Axis(x, side = 1)
            Axis(y, side = 2)
        }
    }
    else plot.axes
    if (frame.plot) 
        box()
    if (missing(plot.title)) 
        title(...)
    else plot.title
    invisible()
}
resampleGLM	<-	function(fileName,lyrDz=0.25){
	require("ncdf")
	GLM.nc	<- 	open.ncdf(fileName)
	elev	<- 	get.var.ncdf(GLM.nc, "z" )
	dates	<- 	get.var.ncdf(GLM.nc, "time")
	E		<- 	get.var.ncdf(GLM.nc, "evap")
	wtr		<- 	get.var.ncdf(GLM.nc, "temp")
	rmvI	<- 	which(wtr>=1e30 | elev>=1e30)
	elev[rmvI]	<- NA
	mxElv	<-	max(elev,na.rm = TRUE)+lyrDz
	mnElv	<-	min(elev,na.rm = TRUE)-lyrDz

	elevOut	<-	seq(mnElv,mxElv,lyrDz)
	numStep <-	ncol(wtr)
	numDep	<-	length(elevOut)

	wtrOut	<-	matrix(nrow=numStep,ncol=numDep)

	for (tme in 1:numStep){
		useI	<- which(wtr[,tme]<1e30 & elev[,tme]<1e30)
		useI	<- which(wtr[,tme]<1e30 & elev[,tme]<1e30)
		x		<- elev[useI,tme]
		y		<- wtr[useI,tme]
		if (length(y)>0){
			ap		<-	approx(c(mnElv,x),c(y[1],y),xout=elevOut)
			wtrOut[tme,1:length(ap$y)]	<- ap$y}
	}
	GLM	<- list(Time=dates,Elevation=elevOut,Temperature=wtrOut)
	return(GLM)
}

getPivotLTER	<-	function(lakeName,lyrDz=0.25){
	fileName	<-	'/Users/jread/Desktop/Science Projects/WiLMA/Validation data/LTER_wtr.csv'
	LTER	<- read.csv(fileName)
	useI	<-	which(LTER["lakeid"]==lakeName)
	LTER	<- LTER[useI,]
	mnElv	<- min(LTER["depth"],na.rm=TRUE)
	mxElv	<- min(LTER["depth"],na.rm=TRUE)
	wtr		<-	LTER[["wtemp"]]
	elev	<-	LTER[["depth"]]-mnElv
	dates	<-	as.Date(LTER[["sampledate"]])
	elevOut	<-	seq(min(elev),max(elev),lyrDz)
	unDates	<-	unique(dates)
	numStep <-	length(unDates)
	numDep	<-	length(elevOut)
	wtrOut	<- matrix(nrow=numStep,ncol=numDep)
	
	for (tme in 1:numStep){
		useI	<- which(dates==unDates[tme])
		x		<- elev[useI]	# sort?
		y		<- wtr[useI]
		x		<-	rev(x[!is.na(y)])
		y		<- y[!is.na(y)]
		if (length(y)>0){
			ap		<-	approx(c(mxElv,x),c(y[length(y)],y),xout=elevOut)
			wtrOut[tme,1:length(ap$y)]	<- ap$y}
	}
	
	LTER	<- list(Time=unDates,Elevation=elevOut,Temperature=wtrOut)
	return(LTER)
}