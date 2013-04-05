lakeName<-	"Mendota"
fileName<-	paste("/Volumes/projects/WiLMA/GLM/GLM run/sim/",lakeName,"/output.nc",sep = "")
#install.packages("ncdf")
source("~/Documents/R/WiLMA-R/GLMnetCDF.R", echo=TRUE)

plotRange	<- c(as.Date('1995-01-01'),as.Date('2005-01-01'))

runLTER <- TRUE
if (runLTER){
	GLM		<- getPivotLTER('ME',lyrDz=0.1)
	figName <- "_temps_LTER"}

if(!runLTER) {GLM		<- resampleGLM(fileName,lyrDz=0.1)
	GLM$Time <- seq(as.Date('1980-01-01'),as.Date('2012-06-30'),length.out=length(GLM$Time))
	figName <- "_temps_GLM"
}





lvls	<- seq(0,30)
figW  <- 8
figH  <- 3.5
lM    <-.95
bM    <-.55
rM    <-.15
tM    <- 0.25
fRes  <- 200
fontN <- 11
xL    <- plotRange
yL    <- c(0,25)
cMap  <- rev(rainbow(length(lvls),s=1,v=1,start=0,end=4/6))

output = paste(lakeName,figName,".png", sep = "")
png(output, width=figW, height=figH, units="in",res=fRes)
par(mai=c(bM,lM,rM,tM),usr=c(xL[1],xL[2],yL[1],yL[2]))

#rect(0, levels[-length(levels)], 1, levels[-1L], col = col, border = NA)
filledContour(GLM$Time,GLM$Elevation,GLM$Temperature, col = cMap,
	levels=lvls,xaxs = "i",plot.title = title(ylab = "Elevation from bottom (m)"),
	xlim=xL, ylim=yL, xaxp = c(xL[1],xL[2],50))
dev.off()