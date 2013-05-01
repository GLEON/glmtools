# example comparison with field data for GLM

source('../Source/GLMnetCDF.R')
minZ <-	1	# minimum depth for use in regression
# open field data
fileN	<-	'../Data/fieldData.tsv'
sampleVals <- read.table(fileN,header=TRUE)

dates	<-	as.POSIXct(sampleVals$sampledate)
depths	<-	sampleVals$depth
wtrObs	<-	sampleVals$wtemp

unDates	<-	unique(dates)

lyrDz <-  0.25  # grid vertical thickness for resampling
GLMfolder <-  '../Data/'
GLMfile <-  'output.nc'
GLMnc  <- getGLMnc(fileName=GLMfile,folder=GLMfolder)
GLM	<-  resampleGLM(GLMnc,lyrDz=lyrDz)

# - plot

for (j in length(unDates)){
	dt	<-	unDates[j]
	useI	<-	which(dates==dt)
	wtrMod	<-	subsampleGLM(GLM, dt, depths[useI])
	# if != NA, add comparison to plot..
	# plot(wtrMod,wtrObs)
}



