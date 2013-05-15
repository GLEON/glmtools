# example comparison with field data for GLM

source('../Source/GLMnetCDF.R')
minZ <-	1	# minimum depth for use in regression
# open field data
fileN	<-	'../Data/fieldData.tsv'
sampleVals <- read.table(fileN,header=TRUE)

dates	<-	as.POSIXct(sampleVals$sampledate)
depths	<-	sampleVals$depth
wtrObs	<-	sampleVals$wtemp

# get rid of depths < minZ
rmvI	<-	depths < minZ
dates	<-	dates[!rmvI]
depths	<-	depths[!rmvI]
wtrObs	<-	wtrObs[!rmvI]

unDates	<-	unique(dates)

lyrDz <-  0.25  # grid vertical thickness for resampling
GLMfolder <-  '../Data/'
GLMfile <-  'output.nc'
GLMnc  <- getGLMnc(fileName=GLMfile,folder=GLMfolder)
GLM	<-  resampleGLM(GLMnc,lyrDz=lyrDz)

# - THIS SHOULD BUILD A VECTOR OF COMPARE VALS, FOR LM()
# - plot

cnt = 0
wtrMod	<-	wtrObs*NA
for (j in 1:length(unDates)){
	dt	<-	unDates[j]
	useI	<-	which(dates==dt)
	wtrT	<-	subsampleGLM(GLM, dt, depths[useI])
	if (!is.na(wtrT[1])){
		wtrMod[useI]	<-	wtrT
		cnt = cnt+1
	}
}
plot(wtrObs,wtrMod)
LM	<-	lm(wtrMod~wtrObs)
m	<-	summary(LM)
m$sigma


