# example for writing all Lake Analyzer files from GLM output

source('../Source/GLM-LA.R')
source('../Source/GLMnetCDF.R')
source('../Source/GLMnml.R')

# 1) create data frame for GLM water temperature on a uniform grid:
lyrDz <-  0.25  # grid vertical thickness for resampling
GLMfolder <-  '../Data/'
GLMfile <-  'output.nc'
NMLfile	<-	'glm.nml'
GLMnc  <- getGLMnc(fileName=GLMfile,folder=GLMfolder)
nml	<-	getNML(folder=GLMfolder,fileName=NMLfile)

lke	<-	getLke()	# gets a default list of lke parameters
mxDep	<-	getMaxDepth(nml)
lkName	<-	getLakeName(nml)

lke	<-	setLKE(lke,'totalDep',mxDep)
writeLKE(lke,lkName)		# write the .lke file

lvl	<-	getLVL(GLMnc,nml)
writeLVL(lvl,lkName)		# write the .lvl file

bth	<-	getBTH(nml)
writeBTH(bth,lkName)		# write the .bth file

depths	<-	seq(0,mxDep,1)
wtr	<-	getWTR(GLMnc,depths)
writeWTR(wtr,lkName)		# write the .wtr file

wnd	<-	getWND(GLMnc)
writeWND(wnd,lkName)		# write the .wtr file

