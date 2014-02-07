# example for writing all Lake Analyzer files from GLM output
require('rGLM')

# 1) create data frame for GLM water temperature on a uniform grid:
lyrDz	<-  0.5  # grid vertical thickness for resampling
lkName	<-	"testLake"
GLMfolder	<-	'../resources/'
out.folder	<-	'.'
GLMfile	<-	'output.nc'
NMLfile	<-	'glm.nml'
GLMnc	<-	getGLMnc(fileName=GLMfile,folder=GLMfolder)
nml	<-	read.nml(folder=GLMfolder,fileName=NMLfile)
lke	<-	init.lke()	# gets a default list of lke parameters
pretty.nml(nml) # print the nml list 

bth	<-	get.bth(nml)
mxDep	<-	max(bth[,1])

write.bth(bth,lkName,folder=out.folder)		# write the .bth file

lke	<-	set.lke(lke,'totalDep',mxDep)
write.lke(lke,lkName,folder=out.folder)		# write the .lke file

lvl	<-	get.lvl(GLMnc,nml)
write.lvl(lvl,lkName,folder=out.folder)		# write the .lvl file

depths	<-	seq(0,mxDep,by=lyrDz)
wtr	<-	get.wtr(GLMnc,ref='surface',z.out=depths)
write.wtr(wtr,lkName,folder=out.folder)		# write the .wtr file

wnd	<-	get.wnd(GLMnc)
write.wnd(wnd,lkName,folder=out.folder)		# write the .wtr file

