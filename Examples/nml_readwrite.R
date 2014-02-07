# example for writing all Lake Analyzer files from GLM output
require('rGLM')

# 1) open existing gml.nml file
GLMfolder	<-	'../resources/'
NMLfile	<-	'glm.nml'
nml	<-	read.nml(folder=GLMfolder,fileName=NMLfile)

# 2) pretty print the nml list 
pretty.nml(nml) 

# 3) get existing value from nml for Kw parameter
get.nml(nml,argName='Kw')

# 4) change Kw value to a new one
nml	<-	set.nml(nml,argName='Kw',argVal=0.4)

# 5) view the change in the nml
get.nml(nml,argName='Kw')

# 6) change a list of params
nml	<-	set.nml(nml,argList=list('Kw'=0.4,'outflow_factor'=0.9))

# 7) try to set a param that doesn't exist (will error). Un-comment this to try:
#nml	<-	set.nml(nml,argName='donkey',argVal=23)

# 8) pretty print the nml list 
pretty.nml(nml)

# 9) write it back to glm.nml so it can be used in GLM
write.nml(nml,folder=GLMfolder,fileName='glm2.nml')