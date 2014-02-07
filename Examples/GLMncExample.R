# example usage of GLMnetCDF
require('rGLM')



# 1) create data frame for GLM water temperature on a uniform grid:
#lyrDz <-  0.25  # grid vertical thickness for resampling
out.depths <- c(0,.5,1.5,5,10)
GLMfolder <-  '../resources/'
GLMfile <-  'output.nc'
GLMnc  <- getGLMnc(fileName=GLMfile,folder=GLMfolder)
GLMwtr <-  getTempGLMnc(GLMnc=GLMnc,ref='surface',z.out=out.depths)

# 2) write data to a text file
outFolder <-  '../Examples/'
write.wtr(wtr=GLMwtr,lakeName='testLake',folder=outFolder)