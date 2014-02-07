# example usage of GLMnetCDF
require('rGLM')
require('ncdf4')


# 1) create data frame for GLM water temperature on a uniform grid:
lyrDz <-  0.25  # grid vertical thickness for resampling
GLMfolder <-  '../resources/'
GLMfile <-  'output.nc'
GLMnc  <- getGLMnc(fileName=GLMfile,folder=GLMfolder)
GLMwtr <-  getTempGLMnc(GLMnc=GLMnc,lyrDz,ref='surface')

# 2) write data to a text file
outFolder <-  '../Examples/'
write.wtr(wtr=GLMwtr,lakeName='testLake',folder=outFolder)