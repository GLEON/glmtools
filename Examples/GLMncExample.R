# example usage of GLMnetCDF

source('../Source/GLMnetCDF.R')
# 1) create data frame for GLM water temperature on a uniform grid:
lyrDz <-  0.25  # grid vertical thickness for resampling
GLMfolder <-  '../Data/'
GLMfile <-  'output.nc'
GLM <-  resampleGLM(fileName=GLMfile,folder=GLMfolder,lyrDz=lyrDz)

# 2) write data to a text file
outFile <- 'GLMout.txt'
outFolder <-  '../Examples/'
writeGLM(GLM,fileName=outFile,folder=outFolder)

# 3) plot a heatmap and save
figureName <- 'exampleFig'
plotGLM(GLM,figName=figureName,folder=outFolder)
