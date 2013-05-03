# example usage of GLMnetCDF

source('../Source/GLMnetCDF.R')
# 1) create data frame for GLM water temperature on a uniform grid:
lyrDz <-  0.25  # grid vertical thickness for resampling
GLMfolder <-  '../Data/'
GLMfile <-  'output.nc4'
GLMnc  <- getGLMnc(fileName=GLMfile,folder=GLMfolder)
GLMwtr <-  resampleGLM(GLMnc,lyrDz=lyrDz)

# 2) write data to a text file
outFile <- 'GLMout.txt'
outFolder <-  '../Examples/'
writeGLM(GLMwtr,fileName=outFile,folder=outFolder)

# 3) plot a heatmap and save
figureName <- 'exampleFig'
plotGLM(GLMwtr,figName=figureName,folder=outFolder)
