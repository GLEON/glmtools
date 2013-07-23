\name{resampleGLM}
\alias{resampleGLM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{gets wtr temperature observations at depths from GLM netCDF object}
\description{This function gets wtr temperature observations at depths from GLM netCDF object}
\usage{
resampleGLM(GLMnc,lyrDz)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{GLMnc}{GLM netCDF object}
  \item{lyrDz}{layer thickness for interpolation}
}
\value{
GLM data.frame
}
\author{
Jordan S. Read
}

\keyword{rGLM}
