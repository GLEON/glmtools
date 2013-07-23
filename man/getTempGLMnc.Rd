\name{getTempGLMnc}
\alias{getTempGLMnc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{gets wtr temperature observations at depths from GLM netCDF object}
\description{This function gets wtr temperature observations at depths from GLM netCDF object}
\usage{
getTempGLMnc(GLMnc,lyrDz)
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
Luke A. Winslow
}

\keyword{rGLM}
