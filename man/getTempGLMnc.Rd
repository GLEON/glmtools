\name{getTempGLMnc}
\alias{getTempGLMnc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{gets wtr temperature observations at depths or elevations from GLM netCDF object}
\description{This function gets wtr temperature observations at elevations or depths from GLM netCDF object}
\usage{
getTempGLMnc(GLMnc,lyrDz,ref,z.out)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{GLMnc}{GLM netCDF object}
  \item{lyrDz}{layer thickness for interpolation}
  \item{ref}{reference location for z.out vector. Can be surface or bottom.}
  \item{z.out}{vector for z output. Positive downward if ref==surface, positive upward if ref==bottom}
}
\value{
GLM data.frame
}
\author{
Luke A. Winslow & Jordan S. Read
}

\keyword{rGLM}
