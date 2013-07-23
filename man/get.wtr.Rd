\name{get.wtr}
\alias{get.wtr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{gets time series .wtr file from GLM nc output}
\description{This function gets time series .wtr file from GLM nc output}
\usage{
get.wtr(GLMnc,depths,lyrDz)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{GLMnc}{GLM nc object}
  \item{depths}{an array of depths}
  \item{lyrDz}{spacing for GLM nc interpolation}
}
\value{
a data.frame time series water temperature observations
}
\author{
Jordan S. Read
}

\keyword{rGLM}
