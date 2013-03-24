\name{divideSet}
\alias{divideSet}
\title{
    divide the data
}
\description{
    divide the data
}
\usage{
divideSet(...)
}
\arguments{
  \item{\dots}{
    Argument that corresponds the class
}
}

\value{
  UpliftTreeNodeData
}

\author{
Yohei Sato
}

\seealso{
  \code{link{divideSet.UpliftTreeNodeData}}
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (...) 
{
    UseMethod("divideSet")
  }
}

