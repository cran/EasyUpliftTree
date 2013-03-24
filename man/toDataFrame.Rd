\name{toDataFrame}
\alias{toDataFrame}
\title{
    cast to data.frame
}
\description{
    cast to data.frame
}
\usage{
toDataFrame(...)
}
\arguments{
  \item{\dots}{
    Data into a data.frame
    }
}

\value{
A data.frame
}

\author{
Yohei Sato
}

\seealso{
  \code{\link{toDataFrame.UpliftTreeNode}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (...) 
{
    UseMethod("toDataFrame")
  }
}

