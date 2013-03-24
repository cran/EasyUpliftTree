\name{divideSet.UpliftTreeNodeData}
\alias{divideSet.UpliftTreeNodeData}
\title{
    Divide the data for Uplift Tree Node Data
}
\description{
    Divide the data for Uplift Tree Node Data
}
\usage{
divideSet.UpliftTreeNodeData(data, value)
}
\arguments{
  \item{data}{
    UpliftTreeNodeData
}
  \item{value}{
    numeric or character
}
}

\value{
  UpliftTreeNodeData
}

\author{
Yohei Sato
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, value) 
{
    if (class(value) == "numeric") {
        splitFunc <- function(x) {
            x >= value
        }
    }
    else {
        splitFunc <- function(x) {
            x == value
        }
    }
    data$split <- ifelse(splitFunc(data$x), 1, 0)
    data
  }
}

