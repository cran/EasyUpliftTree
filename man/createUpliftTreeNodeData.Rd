\name{createUpliftTreeNodeData}
\alias{createUpliftTreeNodeData}
\title{
    create UpliftTreeNodeData
}
\description{
    create UpliftTreeNodeData
}
\usage{
createUpliftTreeNodeData(y, treat, x)
}
\arguments{
  \item{y}{
    response vector
}
  \item{treat}{
    treat vector
}
  \item{x}{
    variables
}
}

\value{
    UpliftTreeNodeData
}

\author{
Yohei Sato
}

\seealso{
  \code{\link{buildUpliftTree}},\code{\link{createUpliftTreeNode}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (y, treat, x) 
{
    df <- data.frame(y = y, treat = treat, x = x, split = 0)
    class(df) <- c("UpliftTreeNodeData", "data.frame")
    df
  }
}
