\name{interactionScore}
\alias{interactionScore}
\title{
    interaction score
}
\description{
    calc interaction score
}
\usage{
interactionScore(...)
}

\arguments{
  \item{\dots}{
    Argument that corresponds the class
}
}

\value{
    interaction score vector
}

\author{
Yohei Sato
}

\seealso{
  \code{\link{interactionScore.UpliftTreeNodeData}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (...) 
{
    UseMethod("interactionScore")
  }
}

