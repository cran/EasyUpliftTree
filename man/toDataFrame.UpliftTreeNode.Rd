\name{toDataFrame.UpliftTreeNode}
\alias{toDataFrame.UpliftTreeNode}
\title{
    cast to data.frame for Uplift Tree
}
\description{
    cast to data.frame for Uplift Tree
}
\usage{
toDataFrame.UpliftTreeNode(tree)
}
\arguments{
  \item{tree}{
    Uplift Tree
    }
}

\value{
    A data.frame
}

\author{
Yohei Sato
}

\seealso{
  \code{\link{buildUpliftTree}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (tree) 
{
    if (is.null(tree$data)) {
        r.df <- toDataFrame(tree$r.branch)
        l.df <- toDataFrame(tree$l.branch)
        df <- rbind(r.df, l.df)
    }
    else {
        df <- tree$data
        df$node.type <- tree$node.type
    }
    df
  }
}

