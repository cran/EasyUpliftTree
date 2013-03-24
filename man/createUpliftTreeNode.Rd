\name{createUpliftTreeNode}
\alias{createUpliftTreeNode}
\title{
create UpliftTreeNode
}
\description{
create UpliftTreeNode
}
\usage{
createUpliftTreeNode(split.key = -1, split.value = NULL, l.branch = NULL, r.branch = NULL, data = NULL, score = NULL, node.type = "top")
}
\arguments{
  \item{split.key}{
    split key
}
  \item{split.value}{
    split value
}
  \item{l.branch}{
    UpliftTreeNode of left branch
}
  \item{r.branch}{
    UpliftTreeNode of right branch
}
  \item{data}{
    data
}
  \item{score}{
    score
}
  \item{node.type}{
    top or L or R
}
}

\value{
    A UpliftTreeNode
}

\author{
Yohei Sato
}

\seealso{
  \code{\link{buildUpliftTree}},\code{\link{createUpliftTreeNodeData}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (split.key = -1, split.value = NULL, l.branch = NULL, 
    r.branch = NULL, data = NULL, score = NULL, node.type = "top") 
{
    node <- list(split.key = split.key, split.value = split.value, 
        l.branch = l.branch, r.branch = r.branch, data = data, 
        score = score, node.type = node.type)
    class(node) <- c("UpliftTreeNode", "list")
    node
  }
}
