\name{classify.UpliftTreeNode}
\alias{classify.UpliftTreeNode}
\title{
    classify new data
}
\description{
    classify new data
}
\usage{
classify.UpliftTreeNode(tree, row)
}

\arguments{
  \item{tree}{
    UpliftTreeNode
}
  \item{row}{
    new data
}
}

\value{
    class vector
}

\author{
Yohei Sato
}

\examples{
\dontrun{
treat.train <- treat[train.index]
uplift.tree <- buildUpliftTree(y.train, treat.train, x.train)
sapply(1:nrow(x.test), function(i) classify(uplift.tree, x.test[i, ]))
}
}

