\name{classify}
\alias{classify}
\title{
    classify new data
}
\description{
    classify new data
}
\usage{
classify(...)
}

\arguments{
  \item{\dots}{ Argument that corresponds the class}
}

\value{
    class vector
}

\author{
Yohei Sato
}

\seealso{
  \code{\link{classify.UpliftTreeNode}}
}
\examples{
\dontrun{
treat.train <- treat[train.index]
uplift.tree <- buildUpliftTree(y.train, treat.train, x.train)
sapply(1:nrow(x.test), function(i) classify(uplift.tree, x.test[i, ]))
}
}

