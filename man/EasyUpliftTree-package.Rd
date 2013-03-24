\name{EasyUpliftTree-package}
\alias{EasyUpliftTree-package}
\alias{EasyUpliftTree}
\docType{package}
\title{
Easy Uplift Tree Model for R
}
\description{
Easy Uplift Tree Model for R
}
\details{
\tabular{ll}{
Package: \tab EasyUpliftTree\cr
Type: \tab Package\cr
Version: \tab 0.0.2\cr
Date: \tab 2013-02-24\cr
License: \tab BSD\cr
}

}
\author{
Yohei Sato, Issei Kurahashi
Maintainer: Yohei Sato <yokkun@tkul.jp>
}
\references{
http://stochasticsolutions.com/sbut.html
}
\keyword{ package }
\seealso{
  \code{\link{buildUpliftTree}},\code{\link{toDataFrame}},\code{\link{classify}}
}
\examples{
\dontrun{
    uplift.tree <- buildUpliftTree(y.train, treat.train, x.train)
    print(uplift.tree)
    uplift.df <- toDataFrame(uplift.tree)
    x.test$node.type <- sapply(1:nrow(x.test), function(i) classify(uplift.tree, x.test[i, ]))
}
}
