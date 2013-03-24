\name{buildUpliftTree}
\alias{buildUpliftTree}
\title{
Build Uplift Tree
}
\description{
Build Uplift Tree
}
\usage{
buildUpliftTree(y, treat, x, node.type = "top")
}
\arguments{
  \item{y}{
    response vector
}
  \item{treat}{
    treat vector
}
  \item{x}{
    Variables
}
  \item{node.type}{
    L or R
}
}

\value{
    A UpliftTreeNode
}

\author{
Yohei Sato
}

\seealso{
  \code{\link{toDataFrame.UpliftTreeNode}},\code{\link{classify.UpliftTreeNode}}
}
\examples{
\dontrun{
library(survival)
data(colon)
sample.data <- na.omit(colon[colon$rx != "Lev" & colon$etype == 2, ])
treat <- ifelse(sample.data$rx == "Lev+5FU", 1, 0)
y <- ifelse(sample.data$status == 0, 1, 0)
x <- sample.data[, c(4:9, 11:14)]

x$v1 <- factor(x$sex)
x$v2 <- factor(x$obstruct)
x$v3 <- factor(x$perfor)
x$v4 <- factor(x$adhere)
x$v5 <- factor(x$differ)
x$v6 <- factor(x$extent)
x$v7 <- factor(x$surg)
x$v8 <- factor(x$node4)

index <- 1:nrow(x)
train.index <- index[(index\%\%2 == 0)]
test.index <- index[index\%\%2 != 0]

y.train <- y[train.index]
x.train <- x[train.index, ]
treat.train <- treat[train.index]

y.test <- y[test.index]
x.test <- x[test.index, ]
treat.test <- treat[test.index]

uplift.tree <- buildUpliftTree(y.train, treat.train, x.train)
print(uplift.tree)
}
}

