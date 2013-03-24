\name{interactionScore.UpliftTreeNodeData}
\alias{interactionScore.UpliftTreeNodeData}
\title{
    interaction score
}
\description{
    Calc interaction score
}
\usage{
interactionScore.UpliftTreeNodeData(data)
}
\arguments{
  \item{data}{
    UpliftTreeNodeData
  }
}

\value{
  score
}

\author{
Yohei Sato
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data) 
{
    fit <- lm(y ~ treat + split + treat * split, data = data)
    if (is.na(fit$coefficients[4])) {
        score <- 0
    }
    else {
        score <- summary(fit)$coefficient[4, 3]
    }
    ifelse(score > 1, score, 0)
  }
}

