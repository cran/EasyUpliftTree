## generic functions

divideSet <- function(...){
  UseMethod("divideSet")
}

divideSet.default <- function(...){
}

interactionScore <- function(...){
  UseMethod("interactionScore")
}

interactionScore.default <- function(...){
}

toDataFrame <- function(...){
  UseMethod("toDataFrame")
}

toDataFrame.default <- function(...){
}

classify <- function(...){
  UseMethod("classify")
}

classify.default <- function(...){
}

#print <- function(...){
#  UseMethod("print")
#}

createUpliftTreeNodeData <- function(y,treat,x){
  ##
  ## create UpliftTreeNodeData object
  ##
  ## Argument
  ##  y : response vecter.
  ##
  ##
  df <- data.frame(y=y, treat=treat, x=x, split=0)
  class(df) <- c("UpliftTreeNodeData", "data.frame")
  df
}


divideSet.UpliftTreeNodeData <- function(data, value){
  ##
  ## divide data set by value
  ##
  ## Argument:
  ##   data: UpliftTreeNodeData
  ##   value: numeric or character
  ##
  if (class(value) == "numeric"){
    splitFunc <- function(x) { x >= value}
  } else {
    splitFunc <- function(x) { x == value}
  }

  data$split <- ifelse(splitFunc(data$x), 1, 0)
  data
}

interactionScore.UpliftTreeNodeData <- function(data){
  ##
  ## calcurate interaction score
  ##
  ## Argument:
  ##   data: UpliftTreeNodeData
  ##

  ##fit <- glm(y~treat+split+treat*split, family=binomial, data=data)
  fit <- lm(y~treat+split+treat*split, data=data)
  if(is.na(fit$coefficients[4])){
    score <- 0
  } else {
    score <- summary(fit)$coefficient[4,3]
  }
  ifelse(score > 1, score, 0)
}

createUpliftTreeNode <- function(split.key=-1, split.value=NULL,l.branch=NULL, r.branch=NULL, data=NULL,score=NULL, node.type="top"){
  ##
  ## create Uplift Tree Node
  ##
  ## Argument:
  ##   split.key: character
  ##   split.value: numeric or character
  ##   l.brahcn: UpliftTreeNode or NULL
  ##   r.branch: UpliftTreeNode or NULL
  ##   data: UpliftTreeNodeData or NULL
  ##
  node <- list(split.key=split.key,
               split.value=split.value,
               l.branch=l.branch,
               r.branch=r.branch,
               data=data,
               score=score,
               node.type=node.type)
  class(node) <- c("UpliftTreeNode", "list")
  node
}

buildUpliftTree <- function(y, treat, x, node.type="top"){
  ##
  ## build Uplift Tree
  ##
  ## Argument:
  ##   y: response vector
  ##   treat: treated indicator
  ##   x: data.frame
  ##   node.type: character. - top or R or L
  ##
  data <- x; data$y <- y; data$treat <- treat
  if (nrow(x) == 1) return(createUpliftTreeNode(data=data))
  
  key.score.list <-
    lapply(names(x),
           function(key){
             u.data <- createUpliftTreeNodeData(y=y,treat=treat,x=x[,key])
             if( class(x[,key]) == "numeric" ){
               values <- quantile(x[,key], probs=(1:10)/10)
             } else {
               values <- unique(x[,key])
             }
             score.list <-
               lapply(values,
                      function(value){
                        my.data <- divideSet(u.data,value)
                        score <- interactionScore(my.data)
                        list(score=score, key=key, value=value, data=my.data)
                      })
             score.list[order(sapply(score.list, function(l) l[[1]]),decreasing=T)][[1]]
           })
  best.score.list <- key.score.list[order(sapply(key.score.list, function(l) l[[1]]),decreasing=T)][[1]]

  best.key <- best.score.list$key
  best.value <- best.score.list$value
  best.score <- best.score.list$score
  best.split <- best.score.list$data$split
  if (best.score > 0){
    r.branch <- buildUpliftTree(y=y[best.split==1],treat=treat[best.split==1],x=x[best.split==1,], node.type="R")
    l.branch <- buildUpliftTree(y=y[best.split!=1],treat=treat[best.split!=1],x=x[best.split!=1,], node.type="L")
    
    return(
           createUpliftTreeNode(split.key=best.key,
                                split.value=best.value,
                                l.branch=l.branch,
                                r.branch=r.branch,
                                score=best.score,
                                node.type=node.type)
           )
  } else {
    return(
           createUpliftTreeNode(data=data,node.type=node.type)
           )
  }
}

print.UpliftTreeNode <- function(tree, indent=""){
  ##
  ## print tree node
  ##
  ## Argument:
  ##   tree: UpliftTreeNode
  ##   indent: character
  ##
  if (! is.null(tree$data)){
    data <- tree$data
    cat(paste(mean(data[data$treat==1,"y"]),"\n"))
  }
  else {
    cat( paste(tree$split.key,":", tree$split.value,"?\n") )
    cat( paste(indent, "R->"))
    print(tree$r.branch, paste(indent," "))
    cat( paste(indent, "L->"))
    print(tree$l.branch, paste(indent," "))
  }
}

toDataFrame.UpliftTreeNode <- function(tree){
  ##
  ## to data.frame
  ##
  ## Argument:
  ##   tree: UpliftTreeNode
  ##
  if (is.null(tree$data)){
    r.df <- toDataFrame(tree$r.branch)
    l.df <- toDataFrame(tree$l.branch)
    df <- rbind(r.df,l.df)
  } else {
    df <- tree$data
    df$node.type <- tree$node.type
  }
  df
}

classify.UpliftTreeNode <- function(tree, row){
  ##
  ## cassify by uplift tree node
  ##
  ## Argument:
  ##   tree: UpliftTreeNode
  ##   row: 1 row data.frame
  ##
  if (! is.null(tree$data)){
    return(tree$node.type)
  } else {
    v <- row[tree$split.key]
    branch <- NULL
    if( class(v) == "numeric" ){
      if( v >= tree$split.value ){
        branch <- tree$r.branch
      } else {
        branch <- tree$l.branch
      }
    } else {
      v <- as.character(v)
      split.value <- as.character(tree$split.value)
      if( v == split.value ){
        branch <- tree$r.branch
      } else {
        branch <- tree$l.branch
      }
    }
    return(classify(branch,row))
  }
}
