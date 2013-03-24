source("../R/easy_uplift_tree.R")

## データの用意
##
## ・実際は医療データだが、ちょっと加工して以下のようなものとして考えてみる
##   ・y==1     : 購入
##   ・treat==1 : DM 送信
##   ・x        : 何か属性など様々な変数

library(survival)
data(colon)
sample.data <- na.omit(colon[colon$rx!="Lev" & colon$etype==2,])

treat <- ifelse(sample.data$rx=="Lev+5FU", 1, 0)
y <- ifelse(sample.data$status==0, 1, 0)
x <- sample.data[,c(4:9,11:14)]

x$v1 <- factor(x$sex)
x$v2 <- factor(x$obstruct)
x$v3 <- factor(x$perfor)
x$v4 <- factor(x$adhere)
x$v5 <- factor(x$differ)
x$v6 <- factor(x$extent)
x$v7 <- factor(x$surg)
x$v8 <- factor(x$node4)

## 学習用のデータとテスト用データの作成
index <- 1:nrow(x)
train.index <- index[(index%%2==0)]
test.index <- index[index%%2!=0]

y.train <- y[train.index]
x.train <- x[train.index,]
treat.train <- treat[train.index]

y.test <- y[test.index]
x.test <- x[test.index,]
treat.test <- treat[test.index]

## Uplift Treeの作成
uplift.tree <- buildUpliftTree(y.train,treat.train,x.train)

## 結果確認
print(uplift.tree)
uplift.df <- toDataFrame(uplift.tree)
mean(uplift.df[uplift.df$node.type=="R" & uplift.df$treat==1,"y"])
mean(uplift.df[uplift.df$node.type=="L" & uplift.df$treat==1,"y"])

## テストデータをRグループとLグループに分類
x.test$node.type <- sapply(1:nrow(x.test), function(i) classify(uplift.tree,x.test[i,]))

## 以下のような条件でROIを計算してみる
## 
## 1人当たりのコスト : 100円
## 1個当たりの売上   : 200円


## 全体に介入
cost.a <- length(y.test[treat.test==1]) * 100
amount.a <- sum(y.test) * 200
(roi.a <- amount.a / cost.a)

## Rのグループに介入
## Rグループのtreat=0は介入ありでも結果が変わらず、Lグループのtreat==1は、購入無しと厳しめな仮定をおく
cost.r <- length(y.test[x.test$node.type=="R"]) * 100
amount.r <- sum(y.test[!(x.test$node.type=="L" & treat.test==1)]) * 200
(roi.r <- amount.r / cost.r)

## Lのグループに介入
## Lグループのtreat=0は介入があっても結果が変わらず、Rグループのtreat==1は、購入無しと厳しめな仮定をおく
cost.l <- length(y.test[x.test$node.type=="L"]) * 100
amount.l <- sum(y.test[!(x.test$node.type=="R" & treat.test==1)]) * 200
(roi.l <- amount.l / cost.l)


