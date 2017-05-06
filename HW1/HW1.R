wine = read.csv("RedWhiteWine.csv", stringsAsFactors =  FALSE)
wine$Class = as.factor(wine$Class)
#wine = read.arff("RedWhiteWine.arff")
summary(wine$quality)
plot(wine$quality)


# wine$taste = ifelse(wine$quality < 6, "bad","good")
# wine$taste[wine$quality == 6] = "normal"
# wine$taste = as.factor(wine$taste)



set.seed(1)
train = sample(nrow(wine),0.6*nrow(wine))
test = -train
winetrain = wine[train,]
winetest = wine[test,]
dim(winetrain)
dim(winetest)

require(rpart)
names(wine)
# tree.wine = rpart(Class ~. -quality-volatile.acidity-fixed.acidity-citric.acid
#                   -residual.sugar-chlorides-free.sulfur.dioxide-total.sulfur.dioxide
#                   -density-pH-sulphates-alcohol-Class, wine, subset = train)

tree.wine = rpart(Class ~., wine, subset = train)
summary(tree.wine)

require(rpart.plot)
rpart.plot(tree.wine)

predict.WineClass = predict(tree.wine, winetest, type = "class")
winetable = table(predict.WineClass, winetest$Class)
winetable
acc = sum(diag(winetable))/sum(winetable) 
acc

predict.WineClass = predict(tree.wine, wine, type = "class")
winetable = table(predict.WineClass, wine$Class)
acc = sum(diag(winetable))/sum(winetable) 
acc


tree.wine = rpart(Class ~. -quality-pH-alcohol-density-residual.sugar-sulphates-citric.acid
                    -free.sulfur.dioxide-fixed.acidity-volatile.acidity-chlorides, 
                    wine, subset = train)
summary(tree.wine)

require(rpart.plot)
rpart.plot(tree.wine)

predict.WineTestClass = predict(tree.wine, winetest, type = "class")
winetable = table(predict.WineTestClass, winetest$Class)
winetable
acc = sum(diag(winetable))/sum(winetable) 
acc

library(pROC)
wine.ROC = roc(as.numeric(winetest$Class), as.numeric(predict.WineTestClass))
plot(wine.ROC)

auc.ROC = auc(wine.ROC)
auc.ROC

predict.WineClass = predict(tree.wine, wine, type = "class")
winetable = table(predict.WineClass, wine$Class)
acc = sum(diag(winetable))/sum(winetable) 
acc


wine.ROC = roc(as.numeric(wine$Class), as.numeric(predict.WineClass))
plot(wine.ROC)

auc.ROC = auc(wine.ROC)
auc.ROC
