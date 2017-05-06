bank = read.csv("BankData.csv")
names(bank)
str(bank)

bank = data.frame(bank)

library(ggplot2)
ggplot(bank, aes(y = pep, x = sex)) + geom_histogram() + xlab("Age") + ylab("Loan Offered") + 
  ggtitle("Relationship between Age and Loan Offered")

ggplot(bank, aes(x = sex)) +geom_bar(aes(fill = pep))

library(rpart)
library(rpart.plot)
# Testing on the entire set
tree.bank = rpart(pep ~.,data = bank, method = "class", rpart.control(cp = 0.1))
summary(tree.bank)
rpart.plot(tree.bank)
printcp(tree.bank)

#Testiing using train/test set
set.seed(1)
train = sample(1:nrow(bank), 0.7*nrow(bank))
test = -train

bank.train = bank[train,]
bank.test = bank[test,]  


library(caret)
cost = expand.grid(cp = seq(1,0,-.01))
control = trainControl(method = "cv", number = 10)
tree.bankTrain = train(pep~., data = bank.train, method = "rpart", trControl = control, tuneLength = 100)
print(tree.bankTrain)
plot(tree.bankTrain$cp, tree.bankTrain$results)

tree.bankTrain$

#Single tree
tree.bankTrain = rpart(pep~., data = bank.train, method = "class", 
                       control = rpart.control(xval = 10, cp = 0.004))
summary(tree.bankTrain)
rpart.plot(tree.bankTrain)
printcp(tree.bankTrain)

predict.bankTrain = predict(tree.bankTrain, bank.train, type="class")
acc = table(predict.bankTrain, bank.train$pep)
print(paste0("Training Accuracy:", sum(diag(acc))/sum(acc)))

predict.bankTest = predict(tree.bankTrain, bank.test, type="class")
acc = table(predict.bankTest, bank.test$pep)
print(paste0("Test Accuracy:", sum(diag(acc))/sum(acc)))

library(pROC)
bank.ROC = roc(as.numeric(bank.test$pep), as.numeric(predict.bankTest))
plot(bank.ROC)

auc.ROC = auc(bank.ROC)
auc.ROC



# Random Forests
library(randomForest)
set.seed(1)
bag.bank = randomForest(pep ~. ,data = bank, subset = train, mtry = 10, importance = TRUE)
bag.bank
predict.bankTest = predict(bag.bank, newdata = bank.test)
acc = table(predict.bankTest, bank.test$pep)
print(paste0("Test Accuracy:", sum(diag(acc))/sum(acc)))

bank.ROC = roc(as.numeric(bank.test$pep), as.numeric(predict.bankTest))
plot(bank.ROC)

auc.ROC = auc(bank.ROC)
auc.ROC

rf.bank = randomForest(pep ~. ,data = bank, subset = train, mtry = 7, importance = TRUE)
predict.bankTest = predict(rf.bank, newdata = bank.test)
acc = table(predict.bankTest, bank.test$pep)
print(paste0("Test Accuracy:", sum(diag(acc))/sum(acc)))

bank.ROC = roc(as.numeric(bank.test$pep), as.numeric(predict.bankTest))
plot(bank.ROC)

auc.ROC = auc(bank.ROC)
auc.ROC


#Tree Experiments:
cost = seq(1,0,-.1)

tree.bankTrain = rpart(pep~., data = bank.train, method = "class", 
                       control = rpart.control(min.split = 1, xval = 10, cp = cost))
summary(tree.bankTrain)
rpart.plot(tree.bankTrain)
printcp(tree.bankTrain)
predict.bankTest = predict(tree.bankTrain, bank.test, type="class")
acc = table(predict.bankTest, bank.test$pep)
print(paste0("Test Accuracy:", sum(diag(acc))/sum(acc)))
