bank = read.csv("BankData.csv")
names(bank)
str(bank)

bank = data.frame(bank)
library(tree)

# Testing on the entire set
tree.bank = tree(pep ~., bank)
summary(tree.bank)
plot(tree.bank)
text(tree.bank)
tree.bank


#Testiing using train/test set
set.seed(1)
train = sample(1:nrow(bank), 0.7*nrow(bank))
test = -train

bank.train = bank[test,]
bank.test = bank[test,]  

tree.bank.train = tree(pep ~.,bank, subset = train)
bank.test.predict = predict(tree.bank.train, bank.test, type = "class")
table(bank.test.predict, bank.test$pep)


#perform cross validations
set.seed(1)
cv.bank = cv.tree(tree.bank.train, FUN = prune.misclass)
names(cv.bank)
par(mfrow = c(1,2))
plot(cv.bank$size, cv.bank$dev, type = "b", xlab = "# of Terminal Nodes", ylab = "Cross Validated Errors")
plot(cv.bank$k, cv.bank$dev, type = "b", xlab = "Cost complexity pruning value", ylab = "Cross Validated Errors")

prune.bank = prune.misclass(tree.bank.train, best = 9)
plot(prune.bank)
text(prune.bank)

bank.test.predict = predict(prune.bank, bank.test, type = 'class')
table(bank.test.predict, bank.test$pep)
