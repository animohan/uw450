veh = read.csv("veh-prime.csv", stringsAsFactors = FALSE)
dim(veh)
str(veh)
names(veh)
veh$CLASS[veh$CLASS == 'car'] = 1
veh$CLASS[veh$CLASS == 'noncar'] = -1
str(veh)
veh$CLASS = as.factor(veh$CLASS)
str(veh)


library(e1071)

# Sample the data:
set.seed(1)
train = sample(nrow(veh),0.8*nrow(veh))
test = -train

veh.train = veh[train, ]
veh.test = veh[test, ]


set.seed(1)
tuned.svm = tune(svm, CLASS ~., data = veh.train, kernel = "radial", ranges = list(cost = c(0.001,0.01,0.1,1,5,10,100)))
summary(tuned.svm)
best.model = tuned.svm$best.model
best.model
summary(best.model)
#Training error
svm.pred = predict(best.model, veh.train)
table(predict = svm.pred, truth = veh.train$CLASS ) 


#Test Error
svm.test.pred = predict(best.model, veh.test)
table(predict = svm.test.pred, truth = veh.test$CLASS )

svmfit = svm(CLASS ~., data = veh, kernel = "radial", cost = 10, scale = TRUE)
plot(svmfit, veh, f0~f7)
