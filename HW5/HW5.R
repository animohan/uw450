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
svmfit = svm(CLASS ~., data = veh, kernel = "linear", cost = 10, scale = TRUE)
plot(svmfit, veh, f0~f1)
