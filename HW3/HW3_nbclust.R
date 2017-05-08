library("NbClust")

cart = read.csv("Sessions.csv", stringsAsFactors = FALSE)
dim(cart)
str(cart)
head(cart)

res.nb = NbClust(cart, distance = "euclidean", min.nc = 4, max.nc = 10, method = "complete" , index = "gap")
res.nb

res.nb$Best.partition
res.nb$Best.nc

res.nb = NbClust(cart, distance = "euclidean", min.nc = 4, max.nc = 10, method = "complete" , index = "all")
