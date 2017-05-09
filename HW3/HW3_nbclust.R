library("NbClust")
library(cluster)
library(factoextra)
library(fpc)

cart = read.csv("Sessions.csv", stringsAsFactors = FALSE)
dim(cart)
str(cart)
head(cart)
cart.out = kmeans(cart, 4, nstart = 20)

res.nb = NbClust(cart, distance = "euclidean", min.nc = 2, max.nc = 50, method = "complete" , index = "gap")
res.nb
clust = fviz_nbclust(cart, kmeans, method = c("silhouette", "wss", "gap_stat"), k.max = 5)
fviz_cluster(cart.out, cart)


#pam
pamk.best = pamk(cart)
plot(pam(d,pamk.best$nc))
pamk.best$nc

res.nb$Best.partition
res.nb$Best.nc

res.nb = NbClust(cart, distance = "euclidean", min.nc = 4, max.nc = 50, method = "complete" , index = "all")
re.