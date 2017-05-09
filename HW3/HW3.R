library("NbClust")
library(cluster)
library(factoextra)
library(fpc)

cart = read.csv("Sessions.csv", stringsAsFactors = FALSE)
dim(cart)
str(cart)
head(cart)




set.seed(2)
x = matrix(rnorm(50*2), ncol = 2)
x[1:25,1] = x[1:25,1] + 3
x[1:25,2] = x[1:25,2] -4

for(i in 4:8){
  km.out = kmeans(x, i, nstart = 20)
  km.out$betweenss/km.out$totss
}


#totss: The total sum of squares.
#withinss: Vector of within-cluster sum of squares, one component per cluster.
#tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss).
#betweenss:The between-cluster sum of squares, i.e. totss-tot.withinss.

#Maximize between cluster variation.
#Minimize within cluster variation
# Maximize: betweenss/totss

set.seed(1)
cart.withinss = numeric()
cart.ssratio = numeric()
for(i in 3:25){
  cart.out = kmeans(cart, i, nstart = 20)
  cart.withinss = c(cart.withinss, cart.out$tot.withinss)
}
plot(cart.withinss)


# nbCluster
res.nb = NbClust(cart, distance = "euclidean", min.nc = 4, max.nc = 50, method = "complete" , index = "gap")
res.nb
clust = fviz_nbclust(cart, kmeans, method = c("silhouette", "wss", "gap_stat"), k.max = 5)
fviz_cluster(cart.out, cart)


#pam
pamk.best = pamk(cart)
plot(pam(d,pamk.best$nc))
pamk.best$nc


gap_stat <- clusGap(cart, FUN = kmeans, nstart = 25,K.max = 20, B = 50)
plot(gap_stat, frame = FALSE, xlab = "Number of clusters k")
abline(v = 3, lty = 2)


cart.out[cart$Prod_B==1 & cart$Search ==1 & cart$Home == 1 & cart$Purchase == 0 & cart$Cart == 0 & cart$Prod_A == 0 & cart$Prod_C ==0, ]
cart.out[cart$Prod_C ==1 & cart$Products == 1 & cart$Home == 0, ]