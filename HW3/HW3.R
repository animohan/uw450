cart = read.csv("Sessions.csv", stringsAsFactors = FALSE)
dim(cart)
str(cart)
head(cart)

km.cart = kmeans(cart,4,nstart = 20)
km.cart
plot(cart[c("Home","Products")], col = km.cart$cluster)
points(km.cart$centers[,c("Home","Products")], col = 1:4, pch  = 23, cex = 3)


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


km.out$withinss

km.out = kmeans(x, 4, nstart = 20)
km.out
km.out$tot.withinss
km.out$betweenss
km.out$betweenss/km.out$totss
km.out$ifault

for(i in 4:8){
  cart.out = kmeans(cart, i, nstart = 20)
  print(cart.out$betweenss/cart.out$totss)
}


cart.out = cbind(cart, km.out$cluster)

temp.cart = cart.out[cart$Prod_B==1 & cart$Search ==1 & cart$Home == 1,]
sum(temp.cart$Prod_A)/sum(temp.cart$Home)
sum(temp.cart$Prod_C)/sum(temp.cart$Home)

cart.out[cart$Prod_B==1 & cart$Search ==1 & cart$Home == 1 & cart$Purchase == 0 & cart$Cart == 0 & cart$Prod_A == 0 & cart$Prod_C ==0, ]
cart.out[cart$Prod_C ==1 & cart$Products == 1 & cart$Home == 0, ]