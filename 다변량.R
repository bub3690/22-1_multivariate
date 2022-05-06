###############################
#          Homework 3         #
###############################

## 3.
install.packages("readbitmap")
library(readbitmap)

setwd("D:/project/22-1_multivariate")
img = array(NA,c(100,600,600))
for (i in 1:100){
  img[i,,] = read.bitmap(paste("./data/img/img",i,sep=""))
}
dim(img[1,,]) # the dimension of the first image

# visualize the first 600 x 600 grayscale image
par(mfrow=c(1,1))
par(mar=c(.5, .5, .5, .5))
image(t(apply(img[1,,],2,rev)), col=gray((0:229)/229), axes=F)

# select n random samples of 12 × 12 image patches
n = 100000
r1 = sample(1:100,n,replace=T)
r2 = sample(1:589,n,replace=T)#가로 시작점
r3 = sample(1:589,n,replace=T)#세로 시작점
table(r1)
# vectorize the selected patches
X = matrix(NA,n,144)
for (i in 1:n){
  X[i,] = as.vector(img[r1[i],r2[i]:(r2[i]+11),r3[i]:(r3[i]+11)])
}
dim(X)
mean(X)
# compute the sample mean and the sample covariance
mu = mean(X)
S = cov(X)

dim(S)
# conduct eigen decomposition using the sample covariance
# and make U including the first d eigenvectors
U = princomp(cov(X))$loadings
U$loadings



# image reconstruction using PCA by gluing 50^2 disjoint 12 × 12 image patches
Z = matrix(NA,600,600)
for (i in 12*(0:49)+1){
  for (j in 12*(0:49)+1){
    Z[i:(i+11),j:(j+11)] = as.vector(mu + U[,1:1]%*%t(U[,1:1])%*%(as.vector(img[1,i:(i+11),j:(j+11)])-mu))
  }
}
Z

image(Z, col=gray((0:229)/229), axes=F)

