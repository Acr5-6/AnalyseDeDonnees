#### Generation de points ####

#generation de x, y uniformes sur [0,1]
x1 = runif(100,0,1)
y1 = runif(100,0,1)

#variance = ecart_typeÂ²
#generation de x, y gaussiennes variance=1, moy_x=4/moy_y=0
x2 = rnorm(100,4,sqrt(1))
y2 = rnorm(100,0,sqrt(1))

#generation de x, y gaussiennes varriance=2, moy_x=0.5/moy_y=6
x3 = rnorm(100,0.5,sqrt(2))
y3 = rnorm(100,6,sqrt(2))

m1 <- cbind(x1,y1)
m2 <- cbind(x2,y2)
m3 <- cbind(x3,y3)

m <- rbind(m1,m2,m3)

# plot(m[,1],m[,2], main="Ensemble de points", xlab="x", ylab="y")

par(mfrow=c(2,2))

plot(-3:9, -3:9, type = "n" , main="nuage de points")
points(x1,y1, col="blue", pch=19)
points(x2,y2, col="green", pch=19)
points(x3,y3, col="red", pch=19)



#### Classification non supervisee ####

class_asc_hie <- function(data, k) {
    count = 1
    n = nrow(data)
    mat_C = diag(n)
    distance = matrix(numeric(0), n, n)
    
    #calcul des distances
    for (i in 1:n) {
        for (j in 1:n) {
            # if(i!=j)
            distance[i, j] = sqrt( (data[i, 1] - data[j, 1])** 2 + (data[i, 2] - data[j, 2])**2 )
        }
    }
    
    while (count <= (n - k) ) {
        dmin = distance[2, 1]
        pi = 2
        pj = 1
        
        #recuperation du minimum
        for (i in 2:(n - count + 1)) {
            for (j in 1:(i - 1)) {
                # if (distance[i, j] < dmin && i!=j) {
                if (distance[i, j] < dmin) {
                    dmin = distance[i, j]
                    pi = i
                    pj = j
                }
            }
        }
        
        mat_C[pj, ] = mat_C[pj, ] + mat_C[pi, ]
        
        data_tmp = data[mat_C[pj,]==1,]
        g_tmp = c( mean(data_tmp[1]), mean(data_tmp[2]))
        
        for (i in 1:(n - count + 1)) {
            data_i_tmp = data[mat_C[i,]==1,]
            g_i_tmp = c( mean(data_i_tmp[1]), mean(data_i_tmp[2]) )
            dist_ = sqrt( (g_tmp[1]-g_i_tmp[1])**2 + (g_tmp[2]-g_i_tmp[2])**2 )
            if (i<pj){
                distance[pj,i]=dist_
            }
            else {
                distance[i,pj]=dist_
            }
        }
        mat_C=mat_C[-pi,]
        # data_tmp = data[mat_C[pj,]==1,]
        # g_tmp = c( mean(data_tmp[1]), mean(data_tmp[2]))
        distance=distance[-pi,]
        distance=distance[,-pi]
        count = count + 1
    }
    return ( t(mat_C) )
}

C = class_asc_hie(m,3)

z = matrix(0,300,1)

for (i in 1:nrow(m)){
    if(C[i,1]==1){
        z[i]=1
    }
    if(C[i,2]==1){
        z[i]=2
    }
    if(C[i,3]==1){
        z[i]=3
    }
    
}

plot(m, col = c("red", "blue", "green")[z], pch=19, main="nauge par rapport aux classes")

  
#### Verification ####
D <- dist(m, method = "euclidean")

AscHierarchique <- hclust(D, method = "complete") #complete ou ward

# plot(AscHierarchique, cex = 0.6, hang = -1)

cluster = cutree(AscHierarchique, 3)

plot(m, col = c("red", "blue", "green")[cluster], pch=19,main="Vérification")


#### Question 3 ####
fac1 <- factor(cluster)

# # le cutree
# fac1 <- factor(cutree(h1, h=5))

# le calcul du barycentre de chaque groupe :
barycentre2 <- function(dfxy, fac){
    wt = rep(1, length(fac))
    # fonction dérivée de la fonction s.class (ade4)
    f1 <- function(cl) {
        n <- length(cl)
        cl <- as.factor(cl)
        x <- matrix(0, n, length(levels(cl)))
        x[(1:n) + n * (unclass(cl) - 1)] <- 1
        dimnames(x) <- list(names(cl), levels(cl))
        data.frame(x)
    }
    dfxy <- data.frame(dfxy)
    dfdistri <- f1(fac) * wt
    w1 <- unlist(lapply(dfdistri, sum))
    dfdistri <- t(t(dfdistri)/w1)
    coo2 <- t(dfdistri) %*% as.matrix(dfxy)
    rownames(coo2) <- levels(fac)
    coo2
}

ba1 <- barycentre2(m, fac1)

# la distance entre le barycentre du nuage de points et les barycentres des classes :
db1 <- (t(ba1)-colMeans(m))^2
db1 <- colSums(db1)

# multiplier par le nombre d'individus par groupe :
db1 <- db1*table(fac1)

# l'inertie inter (% de l'inertie totale) :
iner.inter <- sum(db1)/sum((t(m)-colMeans(m))^2)

# inertie intra :
iner.intra <- 1-iner.inter

# ce qui correspond à :
# install.packages("ade4")
require(ade4)
acp1 <- dudi.pca(m, scale=FALSE, scannf=FALSE, nf=ncol(m))

# install.packages("dplyr")
# library(dplyr)
# be1 <- between(acp1, fac1, scannf=F, nf=2)
# 
# be1$ratio
