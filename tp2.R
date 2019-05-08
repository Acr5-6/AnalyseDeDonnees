data1=read.table("C:\\Users\\cr_al\\Documents\\Scolaire\\POLYTECH\\4A\\OPTION - InSI\\JIN83E - Analyse de données\\tp2\\data1TP2.txt", header = TRUE)
print(data1)

## A installer dans le terminal 
#install.packages("plot3D") 
library("plot3D")

##### QUESTION 1 #####

par(mfrow=c(2,2))

nuage3d <- points3D(data1$Stature, data1$Poids, data1$Taille, pch = 16, main = "Nuage de points en 3D avec la matrice A",
       xlab="Stature",
       ylab="Poids",
       zlab="Taille")


##### QUESTION 2 #####

c1 <- c(data1[,1])
c2 <- c(data1[,2])
c3 <- c(data1[,3])

a <- cbind(c1, c2, c3)

b <- cbind(c1, c2, c3)

k <- ncol(b)
n <- nrow(b)

## Moyenne de chaque colonne
b_mean <- matrix(data=1, nrow=n) %*% cbind(mean(c1),mean(c2),mean(c3)) 

## Difference entre les valeurs et les moyennes
b <- b - b_mean

## Matrice de covariance
v <- (n-1)^-1*t(b) %*% b


##### QUESTION 3 #####

## Extraction des valeurs et vecteurs propres
v_propres = eigen(v)
v_propres$values # valeurs propres
v_propres$vectors # vecteurs propres


##### QUESTION 4 #####

## Axes principaux (dans l'ordre)
#Sature - Poinds - Taille

##### QUESTION 5 #####

## Tableau c = b * vect_propres
c <- b %*% v_propres$vectors


princomp(a)$scores


##### QUESTION 6 #####

#points3D(princomp(a)$scores[,1], princomp(a)$scores[,2], princomp(a)$scores[,3], pch = 16, main = "Nuage de points en 3D avec princomp")

# reglin_stat_pds <- line(c[,1], c[,2])
# reglin_stat_taille <- line(c[,1], c[,3])
# 
# lm_stat_pds <- lm(data1$Stature~data1$Poids, data=data1)
# lm_stat_taille <- lm(data1$Stature~data1$Taille, data=data1)
# 
# lm_stat <- lm(data1$Stature~data1$Poids + data1$Taille, data=data1)

#nuage3d$points3D(lm_stat, type="l")
# plot(reglin_stat_pds)

val1 <- v_propres$values[1]
vect1 <- v_propres$vectors[,1]
points3D( (vect1[2]*data1$Poids + vect1[3]*data1$Taille)/(val1-vect1[1]),
                  data1$Poids,
                  data1$Taille)

f_y=vect1[2]/(val1-vect1[1])
f_z=vect1[3]/(val1-vect1[1])

points3D( f_y*data1$Poids + f_z*data1$Taille, data1$Poids, data1$Taille,
                  pch = 16)


##### QUESTION 7 #####

plot(princomp(a)$scores[,1],princomp(a)$scores[,2])



##### QUESTION 8 #####


