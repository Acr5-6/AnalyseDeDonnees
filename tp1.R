data1=read.table("C:\\Users\\cr_al\\Documents\\Scolaire\\POLYTECH\\4A\\OPTION - InSI\\JIN83E - Analyse de données\\tp1\\data1TP1.txt", header = TRUE)

#### QUESTION 1 ####
par(mfrow=c(2,3))

plot(data1$A,data1$Y, main="A par rapport a Y",
     xlab="l'axe des A",
     ylab="l'axe des Y")
plot(data1$B,data1$Y, main="B par rapport a Y",
     xlab="l'axe des B",
     ylab="l'axe des Y")
plot(data1$C,data1$Y, main="C par rapport a Y",
     xlab="l'axe des C",
     ylab="l'axe des Y")
plot(data1$D,data1$Y, main="D par rapport a Y",
     xlab="l'axe des D",
     ylab="l'axe des Y")
plot(data1$E,data1$Y, main="E par rapport a Y",
     xlab="l'axe des E",
     ylab="l'axe des Y")



#### QUESTION 2 ####
Coeff_r_Pearson <- function(X, Y){
 cov(X, Y, method = "pearson") / (sd(X)*sd(Y))
}

paste("coefficient de r de Pearson avec A et Y (fonction) :", Coeff_r_Pearson(data1$A,data1$Y) )
paste("coefficient de r de Pearson avec A et Y (cor) :", cor(data1$A,data1$Y) )

paste("coefficient de r de Pearson avec B et Y (fonction) :", Coeff_r_Pearson(data1$B,data1$Y) )
paste("coefficient de r de Pearson avec C et Y (fonction) :", Coeff_r_Pearson(data1$C,data1$Y) )
paste("coefficient de r de Pearson avec D et Y (fonction) :", Coeff_r_Pearson(data1$D,data1$Y) )
paste("coefficient de r de Pearson avec E et Y (fonction) :", Coeff_r_Pearson(data1$E,data1$Y) )

#En valeur absolue, La variable E a la plus petite correlation.
#Sinon c'est La variable A qui a la plus petite correlation.



#### QUESTION 3 ####
Coeff_Spearman <- function(X, Y){
  sum=0
  R_x=rank(X)
  R_y=rank(Y)
  for (i in 1:length(X) ){
    sum = sum + ( R_x[i]-R_y[i] )^2
  }
  1-(6*sum)/(length(X)^3-length(X))
}

paste("coefficient de Spearman avec A et Y (fonction) :", Coeff_Spearman(data1$A,data1$Y) )
paste("coefficient de Spearman avec A et Y (cor) :", cor(data1$A,data1$Y,method="spearman") )

paste("coefficient de Spearman avec B et Y (fonction) :", Coeff_Spearman(data1$B,data1$Y) )
paste("coefficient de Spearman avec C et Y (fonction) :", Coeff_Spearman(data1$C,data1$Y) )
paste("coefficient de Spearman avec D et Y (fonction) :", Coeff_Spearman(data1$D,data1$Y) )
paste("coefficient de Spearman avec E et Y (fonction) :", Coeff_Spearman(data1$E,data1$Y) )



#### QUESTION 4 ####
#
#
#
#


##Test de validation d'hypostheses

##Test paramétrique

#### QUESTION 5 ####
data2=read.table('C:\\Users\\cr_al\\Documents\\Scolaire\\POLYTECH\\4A\\OPTION - InSI\\JIN83E - Analyse de données\\tp1\\data2TP1.txt', header = TRUE)


#H0=l'inflation n'a pas affecte le cout de la vie.
#degre de liberte = 14
#alpha = 5%
#score < t_distribution => on accepte H.
#score > t_distribution => H fausse, l'inflation a affecte le cout de la vie.

Score_t <- function(X){
  abs( mean(X)-19 ) / ( sd(X) / sqrt(length(X)) )
}

paste('Score de t 2010-2019 : ', Score_t(data2$Marseille))

#Ici, l'inflation a affecte le cout de la vie.



#### QUESTION 6 ####

#degre de liberte = 29
#H1=dependance significative entre Marseille et Aix.
Score_t_2_var <- function(X,Y){
  abs( mean(X)-mean(Y) ) / sqrt( sd(X)^2/length(X)+sd(Y)^2/length(Y) )
}

paste('Score de t pour 2 variables: ', Score_t_2_var(data2$Marseille,data2$Aix) )
#avec alpha=5%, H0 fausse => il n'y a pas de dependance.
#Cependant avec 2%, H0 vraie => il y a une dependance.



##Test non paramétrique

#### QUESTION 7 ####

#H0=le vrai ratio est 9:3:3:1
#alpha = 5% 
#degré liberté = 4 - 1
#Khi_distribution = 7.81
#score < Khi_distribution => on accepte H.
#score > Khi_distribution => H fausse, l'inflation a affecte le cout de la vie.

m_phe <- matrix(c(9, 1528, 3, 106, 3, 117, 1, 381),nrow = 2, ncol = 4)
somme_ratio = sum(m_phe[1,])
V_th_phe = m_pho[1,]/somme_ratio * sum(m_phe[2,])
paste("les valeurs theorique pour chaque phonetique est:", V_th_phe)

Khi_deux <- function(val_obs, val_th){
  res = (val_obs - val_th)^2/val_th
  res=sum(res)
  res
}

paste("Khi_deux :", Khi_deux(m_phe[2,], V_th_phe))

#H0 fausse, 9:3:3:1 n'est pas le vrai ratio.


#### QUESTION 8 ####

#H0 : deux variables sont indépendantes   ----  alpha = 5% 
#degré liberté FORM = 3 - 1               ---- Khi_distribution FORM = 5.99
#degré liberté COLOR = 2 - 1              ---- Khi_distribution COLOR = 3.84

m_form <- matrix(c(29, 40, 18, 5, 32, 22, 46, 8, 0) ,nrow = 3, ncol = 3)
V_th_form <- matrix(nrow = 3, ncol = 3)
m_color <- matrix(c(20, 29, 12, 60, 51, 28), nrow=3 , ncol = 2)
V_th_color <- matrix(nrow=3 , ncol = 2)

for (i in 1:3){
  for (j in 1:3){
    V_th_form[i,j] = sum(m_form[i,])*sum(m_form[,j])/sum(m_form)
  }
}

for (i in 1:3){
  for (j in 1:2){
    V_th_color[i,j] = sum(m_color[i,])*sum(m_color[,j])/sum(m_color)
  }
}

#paste("Khi_deux form absent:", Khi_deux(m_form[,1], V_th_form[,1]))
#paste("Khi_deux form atypical:", Khi_deux(m_form[,2], V_th_form[,2]))
#paste("Khi_deux form typical:", Khi_deux(m_form[,3], V_th_form[,3]))
paste("Khi_deux form :", Khi_deux(m_form, V_th_form))


#paste("Khi_deux color absent:", Khi_deux(m_color[,1], V_th_color[,1]))
#paste("Khi_deux color present:", Khi_deux(m_color[,2], V_th_color[,2]))
paste("Khi_deux color :", Khi_deux(m_color, V_th_color))

#HO est faux pour le test du Khi_deux entre diagnostic-forme
#HO est vrai pour le test du Khi_deux entre diagnostic-couleur
#la variable couleur est importante pour detecter un melanome



#### QUESTION 9 ####
#
# Nous pouvons appliquer les test Student/t sur les donnees qualitatives uniquement 
# sur 2 echantillons.
#


#### QUESTION 10 ####
#
# Nous ne pouvons pas appliquer le coefficient de Pearson et de Spearman sur les
# donnees qualitatives, c'est réservé uniquement pour les données quantitatives
#
