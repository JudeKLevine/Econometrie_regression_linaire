### Advertising data 

library(readxl)

Adv_data <- read_excel("Advertising.xlsx")
View(Adv_data)
attach(Adv_data)
summary(Adv_data)

## Sales vs TV 
plot(TV,Sales, xlab="TV", ylab="Sales", pch=18, 
     main="Figure 1. Advertising data: relation entre Sales et TV ") # cf. Fig. 1. 
# on peut insérer une courbe ou droite de régression, cf. Figure 1
a <- coefficients(lm(Sales~TV))
curve(a[1] + a[2]*x, add = TRUE, col = "blue", lwd = 4)

## Sales vs Radio Figure 2 
plot(Radio,Sales, xlab="Radio", ylab="Sales", pch=18, 
     main="Figure 2. Advertising data: relation entre Sales et Radio ") # cf. Fig. 2. 
# on peut insérer une courbe ou droite de régression, cf. Figure 1
a <- coefficients(lm(Sales~Radio))
curve(a[1] + a[2]*x, add = TRUE, col = "blue", lwd = 4)

## Sales vs Newspaper Figure 3 
plot(Newspaper,Sales, xlab="Newspaper", ylab="Sales", pch=18, 
     main="Figure 3. Advertising data: relation entre Sales et Newspaper ") # cf. Fig. 3. 
# on peut insérer une courbe ou droite de régression, cf. Figure 1
a <- coefficients(lm(Sales~Newspaper))
curve(a[1] + a[2]*x, add = TRUE, col = "blue", lwd = 4)

reg1 <- lm(Sales~TV)
summary(reg1)

reg2 <- lm(Sales~Radio)
summary(reg2)

reg3 <- lm(Sales~Newspaper)
summary(reg3)
anova(reg1)

#### Intervalle de confiance (à 95%) des coefficients
regM <- lm(Sales~TV+Radio+Newspaper)
summary(regM)
confint(regM, level = 0.975)

## Correlation variance covariances 
Vadv = cbind(Sales,TV,Radio,Newspaper)
cor(Vadv)

Mat_X = cbind(TV,Radio,Newspaper)
cor(Mat_X)
var(Mat_X)
cov(Mat_X)
var(TV)

## Approche matricelle : 

n = nrow(Adv_data)
y = as.vector(Sales) # vecteur Y variable endogène 
iotan = rep(1,n) # vecteur iota de dimension n=nombre des observations 
X = cbind(iotan,TV,Radio,Newspaper)
X = as.matrix(X)  # Matrice X des variables explicatives de dimension n*4 
head(X)
dim(X)  # vérifier la dimension de X, ie. 2
qr(X)$rank # vérification du rang de la matrice X, i.e. 4
beta_es = solve(t(X)%*%X) %*% (t(X)%*%y)  # beta estimé = (X'X)^(-1) X'y
beta_es

Px = X %*% solve(t(X)%*%X) %*%(t(X))  # le projecteur Px == X(X'X)^(-1)X'
y_es = Px %*% y  # le vecteur estimé de Y
# Figure 5 : estimé vs observé : les 15 premières observations 
ts.plot(cbind(y[1:15], y_es[1:15]), lty=1:2, main="Prédiction des ventes pour 15 marchés", 
        col = 1:2, xlab="individus",  ylab="Sales")
legend(1,8.0,legend=c("Observé", "Estimé"), lty=c(1,2))

ts.plot(cbind(y[1:200], y_es[1:200]), lty=1:2, main="Prédiction des ventes", col = 1:2, xlab="individus", 
        ylab="Sales")
legend(145,5.5,legend=c("Observé", "Estimé"), lty=c(1,2))

# Anova & Coefficients de détermination 
u_es = y - y_es  # vecteur des résidus 
SCR = t(u_es) %*% u_es  # somme des carrés résiduelle 
y_ybar = y - mean(y)
SCT = t(y_ybar)%*%(y_ybar)
SCE = SCT - SCR  # somme des carrés expliquée 
R2 = 1 - (SCR/SCT) # coefficient de détermination, R^2 = 0.1858065
R2a = 1 - ((n-1)/(n-4-1))*(1-R2)  # R^2 corrigé = 0.1842527
# Il est plus prtique d’utialiser la commande function : 
resMat = function(X,nround){
  beta_es = solve(t(X)%*%X) %*% (t(X)%*%y)
  u_es = y - y_es  # vecteur des résidus 
  # etc pour Anova, R2, et autres 
  #.... 
  vres <- round(rbind(beta_es, R2, R2a),nround) 
  row.names(vres) <-c("Constante", "TV", "Radio", "Newspaper", "R2", "R2 ajusté")
  return(vres)  
}
iotan = rep(1,n) # vecteur iota de dimension n 
X = cbind(iotan,TV,Radio,Newspaper)
resMat(X, 6)
