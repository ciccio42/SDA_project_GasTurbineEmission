
############### Advertising Data ##################
ad = read.csv("Advertising_FP.csv")
attach(ad)
## simple linear model
fit = lm(sales~newspaper, data=ad)
summary(fit)
fit = lm(sales~radio, data=ad)
summary(fit)
fit = lm(sales~TV, data=ad)
summary(fit)
xx <- seq(min(TV),max(TV),along.with = TV)
ci_lin <- predict(fit,newdata=data.frame(TV=xx),se.fit = T,interval = "confidence")
pi_lin <- predict(fit,newdata=data.frame(TV=xx),se.fit = T,interval = "prediction")

dev.new()
par(mfrow=c(1,1))
plot(TV, sales, ylim=c(0,60))
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)
matplot(xx,ci_lin$fit[,2],lty=3,col="red", type="l", add=T)
matplot(xx,ci_lin$fit[,3],lty=3,col="red", type="l", add=T)
matplot(xx,pi_lin$fit[,2],lty=3,col="green", type="l", add=T)
matplot(xx,pi_lin$fit[,3],lty=3,col="green", type="l", add=T)


## multiple linear model
fit = lm(sales~TV+radio+newspaper, data=ad)
# osserviamo che utilizzando tutti e tre i regressori
# il valore del p-value di newspaper aumenta.
summary(fit)
# dalla matrice di covarianza vediamo che la correlazione tra radio e newspaper è abbastanza consistente.
corAdv <- round(cor(ad), digits =2)
print(corAdv)
# eseguiamo un fit eliminando newspaper
fit = lm(sales~TV+radio, data=ad);
summary(fit)
fit = lm(sales~TV*radio, data=ad)
summary(fit)
confint(fit)
## test a group of coefficients simultaneously, e.g. radio=newspaper=0
library(car)
fit = lm(sales~TV+radio+newspaper, data=ad)
summary(fit)
# stiamo facendo un test di ipotesi, dove la nostra ipotesi è che radio e newspaper siano uguali a 0
linearHypothesis(fit, c("radio = 0", "newspaper = 0"))
# dai risultati possiamo osservare che l'ipotesi H_0 si può rifiutare,
# questo significa che almeno uno dei due deve essere diverso da 0

## or alternatively
fit1 = lm(sales~TV, data=ad);
fit2 = lm(sales~TV+radio+newspaper, data=ad)
anova(fit1, fit2)

## test a single coefficient, e.g. newspaper=0
fit = lm(sales~TV+radio+newspaper, data=ad)
summary(fit)
# in questo caso sia testando solo una singola variabile, in particolare stiamo testando
# come ipotesi H_0 il fatto che newspaper è pari a 0
linearHypothesis(fit, c("newspaper = 0"))
# si può vedere come newspaper è irrilevante per spiegare la variabilità dei dati
## or alternatively
fit1 = lm(sales~TV+radio, data=ad);
fit2 = lm(sales~TV+radio+newspaper, data=ad)
anova(fit1, fit2)

## variable selection
# Stepwise
library(MASS)
#fit = lm(sales~TV+radio+newspaper, data=ad)
fit = lm(sales~., data=ad)
step <- stepAIC(fit, direction="both") ## stepwise
step$anova # display results



############### Credit Data ##################
library(ISLR)
credit <- ISLR::Credit
#credit = read.csv("Credit.csv")
summary(credit)
## qualitative predictors
fit = lm(Balance~factor(Ethnicity), data=credit)
summary(fit)
contrasts(credit$Ethnicity)
## collinearity
fit = lm(Balance~Age+Limit, data=credit)
summary(fit)
vif(fit)
fit = lm(Balance~Rating+Limit, data=credit)
summary(fit)
vif(fit)
# write.csv(credit,"Credit.csv",row.names = F,quote = F)
