#### ---- library ---- ####
library(car)
ds = read.csv("dataset.csv")
ds_train = read.csv("ds_train.csv")
ds_test = read.csv("ds_test.csv")
attach(ds_train)
#### ---- Valutazione della possibile eliminazione di outliers nella risposta CO ---- ####
# Il 99% dei punti della risposta si distribuisce tra un minimo di 0.00039 ad un massimo di 11
# quindi i punti da 11 a 44, rappresentano solo 1% della popolazione, e possono essere possibili
# outliers
summary(ds$CO)
dev.new()
boxplot(ds$CO)
quantile(ds$CO, probs = c(0.05, 0.99))

# Obiettivo capire, quando la presenza o meno di questi punti influesce sulla capacità di predizione
# si vuole quindi capire se un modello allenato su tutti i dati sia in grado di predire in maniera consistente
# i punti più comuni, da qui capiamo che l'influenza di questi punti "fuori popolazione" è irrilevante
ds_train_senza = data.frame(ds_train[ds_train[,"CO"]<11, ])
ds_test_senza = data.frame(ds_test[ds_test[,"CO"]<11, ])

# Ovviamente con la rimozione dei punti dal TS, si osserva un'aumento della varianza spiegata
# perché i punti non meno sparsi, i residui diminuoiscono, e R2 aumenta
fit_tit_all = lm(CO~., data = ds_train)
summary(fit_tit_all)
fit_tit_all_senza = lm(CO~., data = ds_train_senza)
summary(fit_tit_all_senza)

# Osserviamo che il modello allenato sui "dati puliti" ha prestazioni peggiori sulla predizione
# di un test non "pulito", con uno scarto di 13 centesimi.
mean((ds_test[,"CO"]-predict(fit_tit_all, newdata = ds_test))^2) # 2.220337
mean((ds_test[,"CO"]-predict(fit_tit_all_senza, newdata =  ds_test))^2) #2.359168

# Osserviamo come la capacità di predizione del modello generale performa leggermente peggio (scarto di 0.069095)
# sul test con i soli punti coerenti alla popolazione di training.
mean((ds_test_senza[,"CO"]-predict(fit_tit_all, newdata = ds_test_senza))^2) # 1.141073
mean((ds_test_senza[,"CO"]-predict(fit_tit_all_senza, newdata =  ds_test_senza))^2) # 1.071978

# Quindi abbiamo visto come la presenza di punti che si trovano alle code della popolazione in generale ha un'impatto sulla
# capacità predittiva, però osserviamo come la capacità del modello allenato sui dati non puliti di predirre i punti
# più probabili, è paragonabile a quella del modello allenato sui dati più comuni.
# Questo, unito al fatto che i punti fuori dalla popolazione non sono errori ma sono semplicemente casi meno probabili
# ci spinge a mantenerli.

#### ---- Least Squares ---- ####

# Partiamo con l'eseguire un fit con tutti i regressori, al fine di verficare che
# 1. In presenza di regressori correlati il VIF è elevato;
# 2. Andando a selezionare le feature realmente dipendenti il MSE di test stimato diminuisce
fit_all = lm(CO~., data = ds_train)

confint(fit_all)

#### ---- VIF ---- ####
# l'indice di VIF legato alle variabili maggiormente correlate tra loro risulta essere maggiore di 1,
# indice di presenza di sicuramente una collinearità (matrice di correlazione), ma anche di multicollinearità
vif(fit_all)

#### ---- Summary fit_all ---- ####
# Vediamo come dal summary la F-statistic risulta essere effettivamente elevata, così come risulta essere
# basso il p-value legato alla F-statistic, di conseguenza si può rigettare con ancora più forza l'ipotesi nulla
# in quanto in situazioni di collinearità in generale la stima dello SE dei coefficienti è più elevato, di conseguenza la F-statistic
# diminuisce facendo aumentare il p-value.
summary(fit_all) 

#### ---- Risoluzione della collinearità ---- ####
# Si considerano i regressori per i quali VIF > 5.
# Dalla matrice di correlazione, abbiamo che i regressori correlati sono: TIT, GTEP, TEY, CDP, TAT, AP

fit_no_tat = lm(CO~.-TAT, data = ds_train)
summary(fit_no_tat)
vif(fit_no_tat)

# Eliminiamo quei regressori con un VIF elevato
fit1 = lm(CO~.-TEY-CDP-GTEP, data = ds_train);
vif(fit1)
# Il modello senza i regressori di sopra ha un valore di R^2 paragonabile al modello totale,
# a dimostrazione del fatto che l'informazione portata da TEY,CDP e GTEP può essere spiegata da altri regressori
# e la F-statistic aumenta. 
summary(fit1)
confint(fit1)
#### ---- Grafici di diagnostica ---- ####
# Osservazioni:
# 1. Una distribuzione non uniforme dei residui;
# 2. La presenza di outliers e punti ad elevato leverage;
# La presenza di questi punti può essere spiegata in due modi:
# 1. Lack of fit (assunzione di non linearità non valida) motivazione data dalla presenza di un pattern nel grado dei residui e un R^2 relativamente basso
# 2. Effetti outliers o high leverage, quindi punti con un valore della y non comune (outliers) o punti non comuni sulla x (high-leverage point)
dev.new()
par(mfrow = c(2,2))
plot(fit1)

fit1.stderrors = rstandard(fit1)
hist(fit1.stderrors)
plot(density(fit1.stderrors))

dev.new()
# Osservando ancora i grafici a coppie dei soli predittori rilevanti
# possiamo osservare come in realtà, la motivazione principale può essere data dalla presenza di relazioni non lineari
pairs(~CO+AT+AP+AH+AFDP+TIT+TAT)

#### ---- Polynomial Regression ---- ####
fit_tit =  lm(CO~TIT, data = ds_train);
summary(fit_tit)

fit_poly =  lm(CO~poly(TIT,2), data = ds_train);
# Osserviamo come R^2 solo con poly aumenta a 0.60
summary(fit_poly)


fit_poly_3 =  lm(CO~poly(TIT,3), data = ds_train);
summary(fit_poly_3)

fit_poly_4 =  lm(CO~poly(TIT,4), data = ds_train);
summary(fit_poly_4)

##### ---- TIT poly plot ---- ####
# Osserviamo come tra il polinomio 2 a 3 si osserva un leggero miglioramento, il 3 e il 4 sono praticamente indistinguibili
dev.new()
plot(TIT, CO)

x <- with(ds_train, seq(min(ds_train$TIT), max(ds_train$TIT), length.out=2000))
y <- predict(fit_poly, newdata = data.frame(TIT = x))
lines(x, y, col = "red")

x <- with(ds_train, seq(min(ds_train$TIT), max(ds_train$TIT), length.out=2000))
y <- predict(fit_poly_3, newdata = data.frame(TIT = x))
lines(x, y, col = "blue")

x <- with(ds_train, seq(min(ds_train$TIT), max(ds_train$TIT), length.out=2000))
y <- predict(fit_poly_4, newdata = data.frame(TIT = x))
lines(x, y, col = "yellow")

#### ---- Fit1 + poly(TIT) ---- ####
fit1.poly2 = lm(CO~.-TEY-CDP-GTEP-TIT+poly(TIT, 2), data = ds_train);
fit1.poly3 = lm(CO~.-TEY-CDP-GTEP-TIT+poly(TIT, 3), data = ds_train);

# Dal sumary si osserva come tra i coefficienti il regressore che domina è proprio quello polinomiale
# infatti le performance sul training set tra il modello con il solo polinomio e il modello con gli altri 
# regressori sono praticamente identiche
summary(fit1.poly2)
summary(fit1.poly3)

vif(fit1.poly2)
vif(fit1.poly3)

# Osserviamo come introducendo un legame non lineare otteniamo due risultati
# 1. L'indice R^2 aumenta da 0.55 a 0.60, ottenendo quindi un aumento considerevole della varianza
# spiegata sul training set.
# 2. Il pattern presente nel plot dei residui si va a stringere, a conferma dell'assunzione di non linearità dei dati.
dev.new()
par(mfrow = c(2,2))
plot(fit1.poly2)

dev.new()
par(mfrow = c(2,2))
plot(fit1.poly3)

# L'introduzione del polinomio di grado 3, per il test anova è significativo
anova(fit1.poly2, fit1.poly3)


#### ---- Outliears and High leverage point considerations---- ####
# In questa sezione andiamo ad eliminare quei punti che risultano essere outliers e di high-leverage
# Confrontando le prestazioni tra i modelli con il polinomio e il lineare fit1
w <- abs(rstudent(fit1.poly2)) < 3 & abs(cooks.distance(fit1.poly2)) < 4/nrow(fit1.poly2$model)
ds_train_no_out = ds_train[w,]

# A causa della eliminazione dei punti con valori insoliti, si osserva un generale aumento della 
# variabilità spiegata, fit2 -> r2 = 0.68; poly2 = 0.7544; poly3 =  0.7574.
fit2 <- lm(CO~.-TEY-CDP-GTEP, data = ds_train_no_out);
summary(fit2)

fit1.poly2_no_out <- lm(CO~.-TEY-CDP-GTEP-TIT+poly(TIT, 2), data = ds_train_no_out);
summary(fit1.poly2_no_out)

fit1.poly3_no_out <- lm(CO~.-TEY-CDP-GTEP-TIT+poly(TIT, 3), data = ds_train_no_out);
summary(fit1.poly3_no_out)

# Si osserva come la condizione di non linearità nel plot dei residui è ancora più accentuata
# e come con l'introduzione del polinomio questa si va a ridurre
dev.new()
par(mfrow = c(2,2))
plot(fit2)

dev.new()
par(mfrow = c(2,2))
plot(fit1.poly2_no_out)

dev.new()
par(mfrow = c(2,2))
plot(fit1.poly3_no_out)

#### ---- Models evaluation ---- ####
# Si andranno a testare i seguenti modelli:
# fit_all, fit1, fit1.poly2, fit1.poly3, fit2, fit1.poly2_no_out, fit1.poly3_no_out

# Osserviamo come il MSE calcolato è molto simile, tra i vari regressori, osservando una diminuzione
# soprattutto dopo l'introduzione del polinomio, a dimostrazione del fatto che la relazione tra i 
# regressori e l'uscita non è lineare.

fit_all_test_mse = mean((ds_test$CO-predict(fit_all,ds_test))^2) # 2.220337

fit_1_test_mse = mean((ds_test$CO-predict(fit1,ds_test))^2) # 2.256107

fit_poly_test_mse = mean((ds_test$CO-predict(fit_poly,ds_test))^2) # 2.004868

fit_1_poly2_test_mse = mean((ds_test$CO-predict(fit1.poly2,ds_test))^2) # 1.988706

fit_1_poly3_test_mse = mean((ds_test$CO-predict(fit1.poly3,ds_test))^2) # 1.937354

fit_2_test = mean((ds_test$CO-predict(fit2,ds_test))^2) # 2.374575

fit_1_poly2_no_out_test_mse = mean((ds_test$CO-predict(fit1.poly2_no_out,ds_test))^2) # 2.048272

fit_1_poly3_no_out_test_mse = mean((ds_test$CO-predict(fit1.poly3_no_out,ds_test))^2) # 1.992207

#### ---- How confidence and prediction intervals change ---- ####
# Andiamo a confrontare come gli intervalli di confidenza e predizione sulla risposta CO, rispetto al regressore
# TIT, per il quale è stato osservato un legame con CO, cambiano rispetto alle varie modifiche apportate al modello
# con l'aspettativa che:
# 1. Introducendo il termine quadratico, l'intervallo si restringa, rispetto alla semplice regressione lineare;
# 2. Intervalli non si modificano con l'introduzione degli altri regressori, in quanto è stato osservato come in realtà a dominare
# è il termine polinomiale
xx <- seq(min(ds_train$TIT), max(ds_train$TIT), along.with = ds_train$TIT)

ci_pred_TIT = predict(fit_tit, newdata = data.frame(TIT = xx), se.fit = T, interval = "confidence")
pi_pred_TIT = predict(fit_tit, newdata = data.frame(TIT = xx), set.fit = T, interval = "prediction")

dev.new()
plot(TIT, CO)
#### ---- Linear only TIT ---- ####
abline(fit_tit, col = "blue")
matplot(xx, ci_pred_TIT$fit[,2],,lty=3,col="red",type="l",add = T, lwd = 2)
matplot(xx, ci_pred_TIT$fit[,3],,lty=3,col="red",type="l",add = T, lwd = 2)
matplot(xx, pi_pred_TIT[,2],,lty=3, col="green", type = "l", add = T, lwd = 2)
matplot(xx, pi_pred_TIT[,3],,lty=3, col="green", type = "l", add = T, lwd = 2)

#### ---- Linear TIT with others ---- ####
dev.new()
plot(TIT, CO)
y <- predict(fit1, newdata = data.frame(TIT = xx))
matplot(xx, y,,lty=3,col="red",type="l",add = T, lwd = 2)
ci_pred_TIT_others = predict(fit1, newdata = data.frame(TIT = xx), se.fit = T, interval = "confidence")
pi_pred_TIT_others = predict(fit1, newdata = data.frame(TIT = xx), set.fit = T, interval = "prediction")
matplot(xx, ci_pred_TIT_others$fit[,2],,lty=3,col="red",type="l",add = T, lwd = 2)
matplot(xx, ci_pred_TIT_others$fit[,3],,lty=3,col="red",type="l",add = T, lwd = 2)
matplot(xx, pi_pred_TIT_others[,2],,lty=3, col="green", type = "l", add = T, lwd = 2)
matplot(xx, pi_pred_TIT_others[,3],,lty=3, col="green", type = "l", add = T, lwd = 2)

#### ---- Poly 2 TIT only ---- ####
# Si osserva come effettivamente con l'introduzione del polinomio
# gli intervalli di predizione seguono di più l'andamento della curva dei dati
# andando a restringere l'intervallo di predizione
dev.new()
plot(TIT, CO)
y <- predict(fit_poly, newdata = data.frame(TIT = xx))
lines(xx, y, col = "red")
ci_pred_TIT_poly = predict(fit_poly, newdata = data.frame(TIT = xx), se.fit = T, interval = "confidence")
pi_pred_TIT_poly = predict(fit_poly, newdata = data.frame(TIT = xx), set.fit = T, interval = "prediction")
matplot(xx, ci_pred_TIT_poly$fit[,2],,lty=3,col="yellow",type="l",add = T, lwd = 2)
matplot(xx, ci_pred_TIT_poly$fit[,3],,lty=3,col="yellow",type="l",add = T, lwd = 2)
matplot(xx, pi_pred_TIT_poly[,2],,lty=3, col="violet", type = "l", add = T, lwd = 2)
matplot(xx, pi_pred_TIT_poly[,3],,lty=3, col="violet", type = "l", add = T, lwd = 2)

#### ---- Poly 2 TIT with others ---- #### 
dev.new()
plot(TIT, CO)
y <- predict(fit1.poly2, newdata = data.frame(TIT = xx))
lines(xx, y, col = "red")
ci_pred_TIT_poly = predict(fit1.poly2, newdata = data.frame(TIT = xx), se.fit = T, interval = "confidence")
pi_pred_TIT_poly = predict(fit1.poly2, newdata = data.frame(TIT = xx), set.fit = T, interval = "prediction")
matplot(xx, ci_pred_TIT_poly$fit[,2],,lty=3,col="yellow",type="l",add = T, lwd = 1)
matplot(xx, ci_pred_TIT_poly$fit[,3],,lty=3,col="yellow",type="l",add = T, lwd = 1)
matplot(xx, pi_pred_TIT_poly[,2],,lty=3, col="violet", type = "l", add = T, lwd = 1)
matplot(xx, pi_pred_TIT_poly[,3],,lty=3, col="violet", type = "l", add = T, lwd = 1)

#### ---- Poly 3 TIT only ---- ####
dev.new()
plot(TIT, CO)
y <- predict(fit_poly_3, newdata = data.frame(TIT = xx))
lines(xx, y, col = "red")
ci_pred_TIT_poly = predict(fit_poly_3, newdata = data.frame(TIT = xx), se.fit = T, interval = "confidence")
pi_pred_TIT_poly = predict(fit_poly_3, newdata = data.frame(TIT = xx), set.fit = T, interval = "prediction")
matplot(xx, ci_pred_TIT_poly$fit[,2],,lty=3,col="yellow",type="l",add = T, lwd = 1)
matplot(xx, ci_pred_TIT_poly$fit[,3],,lty=3,col="yellow",type="l",add = T, lwd = 1)
matplot(xx, pi_pred_TIT_poly[,2],,lty=3, col="violet", type = "l", add = T, lwd = 1)
matplot(xx, pi_pred_TIT_poly[,3],,lty=3, col="violet", type = "l", add = T, lwd = 1)


#### ---- Poly 3 TIT with others ---- ####
dev.new()
plot(TIT, CO)
y <- predict(fit1.poly3, newdata = data.frame(TIT = xx))
lines(xx, y, col = "red")
ci_pred_TIT_poly = predict(fit1.poly3, newdata = data.frame(TIT = xx), se.fit = T, interval = "confidence")
pi_pred_TIT_poly = predict(fit1.poly3, newdata = data.frame(TIT = xx), set.fit = T, interval = "prediction")
matplot(xx, ci_pred_TIT_poly$fit[,2],,lty=3,col="yellow",type="l",add = T, lwd = 1)
matplot(xx, ci_pred_TIT_poly$fit[,3],,lty=3,col="yellow",type="l",add = T, lwd = 1)
matplot(xx, pi_pred_TIT_poly[,2],,lty=3, col="violet", type = "l", add = T, lwd = 1)
matplot(xx, pi_pred_TIT_poly[,3],,lty=3, col="violet", type = "l", add = T, lwd = 1)

#### ---- TIT + TAT + Poly(TIT) ---- ####
fit_tit_poly_tat = lm(formula = CO~TAT+poly(TIT,3), data = ds_train) 
summary(fit_tit_poly_tat)
mean((ds_test[, "CO"]-predict(fit_tit_poly_tat, newdata = ds_test))^2)
