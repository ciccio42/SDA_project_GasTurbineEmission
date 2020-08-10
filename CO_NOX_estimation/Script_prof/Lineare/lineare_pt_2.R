
# Esempio utilizzando la libreria MASS che contiene il dataset Boston
# l'obiettivo e predirre il costo medio delle case (medv)

library(MASS)

?Boston
# crea una copia in locale del dataset
fix(Boston)
# torna il nome delle features presenti nel dataset
names(Boston)

# il primo tentativo che facciamo e predire medv con il regressore lstat
attach(Boston)
fit = lm(medv~lstat)

# dal primo summary osserviamo che i coefficieti sono altamente significati
# quindi possiamo rigettare l'ipotesi nulla, quindi significa che
# b0 e b1 sono diversi da 0
# osserviamo ancora che il R^2 e abbastanza piccolo, infatti e 0.5
# cio significa che il modello non spiega tutta la variabilita.
summary(fit)

# calcoliamo l'intervallo di confidenza dei coefficienti 
confint(fit)

# calcoliamo adesso l'intervallo di confidenza e predizione rispetto ai punti
# l'intervallo di confidenza fa riferimento al punto sulla retta predetta,
# rispetto alla miglior retta che rappresenta la popolazione
predict(fit, data.frame(lstat=c(5,10,15)), interval = 'confidence')
# l'intervallo di predizione fa invece riferimento al valore y_hat predetto.
predict(fit, data.frame(lstat=c(5,10,15)), interval = 'prediction')

# eseguiamo il plot delle variabili lstat->x, mdev->y
# possiamo osservare come effettivamente la retta predetta non spiega granche bene
# i dati di training
# probabimente il legame tra medv e lstat non e lineare
dev.new()
plot(lstat,medv)
abline(fit, col = "red" )


# costruiamo gli intervalli di confidenza e predizione sul grafo 
xx <- seq(min(lstat),max(lstat),along.with = lstat) # sequenza dei punti presi da lstat
ci_lin <- predict(fit, newdata = data.frame(lstat=xx), se.fit = T, interval = 'confidence')
pi_lin <- predict(fit, newdata = data.frame(lstat=xx), set.fit = T, interval = 'prediction')
# permette di aggiungere una linea nel plot
# come sempre gli intervalli di confidenza sono schiacciati verso il centro, 
# perche quello che noi stiamo calcolando e sempre un valore atteso E[Y|X],
# quindi il nostro modello e tanto piu preciso tanto quando ci avviaciamo 
# al baricentro del cluster di punti.
matplot(xx, ci_lin$fit[,2],,lty=3,col="red",type="l",add = T)
matplot(xx, ci_lin$fit[,3],,lty=3,col="red",type="l",add = T)
matplot(xx, pi_lin[,2],,lty=3, col="green", type = "l", add = T)
matplot(xx, pi_lin[,3],,lty=3, col="green", type = "l", add = T)

# discuteremo adesso di alcuni parametri del nostro modello
# 1. Ipotesi di linearita;
# 2. Ipotesi di normalita dei residui;
# 3. Presenza di outliners;
# 4. Presenza di punti ad alto laverage.
# Prima di tutto osserviamo che i residui non si distribuiscono uniformemente
# intorno allo zero, ed in particolare seguo un pattern, questo puo essere sintomo 
# del fatto che la relazione tra lstat e medv non e lineare.
# Outlinears e punti ad alto leverage non ce ne sono, perche dal plot dei residui standardizzati
# non si vedono punti che superano rispettivamente 3 e 5.
# Osserviamo dal QQ-plot, che i residui essere abbastanza distribuiti secondo una normale
# pero c'e una buona discrepanza, questo puo essere dovuto piu al lack del fit che all'effetiva non
# normalita dei dati.
dev.new()
par(mfrow = c(2,2))
plot(fit)

# Iniziamo a complicare il nostro modello

# iniziamo ad introdurre il regressore multiplo
fit_multi = lm(formula = medv ~ lstat + age, data = Boston)
summary(fit_multi)

# eseguiamo un regressore multiplo con tutti e 13 i regressori
fit_multi_all = lm(formula = medv~., data = Boston)
# ovviamente si osserva che inserendo tutti i regressori, R^2 del modello aumenta,
# pero in generale si preferisce avere il modello piu economico possibile,
# anche perche non e detto che il modello con R^2 piu elevato sul TS sia anche 
# buono sulla predizione di nuovi valori.
# Inoltre osservare i p-value, puo essere forviante, quindi in generale e meglio
# procede con producedure step-wise.
summary(fit_multi_all)

# controlliamo la collinearita, quindi dobbiamo calcolare il VIF
library(car)
# osserviamo che i regressori tax e rad hanno un elevato VIF, questo significa 
# che sono fortemente legati ad almeno un regressore.
vif(fit_multi_all)
# guardiamo alla matrice di correlazione.
carBo <- round(cor(Boston), digits = 2)
# dalla matrice di covarianza si puo osservare come le variabili tax e rad,
# hanno un'elevata correlazione, quindi possiamo pensare di eliminare una delle due.
fit_whtout_rad = lm(formula = medv~.-rad, data = Boston )
summary(fit_whtout_rad)

# possiamo includere termini di relazione
# i due comandi di sotto generano lo stesso modello, solo che con l'asterisco 
# e implicito il principio gerarchico, quindi il modello sara lstat+age+lstat*age;
fit_inter = lm(formula = medv ~ lstat*age, data = Boston)
fit_inter = lm(formula = medv ~ lstat+age+lstat:age, data = Boston)
summary(fit_inter)

# Osservando i dati capiamo che la relazione tra lstat e medv non e lineare.
# quindi possiamo decidere di includere un termine di non linearita.
fit_lineare = lm(formula = medv~lstat, data = Boston)
fit_non_lineare = lm(formula = medv~lstat+I(lstat^2), data = Boston)
dev.new()
plot(lstat,medv)

abline(fit_lineare, col = "red")
summary(fit_lineare)
# la prima osservazione che possiamo fare e che il termine lstat^2 e fortemente significativo
# come ci aspettavamo di fatto dall'osservazione del grafo.
# inoltre aggiungendo il termine di non linearita il termine di variabilita spiegata aumenta. 
summary(fit_non_lineare)
# la tabella anova e un test che viene utilizzato per mettere a confronto due modelli
# in particolare, l'ipotesi nulla e legata al fatto che i due modelli fittano i dati ugualmente bene
# l'ipotesi alternativa e che il modello completo e superiore.
# l'anova restituisce un test-F che ha il compito di dire se il modello piu complicato
# spiega meglio la varianza del problema.
# il modello piu piccolo deve essere contenuto nel piu grande.
anova(fit_lineare, fit_non_lineare)


# calcoliamo gli intervalli di confidenza e di predizione del modello non lineare
xx <- seq(min(lstat),max(lstat),along.with = lstat) # sequenza dei punti presi da lstat
# calcolando le predizioni del modello
predictions <- predict(fit_non_lineare, newdata = data.frame(lstat=xx), se.fit = T, interval = 'confidence')
matplot(xx,predictions$fit[,1] ,,lty=3,col="blue",type="l",add = T)

ci_non_lin <- predict(fit_non_lineare, newdata = data.frame(lstat=xx), se.fit = T, interval = 'confidence')
pi_non_lin <- predict(fit_non_lineare, newdata = data.frame(lstat=xx), set.fit = T, interval = 'prediction')
matplot(xx,ci_non_lin$fit[,2] ,,lty=3,col="green",type="l",add = T)
matplot(xx,ci_non_lin$fit[,3] ,,lty=3,col="green",type="l",add = T)
matplot(xx,pi_non_lin[,2] ,,lty=3,col="red",type="l",add = T)
matplot(xx,pi_non_lin[,3] ,,lty=3,col="red",type="l",add = T)

# adesso andiamo ad osservare i plot per la diagnostica finale del modello.
# il fatto che un modello polinomiale di ordine due e migliore rispetto
# alla retta si vede sia dal grafico del fit dei dati
# sia dai plot di diagnosi.
dev.new()
par(mfrow = c(2,2))
plot(fit_non_lineare)

# adesso ci potremmo chiedere qual'e l'ordine massimo del polinomio che introduce dei miglioramenti
fit_ordine_3 = lm(formula = medv ~ poly(lstat,3), data = Boston)
ci_ordine_3 <- predict(fit_ordine_3, newdata = data.frame(lstat=xx), se.fit = T, interval = 'confidence')
pi_ordine_3 <- predict(fit_ordine_3, newdata = data.frame(lstat=xx), set.fit = T, interval = 'prediction')

# Osserviamo dal output della funzione anova che introdurre il terzo grado nel polinimio migliora 
# permette di spiegare meglio la variablita del modello.
anova(fit_non_lineare, fit_ordine_3)

dev.new()
par(mfrow = c(1,1))
plot(lstat, medv, ylim = c(0,60))
matplot(xx, ci_ordine_3$fit[,1], lty = 1, ltw = 2, col = "red", type = "l", add = T)
matplot(xx, ci_ordine_3$fit[,2], lty = 2, ltw = 2, col = "green", type = "l", add = T)
matplot(xx, ci_ordine_3$fit[,3], lty = 2, ltw = 2, col = "green", type = "l", add = T)
matplot(xx, pi_ordine_3[,2], lty = 2, ltw = 2, col = "blue", type = "l", add = T)
matplot(xx, pi_ordine_3[,3], lty = 2, ltw = 2, col = "blue", type = "l", add = T)

fit_ordine_5 = lm(formula = medv ~ poly(lstat,5), data = Boston)
ci_ordine_5 <- predict(fit_ordine_5, newdata = data.frame(lstat=xx), se.fit = T, interval = 'confidence')
matplot(xx, ci_ordine_5$fit[,1], lty = 1, ltw = 1, col = "blue", add = T)
# Osserviamo che all'aumentare del grado del polinomio la qualita del fit aumenta, pero possiamo ancora dire che 
# aumentando il grado del polinomio rendiamo il nostro modello piu dipendente dai dati del training set.
# andando quindi potenzialmente, a degradare le prestazioni su future previsioni.
# in generale quando abbiamo due modelli con prestazioni equiparabili si preferisce quello piu semplice.

dev.new()
par(mfrow = c(2,2))
plot(fit_ordine_5)

# proviamo ad eseguire qualche trasformazione del regressore.
fit_log = lm(formula = medv  ~ log(lstat), data = Boston)
# la trasformazione logaritmica e statisticalmente influente. 
summary(fit_log)

### QUALITATIVE PREDICTORS.
# Ricordiamo che i predittori qualitativi sono dei predittori che assumono valore in un set discreto di punti,
# i valori assunti possono non avere relazione di ordine, questo significa che dire che Male > Female non ha senso.
# Quindi le operazioni matematiche che vengono svolte su questi predittori possono non avere senso.
# Possiamo includere queste variabili in un modello di regressione lineare attraverso le variabili dummy.

# Esaminiamo il dataset Carseats.
install.packages("ISLR")
library(ISLR)
fix(Carseats)
attach(Carseats)
names(Carseats)
# il nostro obiettivo e quello di predire le vendite.

# il primo passo e quello di includure tutti i regressori con qualche termine di interazione
fit = lm(Sales  ~ .+ Income : Advertising +Price :Age , data= Carseats)
# il comando lm si accorge automaticamente delle variabili qualitative al fine di creare le variabili dummy relative
summary(fit)
contrasts(ShelveLoc) # con questo comando siamo in grado di vedere come il comando lm ha creato le dummy variable.
# nel nostro caso e stata scelata come baseline il valore BAD, quindi la lettura dei coefficienti generati vanno letti rispetto alla baseline.

#######
# Adesso creiamo dei regressori multipli.
multre <- lm(medv~.-rad-indus-age-tax+I(lstat^2), data = Boston)
multre <- lm(medv~.-rad-indus-age-lstat+log(lstat), data = Boston)
summary(multre)

# quello che noi stiamo facendo adesso e plottare il valore di medv in funzione di lstat, tenendo conto anche dell'influenza
# degli altri regressori su medv
# quello che faccimo e dire che il nostro legame principale rimane medv -> lstat.
# quindi ci domandiamo come l'influenza degli altri regressori fanno variare il nostro legame principale.
# quello che possiamo osservare che effettivamente la presenza degli altri regressori influenzano il nostro legame principale.
# questo e un modo per capire la variabilita introdotto dagli altri regressori.
xx <- seq(min(lstat), max(lstat), along.with = lstat)
ci_multre <- predict(multre, newdata = data.frame(lstat = xx), se.fit = T, interval = "confidence")
dev.new()
par(mfrow = c(1,1)) 
plot(lstat, medv, ylim = c(0,60))
matplot(xx, ci_multre$fit[,1], lty = 1, ltw = 2, col = "red",type = "l", add = T)
matplot(xx, ci_multre$fit[,2], lty = 2, ltw = 2, col = "red",type = "l", add = T)
matplot(xx, ci_multre$fit[,3], lty = 2, ltw = 2, col = "red",type = "l", add = T)
