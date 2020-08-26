#### ---- Principal Component Analysis ---- ####
# Nei problemi di regressione la Principal Component Analysis ha l'obiettivo di trovare 
# le direzioni lungo le quali si può spiegare la maggiore variabilità del dataset e di conseguenza
# eseguite il fit lineare lungo queste componenti individuate.
# Nel nostro caso, dato che abbiamo l'esistenza di regressori fortemente correlati tra loro
# ci aspettiamo che la PCA riesca ad individuare il numero di componenti principali minori di 9
# Anche se le prestazioni attese andranno sempre nella stessa direzione dei modelli precedenti
ds = read.csv("dataset.csv")
ds_train = read.csv("ds_train.csv")
ds_test = read.csv("ds_test.csv")
#### ---- PCA ---- ####
# Osservando la matrice di correlazione e gli scatter plot si osserva che alcuni regressori presentano
# una relazione lineare, questo significa che andando ad eseguire il calcolo delle componenti principali
# rispetto a questi regressori si potrà notare come un numero minimo di componeneti possa spiegare la loro
# variabilità.
# Un esempio possono essere i predittori TIT e GTEP (fortemente correlati) e come contro esempio TIT e AP (meno correlati)
ds_example = subset(ds, select = c("TIT", "GTEP"))
pr.out=prcomp(ds_example, scale=TRUE)
names(pr.out)

# Si nota come a causa della correlazione tra TIT e GTEP, allora la prima componente è sufficiente a spigare la variabilità
# delle stesse, dato che i punti si distribuiscono in maniera lineare.
# Mentre la seconda componente spiega la restante percentuale di variabilità dei dati che risulta essere a questo punto ininfluente
# infatti i dati si distribuiscono in maniera più sparsa.
dev.new()
par(mfrow = c(1,2))
plot(pr.out$x[, 1],ds_example[, "TIT"], xlab = "1st Principal Component Scores", ylab = "TIT", col = "green")
plot(pr.out$x[, 1],ds_example[, "GTEP"], xlab = "1st Principal Component Scores", ylab = "GTEP", col = "green")
dev.new()
par(mfrow = c(1,2))
plot(pr.out$x[, 2],ds_example[, "TIT"], xlab = "2nd Principal Component Scores", ylab = "TIT", col = "green")
plot(pr.out$x[, 2],ds_example[, "GTEP"], xlab = "2nd Principal Component Scores", ylab = "GTEP", col = "green")

# A conferma di quanto detto sopra si osserva come la PVE (Proportion of Variance Explained) della prima
# componente è pari al 93%, mentre la seconda spiega il restante 6%
pr.out$sdev
pr.var=pr.out$sdev^2; pr.var
pve=pr.var/sum(pr.var) 
pve # 0.93711708 0.06288292

# Contro esempio
ds_example = subset(ds, select = c("TIT", "AP"))
pr.out=prcomp(ds_example, scale=TRUE)

# Osserviamo come in presenza di dati non correlati, la tecnica della Principal Component Analysis fallisce
# in quanto al fine di spiegare la variabilità dei dati si ha la necessità di utilizzare un numero di componenti
# pari al numero dei regressori di partenza
dev.new()
par(mfrow = c(1,2))
plot(pr.out$x[, 1],ds_example[, "TIT"], xlab = "1st Principal Component Scores", ylab = "TIT", col = "blue")
plot(pr.out$x[, 1],ds_example[, "AP"], xlab = "1st Principal Component Scores", ylab = "AP", col = "blue")
dev.new()
par(mfrow = c(1,2))
plot(pr.out$x[, 2],ds_example[, "TIT"], xlab = "2nd Principal Component Scores", ylab = "TIT", col = "blue")
plot(pr.out$x[, 2],ds_example[, "AP"], xlab = "2nd Principal Component Scores", ylab = "AP", col = "blue")
pr.out$sdev
pr.var=pr.out$sdev^2; pr.var
pve=pr.var/sum(pr.var) 
pve # 0.502695 0.497305

#### ---- PCR ---- ####
# Principal Compent Regression
library(pls)

set.seed(1)
pcr.fit = pcr(CO~., data = ds_train,scale = TRUE, validation = "CV")
          
# Osserviamo come già con 3 componenti abbiamo una spiegazione della varianza del dataset
# accettabile, la spiegazione di ciò va ricercato nel fatto ci sono regressori tra loro correlati
# portando la diminuzione della dimensionalità sulla base dei ragionamenti di sopra.
summary(pcr.fit)

# Si osserva come il numero minomo di componenti che ci assicura un valore di MSE valido
# è pari a 5, aggiungerne altri comporta una variazione del MSE non significativa
dev.new()
validationplot(pcr.fit, val.type = "MSEP", legendpos = "topright")
MSEP(pcr.fit)$val # 2.31

# Osserviamo come per quella zona a maggiore densità il fit è ragionevolmente buono, 
# si osserva ancora l'influenza dei outliers e della presenza di non-linearità nella zona 
# a maggiore densità.
dev.new()
plot(pcr.fit, ncomp = 5, asp = 1, line = TRUE)
min_pcr = 5

dev.new()
plot(pcr.fit, plottype = "scores", comps = 1:3)
explvar(pcr.fit)

#### ---- PLS ---- ####
# Dal summary si può osservare l'effetto di introdurre la risposta nel calcolo delle componenti principali
# in quant osserviamo come a parità di componenti auenta la variabilità spiegata dell'uscita
# ma diminuisce quella dei dati.
set.seed(1)
pls.fit = plsr(CO~., data = ds_train,scale = TRUE, validation = "CV")
summary(pls.fit)

# Si ottiene un MSEP ragionevole per un numero di componenti pari a 3.
# Il numero di compomenti determinanti è minore del modello ottenuto sopra
# in quanto la PLS nel calcolo delle componenti prende in considerazione 
# anche la risposta CO.
dev.new()
validationplot(pls.fit, val.type = "MSEP", legendpos = "topright")
MSEP(pls.fit)$val # 2.30

# Osserviamo che si ottiene un grafico simile a quanto osservato sopra ma con meno componenti 
# (3 invece di 5)
dev.new()
plot(pls.fit, ncomp = 3, asp = 1, line = TRUE)


#### ---- PLS + Poly ---- ####

# A parità di componenti la varianza spiegata dell'uscita è maggiore con l'introduzione del
# polinomio.
set.seed(1)
pls_poly.fit = plsr(CO~.-TIT+poly(TIT,3), data = ds_train,scale = TRUE, validation = "CV")
summary(pls_poly.fit)

# Si osserva come con l'introduzione del polinomio il valore del MSEP diminuisce in maniera significativa
# in particolare valori ragionevoli si ottengono per un numero di componenti pari a 3
dev.new()
validationplot(pls_poly.fit, val.type = "MSEP", legendpos = "topright")
MSEP(pls_poly.fit)$val # 2.00 #PC = 3

# Osserviamo come con la presenza della trasformazione i residui che prima si allontanavano dalla retta
# adesso tendono a stringersi.
dev.new()
plot(pls_poly.fit, ncomp = 3, asp = 1, line = TRUE)

#### ---- PCR vs PLS vs PLS+Poly ---- ####
# Osserviamo come le prestazioni migliori tra i modelli migliori ottenuti con la tecnica PCA
# si ottengono ancora una volta con la presenza di una trasformata non lineare.
# Osserviamo come l'andamente delle prestazioni è ancora coerente con quelli ottenuti con gli
# altri modelli, di conseguenza per motivi di facilità di interpretazione si andrà comunque
# a scegliere un modello ottenuto con le tecniche classiche di analisi dei regressori.
pcr_prediction = predict(pcr.fit, ds_test, ncomp = 5)
pcr_mse_test =  mean((pcr_prediction-ds_test[ , "CO"])^2) # 2.301188

pls_prediction = predict(pls.fit, ds_test, ncomp = 3)
pls_mse_test =  mean((pls_prediction-ds_test[ , "CO"])^2) # 2.285168

pls_poly.fit_prediction = predict(pls_poly.fit, ds_test, ncomp = 3)
pls_mse_test =  mean((pls_poly.fit_prediction-ds_test[ , "CO"])^2) # 1.979691

