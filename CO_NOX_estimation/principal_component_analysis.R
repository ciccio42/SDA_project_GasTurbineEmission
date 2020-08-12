#### ---- Principal Component Analysis ---- ####
# Nei problemi di regressione la Principal Component Analysis ha l'obiettivo di trovare 
# le direzioni lungo le quali si può spiegare la maggiore variabilità del dataset e di conseguenza
# eseguite il fit lineare lungo queste componenti individuate.
# Nel nostro caso, dato che abbiamo l'esistenza di regressori fortemente correlati tra loro
# ci aspettiamo che la PCA riesca ad individuare il numero di componenti principali minori di 9

#### ---- PCA ---- ####
# Dividiamo quindi il data set in 2 set.
xx = c(1:nrow(ds))
train = (nrow(ds)*70)/100
test = (nrow(ds)*30)/100

# Sul training andiamo ad eseguire la CV per individuare il lambda miglire
# Sul test andremmo a valutare il modello ottenuto per fare un confronto Ridge vs Lasso
set.seed(1)
training_set = sample(xx, train, rep = FALSE)
test_set = sample(xx[-training_set], test, rep = FALSE)

#### ---- PCR ---- ####
library(pls)

set.seed(1)
pcr.fit = pcr(CO~.-NOX, data = ds[training_set, ],scale = TRUE, validation = "CV")
summary(pcr.fit)

dev.new()
validationplot(pcr.fit, val.type = "MSEP", legendpos = "topright")
which.min(MSEP(pcr.fit)$val[1,,][-1])

dev.new()
plot(pcr.fit, ncomp = 5, asp = 1, line = TRUE)

dev.new()
plot(pcr.fit, plottype = "scores", comps = 1:3)
explvar(pcr.fit)

#### ---- PLS ---- ####

set.seed(1)
pls.fit = plsr(CO~.-NOX, data = ds[train_set, ],scale = TRUE, validation = "CV")
summary(pls.fit)


dev.new()
validationplot(pls.fit, val.type = "MSEP", legendpos = "topright")
which.min(MSEP(pls.fit)$val[1,,][-1])

dev.new()
plot(pls.fit, ncomp = 3, asp = 1, line = TRUE)

dev.new()
plot(pls.fit, plottype = "scores", comps = 1:3)
explvar(pcr.fit)


#### ---- PLS + Poly ---- ####
set.seed(1)
pls.fit = plsr(CO~.-NOX-TIT+poly(TIT,2), data = ds[train_set, ],scale = TRUE, validation = "CV")
summary(pls.fit)


dev.new()
validationplot(pls.fit, val.type = "MSEP", legendpos = "topright")
which.min(MSEP(pls.fit)$val[1,,][-1])

dev.new()
plot(pls.fit, ncomp = 2, asp = 1, line = TRUE)

dev.new()
plot(pls.fit, plottype = "scores", comps = 1:3)
explvar(pcr.fit)
