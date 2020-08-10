# Best Subset selection

# Obiettivo predirre Baseball player's Salary sulla base delle varie statistiche
# di gioco associate al giocatore negli anni passati

library(ISLR)

names(Hitters) # torna il nome delle features
head(Hitters) # torna i primi elementi del dataset

# Osserviamo come per alcuni giocatori la features "Salary" non è presente
# Quindi andiamo ad eliminare tutte quelle righe che contengono un NA nel campo "Salary"
dim(Hitters) 
sum(is.na(Hitters$Salary))
# Abbiamo 59 righe con il campo "Salary" settato con NA
Hitters = na.omit(Hitters) # eliminiamo le righe con NA
dim(Hitters)
sum(is.na(Hitters$Salary))
n = nrow(Hitters) # n è il numero delle features

## -------- Subset Selection --------##

# Iniziamo con l'utilizzare la Best Subset Selection, procedura che seleziona il 
# modello migliore tra tutte le possibili combinazione degli p predittori presenti nel
# data set, il miglio modello è quantificato sulla base del RSS.
library(leaps)
# Salary è la risposta che si desidera predire, con tilde. stiamo indicando di utilizzare
# tutti i predittori presenti in Hitters.
# In questo caso l'algoritmo utilizzato da regsubset è una semplice ricerca esaustiva
regfit.full = regsubsets(Salary~., Hitters)
summary(regfit.full)
# L'output del summary ci permette di vedere come la regressione si è fermata a modelli
# che includono 8 predittori. Quindi per ogni modello con un numero fissato di predittori
# che va da 1 a 8, troviamo il miglior modello che permette di predirre il Salary sulla base
# dello RSS.

# Con il parametro nvmax possiamo indicare anche il numero massimo di predittori
# da aggiungere.

regfit.full = regsubsets(Salary~., data = Hitters, nvmax = 19)
reg.summary = summary(regfit.full)

# Il comando summary ritorna anche le statistiche R2, RSS, adjusted R2, Cp e BIC.
names(reg.summary)
# Osserviamo come plottando R2 semplice, quello che vediamo è che all'aumentare del
# numero di predittori inclusi aumenta il R2
reg.summary$rsq
# andandoci a plottare il minimo RSS, osserviamo come questo corrisponde al modello che include
# tutti e 19 i predittori questo perché, aumentando il numero di predittori, diminuiamo l'errore
# sul traning set
which.min(reg.summary$rss)

# andiamo a vedere come si comportano statistiche adjusted.
cat("\n Location of RSS min: ", which.min(reg.summary$rss), "\n")
cat("\n Location of adj-RSQ max: ", which.max(reg.summary$adjr2), "\n")
cat("\n Location of Cp min: ", which.min(reg.summary$cp), "\n")
cat("\n Location of BIC min: ", which.min(reg.summary$bic), "\n")
# Osserviamo come con le statistiche adjusted, troviamo dei diversi miglior modelli.

# Plotting R2, Cp e Bic
dev.new()
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of variables", ylab = "RSS", type = "l")
points(which.min(reg.summary$rss), min(reg.summary$rss), col="red", cex = 2, pch = 20)

plot(reg.summary$adjr2, xlab = "Number of variables", ylab = "Adj-R2", type = 'l')
points(which.max(reg.summary$adjr2), max(reg.summary$adjr2), col="red", cex = 2, pch = 20)

plot(reg.summary$cp, xlab = "Number of variables", ylab = "Cp", type = 'l')
points(which.min(reg.summary$cp), min(reg.summary$cp), col="red", cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of variables", ylab = "BIC", type = 'l')
points(which.min(reg.summary$bic), min(reg.summary$bic), col="red", cex = 2, pch = 20)

# Utiliziamo adesso le funzioni di plot di regsubsets()
dev.new()
plot(regfit.full, scale = "r2")
dev.new()
plot(regfit.full, scale = "adjr2")
dev.new()
plot(regfit.full, scale = "Cp")
dev.new()
plot(regfit.full, scale = "bic")

# andiamo a plottare i coefficienti legati al modello con 6 regressori. 
coef(regfit.full, 6)
coef(regfit.full, which.min(reg.summary$bic))

### ---- Forward e Backward Stepwise Selection ---- ###
# Osserviamo come al variare della procedura il modello generato dalle diverse procedure può cambiare
regfit.fwd = regsubsets(Salary~., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)

regfit.bwd = regsubsets(Salary~., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)

#### ---- Hybrid Stepwise selection ---- ####
# L'approccio ibrido permette di aggiungere/rimuovere un determinato regressore sulla base della
# sua influenza sul parametro che si desidera migliorare
startmod = lm(Salary~1, data = Hitters) # modello con la sola intercetta
scopmod = lm(Salary~., data = Hitters) # modello con tutti i regressori.
# Adesso dobbiamo scegliere secondo quale parametro andare a valutare il modello
# Il comando step, partendo dal modello startmod, quindi attraverso l'aggiunta/rimozione dei vari
# regressori calcola il valore del AIC, quindi seleziona il modello con il miglior AIC
# L'algoritmo termina quando il miglior valore per l'AIC si ottiene senza inserire/togliere nessun valore
optmodAIC <- step(startmod, direction = "both", scope = formula(lm(Salary~., data = Hitters)))
extractAIC(optmodAIC)


optmodBIC <- step(startmod, direction = "both", scope = formula(lm(Salary~., data = Hitters)), k = log(n))
extractAIC(optmodBIC, k = log(n))

library(MASS)
optmodMASSA <- stepAIC(startmod, direction = "both", scope = formula(scopmod))
extractAIC(optmodMASSA, k = 2)

optmodMASSB <- stepAIC(startmod, direction = "both", scope = formula(scopmod), k = log(n))
extractAIC(optmodMASSB, k = log(n))

dev.new()
par(mfrow(2,2))
plot(optmodMASSB)

### ---- Validation Set Approach e Cross-Validation ---- ###
# Ricordiamo che adjusted-r2, Cp, BIC non sono altro che delle stime del
# potere predittivo del modello.
# Adesso vogliamo vedere il comportamente del modello, andando a separare il data set
# in una parte di training ed una parte di test.
# Quindi abbiamo un training set, e un validation set.

### Validation Set approach
# Dobbiamo dividere il data set in due componenti training e validation, che devono
# essere tra loro indipendenti, e quindi contenere sample diversi.
set.seed(1)
# Il campionamento è con reimmisione
# Il campionamento è fatto su TRUE e FALSE, quindi una riga appartiene al training set
# se train[i] == TRUE
train = sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test = !train

# applichiamo la procedura di Best subset Selection
regfit.best = regsubsets(Salary~., data = Hitters[train, ], nvmax = 19)

# Ci creiamo ora la matrice contenente i sample del test-set
test.mat = model.matrix(Salary~., data = Hitters[test,])

# Adesso vogliamo calcolare l'errore sul validation set.
# Obiettivo è trovare il miglior modello sul test-error tra i modelli ottenuti con la
# best-subset selection
val.err = rep(NA, 19)

for (i in 1:19){
  coefi = coef(regfit.best, id = i) # ci preleviamo l' i-esimo modello
  pred = test.mat[, names(coefi)] %*% coefi # eseguo una moltiplicazione rigaXcolonna, tra i sample del test-set e i coefficienti del modello i-esimo
  val.err[i] = mean((Hitters$Salary[test] - pred)^2) # sto calcolando il MSE
}
val.err
which.min(val.err) # Osserviamo come il modello che minimizza il test-MSE è quello con 7 predittori.

predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars]%*%coefi 
}

#### Cross-Validation approach.

k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace = TRUE)
cv.errors = matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

# Creaimo il for per la CV.
for(j in 1:k){
  best.fit = regsubsets(Salary~., data = Hitters[folds != j, ], nvmax = 19)
  for(i in 1:19){
    pred = predict(best.fit, Hitters[folds == j,], id = i)
    cv.errors[j,i] = mean((Hitters$Salary[folds == j]-pred)^2)
  }
}
# Abbiamo una matrice 10x19 dove l'elemento [i,j] rappresenta il test MSE per la i-th fold per
# il j-th miglior modello
mean.cv.errors = apply(cv.errors, 2, mean);
mean.cv.errors;
colMeans(cv.errors)
dev.new()
par(mfrow = c(1,1))
plot(mean.cv.errors, type = 'b')
 
reg.best = regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(reg.best, which.min(mean.cv.errors)) 
# Ci stiamo ricavando i coefficienti del modello che presenta
# il minor test error

#### ---- Ridge e Lasso Regression ---- ####
# Il pacchetto glmnet non utilizza la sintassi del comando lm
# ma vuole la matrice dei regressori x e il vettore del vettore predetto
library(glmnet)
x  = model.matrix(Salary~., Hitters)[, -1] # il comando matrix introduce anche le variabili dummy
y = Hitters$Salary

### Ridge
# Griglia dei valori per lambda
grid = 10^seq(10,-2, length = 100)
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid) # alpha rappresenta la tecnica di shrinkage
# 0 -> ridge; 1 -> lasso
# E' buona norma standardizzare le variabili quando non stiamo utilizzando strumenti diversi dallo lm classico
# osserviamo come glmnet lo fa automaticamente.
# Avremmo un set di coefficienti per ogni valore di lambda, che quindi verranno inseriti in una matrice
# dove la riga corrisponde al predittore, mentre la colonna rappresenta il coefficiente associato al predittore
# per il j-esimo valore di lambda
dim = coef(ridge.mod)

# Ci aspettiamo di vedere che all'aumentare di lambda il valore dei coefficienti tende verso 0, in quanto
# stiamo aumentando il potere di shrinkage.
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))

# Con il comando predict, possiamo calcolare i coefficienti per un nuovo valore di lambda
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]

# Testiamo il modello con la Cross-Validation
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]
# Fit di un ridge regression model, con lambda = 4.
ridge.mod = glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
# Valutiamo le predizioni del modello ridge.mod sul test set
ridge.pred = predict(ridge.mod, s = 4, newx = x[test, ])
mean((ridge.pred-y.test)^2) # 142199.2
# Calcoliamo adesso il test MSE nel caso in cui il modello fosse semplicemente rappresentato
# dall'intercetta, quindi il modello non farà altro che dare in output la media delle y.
mean((mean(y[train])-y.test)^2) # 224669.9
# Osserviamo come effettivamente l'introduzione di predittori aumentano la capacità predittiva del modello
# Osserviamo inoltre che avremmo avuto lo stesso comportamento avendo un lambda elevato
ridge.pred = predict(ridge.mod, s = 1e10, newx = x[test, ])
mean((ridge.pred-y.test)^2) # 224669.8
# Ricordiamo ancora che, ponendo lambda = 0, ciò che otteniamo è lo stesso modello che avremmo ottenuto
# con lm classico
# Adesso vogliamo capire se effettivamente l'introduzione di regolarizzazione aumenta la capacità predittiva
# Con il parametro "exact" andiamo ad indicare che bisogna rieseguire il train del modello, per il
# lambda indicato.
ridge.pred = predict(ridge.mod, s = 0,newx = x[test, ], exact = T, x = x[train, ], y = y[train])
mean((ridge.pred-y.test)^2) # 168588.6
# Osserviamo come effettivamente l'introduzione del termine di regolarizzazione, migliora le performance 
# predittive del modello.
# Confronto con lm
lm(y~x, subset = train)
# Osserviamo come a meno di piccole discrepanze i coefficienti calcolati sono uguali
predict(ridge.mod, s = 0, type = "coefficients", x = x[train, ], y = y[train])[1:20, ]

#### ---- Cross Validation ---- ####
# Adesso il nostro obiettivo è trovare il lambda che massimizza le prestazioni
set.seed(1)
# il comando cv.glmnet permette di effettuare la cross-validation
# di default k = 10
cv.out = cv.glmnet(x[train, ], y[train], alpha = 0)
dev.new()
# Il plot ci permette di osservare come varia il MSE al variare del log(lambda)
# L'intervallo di confidenza è generato utilizzando la stima della deviazione standard del MSE (sigma)
# Il plot ci dà sia MSE minimo stimato, più MSE minimo stimato + sigma
plot(cv.out)
bestlam = cv.out$lambda.min;
bestlam;
log(bestlam)
cv.out$lambda.1se
# Con la tecnica del CV abbiamo calcolato il lambda che genera il minor MSE test stimato
ridge.pred = predict(ridge.mod, s = bestlam, newx = x[test, ])
mean((ridge.pred-y.test)^2) # 139856.6

# Refit del modello, su tutti i dati con il miglior lambda
out = glmnet(x, y, alpha = 0)
# Osserviamo come in realtà non ci sono predittori che vanno a zero
predict(out, type="coefficients", s=bestlam)[1:20, ]
dev.new()
plot(out, label = T, xvar = "lambda")
# Osserviamo come dei 19 predittori, abbiamo solo 3/4 predittori veramente importanti, dato che tutti gli altri
# hano valori effettivamente bassi

#### ---- Lasoo ---- ####
lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda = grid)
dev.new()
# La norma L1 è massima quando lambda = 0, in questo caso stiamo effettuando semplicemente 
# una least-square, osserviamo come il Lasso effettua variable selection perché ci sono
# predittori che sono a 0 e si mantengono tali fino ad un certo punto
plot(lasso.mod, label = T)
dev.new()
plot(lasso.mod, label = T, xvar = "lambda")

# cross-validation, al fine di trovare il lambda che minimizza il MSE di test
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha = 1)
dev.new()
# Osserviamo come all'aumentare di lambda il numero di regressori nel modello tendono ad 1
# solo intercetta
plot(cv.out)

bestlam = cv.out$lambda.min
bestlam
log(bestlam)

# Valutiamo MSE-test
lasso.pred = predict(lasso.mod, s = bestlam, newx = x[test, ])
mean((lasso.pred-y[test])^2) # 143673.6
# leggermente più alto rispetto a quello ottenuto con la ridge-regression.
# Lasso ha il vantaggio che ha un numero di coefficienti del modello minori rispetto a quello ridge,
# quindi è più semplice
out = glmnet(x,y, alpha = 1, lambda = grid)
lasso.coef = predict(out, type="coefficients", s = bestlam)[1:20, ]
lasso.coef
lasso.coef[lasso.coef != 0]
cat("Number of coefficients equal to 0: ", sum(lasso.coef == 0), "\n")

# Confronto con lm
fit.lm = lm(Salary~AtBat+Hits+Walks+Years+CHmRun+CRuns+CRBI+League+Division+PutOuts+Errors, data = Hitters)
coef(fit.lm)

#### ---- PCR e PLS regression ---- ####
# Vogliamo adesso predire Salary con degli strumenti di proiezione delle features
# Iniziamo con la Principal Component Regression
library(pls)
set.seed(2)
# La cross-validation è utilizzata per determinare il numero di componenti M, che rappresentano lo
# spazio sul quale proiettare.
pcr.fit = pcr(Salary~., data = Hitters, scale = TRUE, validation = "CV")
# Il summary riporta il RMSEP al variare del numero delle componenti
# Riporta la varianza spiegata dal modello al variare del numero delle componenti riga con X
# Riporta la varianza spiegata dal modello tenendo in considerazione la varianza anche della y
summary(pcr.fit)

# Validation plot
dev.new()
# plottiamo il MSE legato ad ogni componente
validationplot(pcr.fit, val.type = "MSEP", legendpos = "topright")
which.min(MSEP(pcr.fit)$val[1,,][-1])
# Abbiamo come il numero di componenti che enerano il modello che presenta il minor MSEP è un 
# modello con 18 predittori, osserviamo quindi che sembra che utilizzare una strategia come la PCR
# non sia conveniente, però osserviamo ancora che il valore del MSEP che si ottiene per gli altri valori è 
# molto vicino a quello minimo, questo significa che si riesce ad ottenere un buon modello anche
# con un numero di regressori minori.

# Eseguiamo PCR sul training data e valutiamo il modello ottenuto sul test set.
set.seed(1)
# Osserviamo come adesso abbiamo indicato gli indici del set da prelevare, che sono quelli 
# nel vettore train
pcr.fit = pcr(Salary~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
dev.new()
validationplot(pcr.fit, val.type = "MSEP", legendpos = "topright")
minPCR = which.min(MSEP(pcr.fit)$val[1,,][-1])
minPCR # 5

# Calcoliamo il test MSE
pcr.pred = predict(prc.fit, x[test, ], ncomp = minPCR)
mean((pcr.pred-y.test)^2) # 142811.8
# Il valore ottenuto è simile a quello ottenuto con quello Ridge e Lasso, solo che però adesso
# stiamo ottenendo questo risultato con un modello che ha 5 regressori.

# Eseguiamo il fit sull'intero dataset
pcr.fit = pcr(y~x, scale = TRUE, ncomp = which.min(MSEP(pcr.fit)$val[1,,][-1]))
summary(pcr.fit)
dev.new()
validationplot(pcr.fit, val.type = "MSEP", legendpos = "topright")
dev.new()
# Con questo plot stiamo rappresentando la relazione che c'è tra i valori predetti dal nostro modello
# (linea) e quelli presenti nel dataset, osserviamo come il desiderio sarebbe avere i dati il più vicino
# possibile alla linea.
plot(pcr.fit, ncomp = 5, asp = 1, line = TRUE)
coef(pcr.fit)
dev.new()
plot(pcr.fit, plottype = "scores", comps = 1:3)
explvar(pcr.fit)

#### ---- PLS ---- ####
# Partial Least Square, concetto simile alla PCR, solo che in questo caso si scelgono le componenti
# che non solo spiegano la varianza dei regressori, ma si prende in considerazione anche la varianza
# della variabile y.
set.seed(1)
pls.fit = plsr(Salary~., data = Hitters, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
which.min(MSEP(pls.fit)$val[1,,][-1]) #  11

# fit sul trining set
set.seed(1)
pls.fit = plsr(Salary~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
which.min(MSEP(pls.fit)$val[1,,][-1]) # 1
# Dato che avere una sola componente nel nostro modello può essere troppo poco
# in quanto possono esistere legami dati dal dataset stesso, quello che facciamo
# è calcolare il test MSE in un intorno di due
pls.pred = predict(pls.fit, x[test, ], ncomp = 2)
mean((pls.pred-y.test)^2) # 145367.7
pls.pred = predict(pls.fit, x[test, ], ncomp = 1)
mean((pls.pred-y.test)^2) # 151995.3
pls.pred = predict(pls.fit, x[test, ], ncomp = 3)
mean((pls.pred-y.test)^2) # 152994.6
# Osserviamo come il minimo lo otteniamo per ncomp = 2, quindi costruiamo il modello con 2 componenti
pls.fit = plsr(Salary~., data = Hitters, scale = TRUE, ncomp = 2)
summary(pls.fit)
# Interessante è osservare che con solo 2 componenti siamo in grado di spiegare il 46% della variabile di y,
# mentre con la PCR con 5 componenti siamo in grado di misurare il 44% della variazione della variabile y.
# Quindi l'idea della PLS è quella di avere una variante supervisionata del PCR.
