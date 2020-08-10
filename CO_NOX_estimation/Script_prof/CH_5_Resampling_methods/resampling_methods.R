###############################
# The Validation set approach #
###############################

# Vogliamo utilizzare il validation set approach per stimare 
# l'errore di test su differenti modelli lineari

library(ISLR)
attach(Auto)

n = nrow(Auto)

set.seed(2020)
# selezioniamo 196 indici, tra interi che vanno da 1 a 392
train = sample(1:n,n/2)

# Il modello lm1 viene allenato solo su un sottoinsime di sample caratterizzati
# dagli indici generati con sample
lm1 = lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg-predict(lm1,Auto))[-train]^2)

# iniziamo ad allenare differenti modelli
lm2 = lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean ((mpg - predict (lm2 ,Auto ))[- train ]^2)

lm3=lm(mpg~poly( horsepower ,3) ,data=Auto , subset =train )
mean ((mpg - predict (lm3 ,Auto ))[- train ]^2)

# ripetiamo i fit con dei dati diversi
set.seed(2019)
train = sample(1:n,n/2)

lm1 = lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg-predict(lm1,Auto))[-train]^2)

estimate_prediction_error = rep(0,5)

for (i in 1:5){
  set.seed(i)
  train = sample(1:n,n/2)
  lm1 = lm(mpg~horsepower, data = Auto, subset = train)
  estimate_prediction_error[i] = mean((mpg-predict(lm1,Auto))[-train]^2)
}

mean_predict = mean(estimate_prediction_error)
mean_predict

lm2 = lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean ((mpg - predict (lm2 ,Auto ))[- train ]^2)

lm3=lm(mpg~poly( horsepower ,3) ,data=Auto , subset =train )
mean ((mpg - predict (lm3 ,Auto ))[- train ]^2)


##################################
# Leave One out Cross Validation #
##################################

# glm permette il fit di modelli lineari generalizzati

glm.fit = glm(mpg~horsepower, data= Auto)
coef(glm.fit)

# otteniamo lo stesso risultato perché se non passiamo niente al parametro family di
# glm allora fitto un modello lineare semplice.
lm.fit = lm(mpg~horsepower, data= Auto)
coef(lm.fit)

library(boot)

# cv calcola la stima dell'errore di predizione attraverso la tecnica del K-fold cross validation
# per i modelli lineari generalizzati
?cv.glm

# se non specifichiamo niente otteniamo la LOOCV
# la funzione di costo standard se niente è specificato è la MSE.
glm.fit = glm(mpg~horsepower, data= Auto)
cv.err = cv.glm(Auto, glm.fit)
# il primo elemento di delta è la stima dell'errore di predizione
cv.err$delta

# adesso vogliamo utilizzare questa tecnica di stima dell'errore di predizione
# per selezionare il modello caratterizzato dalla stima minima

cv.err = rep(0,5)

# osserviamo che rispetto all'approcci sopra riportato, con la tcnica del LOOCV abbiamo
# una stima dell'errore di predizione più stabile, infatti se ripetuta otteniamo gli stessi
# risultati.
# Osserviamo che il minimo si ottiene quando il polinomio assume grado di ordine 2.
for ( i in 1:5){
  glm.fit = glm(mpg~poly(horsepower,i), data = Auto)
  cv.err[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.err


###########################
# K-fold Cross Validation #
###########################

set.seed(17)
cv.error.10 = rep(0,10)
for( i in 1:10){
  glm.fit = glm(mpg~poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10


#############
# Bootstrap #
#############

# function that computes the statistic of interest.
# la fuzione ritorna una stima del parametro alpha sulla base
# dei dati passati come input.
alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

# torniamo una valutazione del parametro 
# alpha sulla base dei primi 100 sample presenti in Portfolio
alpha.fn(Portfolio, 1:100)

# proviamo a randomizzare la prova, prendendo sample a caso
set.seed(2020)
alpha.fn(Portfolio, sample(100, 100, replace = T))

# per poter implementare un'analisi di tipo bootstrap noi possiamo
# rieseguire N volte la procedura sopra riportata.
# Ad ogni nuova chiamata ci possiamo conservare la stima di alpha,
# così che alla fine delle N chiamate ci possiamo calcolare
# la media dell'alpha stimato e la deviazione standard.

# Esiste la funzione boot() che può automatizzare la procedura di sopra
boot(data = Portfolio, statistic = alpha.fn, R = 1000)

########################################################
# Estimating the accuracy of a linear regression model #
########################################################

# In questa prova andremmo ad utilizzare l'approccio bootstrap 
# al fine di ottenere una stima della variabilità dei coefficienti di un
# modello lineare, così da compararli con le stime provenienti da procedure
# statistiche, SE(beta_0) e SE(beta_1)

boot.fn = function(data, index){
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}

boot.fn(Auto, 1:392)


# la funzione boot può quindi essere utilizzata per la stima dei coefficienti,
# sia per implementare la procedura bootstrap.

set.seed(1)
boot.fn(Auto, sample(1:392, 392, replace = T))
boot.fn(Auto, sample(1:392, 392, replace = T))

# Utiliziamo la funzione boot per ottenere la stima dei coefficienti 
# e della deviazione standard
boot(data = Auto, statistic = boot.fn, R = 1000)
# confrontiamo i risultati ottenuti con il bootstrap con quelli
# ottennuti dalla statistica classica del metodo
summary(lm(mpg ~ horsepower, data = Auto))$coef

# Osserviamo che la stima data dalla tecnica del bootstrap e quella data dal summary
# sono diverse, questo cosa significa?
# Ovviamente un prima giustificazione va trovata nel fatto che comunque la tecnica bootstrap
# è una tecnica approssimata, però in questo caso c'è qualcosa di più sottile.
# Infatti ricordiamo che la relazione tra horsepower e mpg non è propriamente lineare,
# questo significa che la stima dello standard error, che ricordiamo dipende dalla stima
# della varianza del termine di errore che a sua volta dipende dalla somma dei residui, non sarà 
# "realistica" in quanto il modello da noi scelto non descrive la vera relazione tra il regressore
# e la risposta y.
# Quindi possiamo osservare come, in realtà, la stima "più realistica" dell'effettivo errore standard
# è data proprio dal risultato del bootstrap. Il quale considera anche il fatto che le feature X non sono fisse e non fa
# ulteriori assunzioni come la distribuzione dell'errore etc..

boot.fn = function(data, index){
  return(coef(lm(mpg ~horsepower + I(horsepower^2), data = data, subset = index)))
}

set.seed(1)
# Osserviamo come cambiando il modello, che descrive meglio la relazione tra risposta
# e regressori 
boot(data = Auto, statistic = boot.fn, R = 1000)
summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))
