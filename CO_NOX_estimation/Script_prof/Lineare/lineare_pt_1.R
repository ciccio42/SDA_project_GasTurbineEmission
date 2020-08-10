############### R Lab: linear regression ###############
############### Simulate Data ################
## Suppose true model: y=f(x)+epsilon, where f(x)=2+3x+x^3
## where x~Uniform(-3,3), y|x~N(f(x), 2^2), i.e. epsilon~N(0, 2^2).

set.seed(2020) # This way the results will be reproducible

# stiamo definendo la funzione per generare la F(x) con l'espressione
# di sopra
f = function(x){
  return(2+3*x+x^3)
}

vv = 4
n = 100 
x = runif(n, -3, 3) # produciamo n valori uniformemente distribuiti.
y = rep(0, n) # stiamo creando un vettore di 100 elementi contenente tutti 0.
for(i in 1:n){
  y[i] = rnorm(1, f(x[i]), sqrt(vv)) # stiamo riempiendo il vettore delle nostre osservazioni
                                     # distribuite secondo una normale di media il valore f(x) e varianza 2
                                     # stiamo aggiungendo il termine di errore.
}
d = data.frame(feature=x, response=y) ## create data frame
#dev.new()
plot(d$feature, d$response) ## scatter plot
xx = seq(-3, 3, length.out = 300) # genera una sequenza di 300 elementi da -3 a -3
lines(xx, f(xx),col="blue") ## true curve
## fit a simple linear model 
fit1 = lm(response~feature, data=d) # 'response~feature' rappresenta la nostra formula.
                                    # response rappresenta la y, feature rappresenta la x, quindi il primo regressore;
                                    # nel caso abbiamo più regressori questi vanno aggiuti con +
fhat_of_xx = (cbind(1, xx)%*%fit1$coefficients) # stiamo eseguendo il prodotto matriciale tra
                                                # i punti della sequenza xx e i coefficienti della funzione che abbiamo calcolato.
                                                # cbind(1, xx) : stiamo concatendando per colonne 1 e i punti della sequenza,
                                                # 1 va moltiplicato con il coefficiente beta_0
                                                # il parametro coefficients di fit1 è un vettore di 2 colonne.
                                                
lines(xx, fhat_of_xx, col="red") ## fitted line
legend('topleft', c('points','true curve','fitted line'), lty=c(NA,1,1), col=c('black','blue',"red"), pch=c(1,NA,NA),cex=0.8)


# Dati i dati sperimentali, vogliamo capire come performa il nostro modello su questi dati
# quindi dividiamo il data_set in due parti una di training e uno di set;

## split data into training and test
indx = sample(n, n/2) # preleva dal range 1:100, 50 elementi; 
d_train = d[indx,] # preleviamo i valori alle righe indicizzate da index;
d_test = d[-indx,] # preleviamo i valori che non sono indicizzate da index;

# Proviamo ad eseguire un fit con un modello polinomiale.

## calcuate training and test MSEs by fitting polynomial of order 1, ..., poly.order
poly.order = 10 # massimo grado raggiungibile

mse_train = rep(NA, poly.order) # save train MSE to a vector
mse_test = rep(NA, poly.order) # save test MSE to a vector

for(i in 1: poly.order){
  fit = lm(response~poly(feature,i), data=d_train); ## see ?poly() how it is defined
  mse_train[i]=mean((d_train$response-fit$fitted.values)^2); # stiamo calcolando il MSE di training
  predicted_test = predict(fit, newdata = d_test); ## predicted values on test data
  mse_test[i]=mean((d_test$response-predicted_test)^2);
}
poly_order=1:poly.order
plot(poly_order, mse_train, "l", lwd=2);
lines(poly_order, mse_test, lty=2, lwd=2)
abline(a=vv,b=0, lty=3)
## find the polynomial order that yields the smallest mse
which.min(mse_train)
which.min(mse_test)

#### repeat above for many times and take the average
sim.repeat = function(nsim = 50){
  poly.order = 10
  mat.test = matrix(NA, nsim, poly.order)
  mat.train = matrix(NA, nsim, poly.order)
  for(sim in 1:nsim){
    f = function(x){
      return(2+3*x+x^3)
    }
    n = 100 
    x = runif(n, -3, 3)
    y = rep(0, n)
    for(i in 1:n){
      y[i] = rnorm(1, f(x[i]), sqrt(vv))
    }
    d = data.frame(feature=x, response=y) ## create data frame
    #plot(d$feature, d$response) ## scatter plot
    xx = seq(-3, 3, length.out = 300)
    #lines(xx, f(xx)) ## true curve
    ## fit a simple linear model 
    fit1 = lm(response~feature, data=d)
    fhat_of_xx = (cbind(1, xx)%*%fit1$coefficients)
    #lines(xx, fhat_of_xx, col="red") ## fitted line
    ## split data into training and test
    indx = sample(n, n/2);
    d_train = d[indx,];
    d_test = d[-indx,];
    ## calcuate training and test MSEs by fitting polynomial of order 1, ..., poly.order
    mse_train = rep(NA, poly.order) # save train MSE to a vector
    mse_test = rep(NA, poly.order) # save test MSE to a vector
    for(i in 1: poly.order){
      fit = lm(response~poly(feature,i), data=d_train); ## see ?poly() how it is defined
      mse_train[i]=mean((d_train$response-fit$fitted.values)^2);
      predicted_test = predict(fit, newdata = d_test); ## predicted values on test data
      mse_test[i]=mean((d_test$response-predicted_test)^2);
    }
    mat.train[sim, ] = mse_train
    mat.test[sim, ] = mse_test
    ## find the polynomial order that yields the smallest mse
    #which.min(mse_train)
    #which.min(mse_test)
  }
  poly_order=1:poly.order
  plot(poly_order, colMeans(mat.train), "l", lwd=2,xlim=c(0,11),ylim=c(0,40));
  lines(poly_order, colMeans(mat.test), lty=2, lwd=2)
  abline(a=vv,b=0, lty=3)
  cat("\nPolynomial degree with minimum MSE_train:",which.min(colMeans(mat.train)),"\n")
  cat("\nPolynomial degree with minimum MSE_test:",which.min(colMeans(mat.test)),"\n")
}

sim.repeat()

