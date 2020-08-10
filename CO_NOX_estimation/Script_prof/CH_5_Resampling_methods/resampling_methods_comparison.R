# generazione dati con 

set.seed(2020) # This way the results will be reproducible

# funzione per la creazione artificiale di sample.
f = function(x){
  return(2+3*x+x^3)
}

vv = 4
n = 300 
x = runif(n, -3, 3) # produciamo n valori uniformemente distribuiti.
y = rep(0, n) # stiamo creando un vettore di 100 elementi contenente tutti 0.
for(i in 1:n){
  y[i] = rnorm(1, f(x[i]), sqrt(vv)) # stiamo riempiendo il vettore delle nostre osservazioni
  # distribuite secondo una normale di media il valore f(x) e varianza 2
  # stiamo aggiungendo il termine di errore.
}

d_ideale = data.frame(feature=x, response=y) ## create data frame
#dev.new()
plot(d_ideale$feature, d_ideale$response) ## scatter plot
xx = seq(-3, 3, length.out = 300) # genera una sequenza di 300 elementi da -3 a -3
lines(xx, f(xx),col="blue")


# divisione del dataset in tre parti
# training, validation e test.
dev.new()
par(mfrow = c(2,2))

n = nrow(d_ideale)
train_indici = sample(n, n/3)
validation_indici = sample(n[-train_indici], n/3)
test_indici = sample(n[-train_indici-validation_indici], n/3)

d_train = d_ideale[train_indici,]
d_validation = d_ideale[validation_indici,]
d_test = d_ideale[test_indici,]

n_poly = 10

mse_training = rep(0, n_poly)
mse_validation = rep(0,n_poly)

for(i in 1:n_poly){
  
  lm.fit = lm(response~poly(feature, i), data = d_train)
  mse_training[i] = mean((d_train$response-lm.fit$fitted.values)^2) 
  plot(lm.fit)
  
  validation_prediction = predict(lm.fit, newdata = d_validation)
  mse_validation[i] = mean((d_validation$response - validation_prediction)^2)

}
mse_training
mse_validation
