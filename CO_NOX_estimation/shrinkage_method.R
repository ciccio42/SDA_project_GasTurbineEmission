#### ---- Shrinkage Method ---- ####
# Gli schrinkage methods (Ridge e Lasso) vengono applicati in un contesto in cui
# p << n e dove il dataset presenta problemi di multicollinearità.
# Questo significa che in generale i metodi di shrinkage dovrebbero darci prestazioni migliori
# rispetto al fit Least Square che comprende tutti i regressori.

#### ---- Library ---- ####
ds = read.csv("dataset.csv")
ds_train = read.csv("ds_train.csv")
ds_test = read.csv("ds_test.csv")
library(glmnet)

x_train  = model.matrix(CO~., ds_train)[, -1]
y_train = ds_train$CO

x_test  = model.matrix(CO~., ds_test)[, -1]
y_test = ds_test$CO
# Vettore dei coefficienti lambda
grid = 10^seq(10,-2, length = 100)

# Sia la tecnica ridge che la tecnica lasso hanno un coefficiente lambda da determinare
# la tecnica utilizzata per la stima del lambda migliore è la Cross Validation.

# Sul training andiamo ad eseguire la CV per individuare il lambda miglire
# Sul test andremmo a valutare il modello ottenuto per fare un confronto Ridge vs Lasso

#### ---- Ridge Regression ---- ####
# K = 10
set.seed(1)
cvridge.mod = cv.glmnet(x_train, y_train, alpha = 0, lambda = grid)

# Osserviamo come all'aumentare del log(lamda) quindi all'aumentare di lamda aumenta il MSE di test predetto
# questo significa che andando a ridurre l'importanza di alcuni regressori si ottiene una capacità di predizione
# peggiore
dev.new()
plot(cvridge.mod)
# Osserviamo come il lambda a 1se è pari a 0.6579332,
# ancora capiamo che l'influenza dell'indice di regolarizzazione è praticamente inutile
bestlambda_ridge = cvridge.mod$lambda.min
lambda1SE_ridge = cvridge.mod$lambda.1se

# Il miglior lambda si ottiene su 0.01, praticamente pari a 0, cioè pari alla regressione least square classica
# Ciò si potrebbe giustificare con il fatto che i coefficienti del modello least square con tutti i regressori
# sono già relativamente bassi.
lm_all = lm(CO~., data = ds_train)
coef(lm_all) # coefficienti modello Least Square
coef(cvridge.mod, bestlambda_ridge) # coefficienti del modello per il lambda migliore

# Osserviamo come i coefficienti partono da valori piccoli, quindi all'aumentare del lambda si vanno
# a stringere.
ridge.mod = glmnet(x_train, y_train, alpha = 0, lambda = grid)
dev.new()
plot(ridge.mod, label = T, xvar = "lambda")

#### ---- Lasso ---- ####
# Si osserva come il fattore di regolarizzazione lambda rimane piccolo
# però adesso si può osservare la capacità di effettuare variable selection 
# della tecnica Lasso, infatti osserviamo come in corrispondenza di lambda+1SE
# i coefficienti irrilevanti si portano a 0, mantenendo diversi da 0 solo
# TIT e TAT, andando a confermare le intuizioni fatte in least_square.R
set.seed(1)
cvlasso.mod = cv.glmnet(x_train, y_train, alpha = 1, lambda = grid)
dev.new()
plot(cvlasso.mod)

bestlambda_lasso = cvlasso.mod$lambda.min # 0.01
lambda1SE_lasso = cvlasso.mod$lambda.1se # 0.2154435

coef(cvlasso.mod, bestlambda_lasso) # coefficienti del modello per il lambda migliore
coef(cvlasso.mod, lambda1SE_lasso) # coefficienti del modello per il lambda + 1SE

lasso.mod = glmnet(x_train, y_train, alpha = 1, lambda = grid)
dev.new()
plot(lasso.mod, label = T, xvar = "lambda")

#### ---- Best Lasso vs Best Ridge ---- ####
# Andiamo a confrontare il Best Ridge (praticamente uguale al Least Square) e il Best Lasso
# al fine di valutare le prestazioni di predizione sul Test Set
# Osserviamo come le performance tra Ridge e Lasso sono tra loro paragonabili
# quindi con Lasso con solo due coefficienti raggiungiamo prestazioni simili a quello
# ridge
lasso.pred = predict(lasso.mod, s = lambda1SE_lasso, newx = x_test)
ridge.pred = predict(ridge.mod, s = bestlambda_ridge, newx = x_test)
lasso.pred_mse = mean((lasso.pred-y_test)^2) #  2.47327
ridge.pred_mse = mean((ridge.pred-y_test)^2) #  2.23

lm_all = lm(CO~., data = ds_train)
lm_all_pred = predict(lm_all, newdata = ds_test)
lm_all_mse = mean((y_test - lm_all_pred)^2)
coef(lm_all)
coef(ridge.mod, lambda1SE_ridge)

lasso.pred = predict(lasso.mod, s = lambda1SE_lasso, newx = x_train)
ridge.pred = predict(ridge.mod, s = lambda1SE_ridge, newx = x_train)
lasso.pred_mes_tr = mean((lasso.pred-y_train)^2) # 2.452729
ridge.pred_mes_tr = mean((ridge.pred-y_train)^2) # 2.431468

#### ---- Introduzione del polinomio ---- ####
# Fissata la presenza di una relazione non lineare tra i regressori e l'uscita
# si andrà a provare il ridge e lasso shrinkage method sul modello che introduce anche
# il polinomio sulla variabile TIT, aspettandoci di osservare un comportamento simile
# a quello di sopra, quindi con un fattore di regolarizzazione basso per Ridge, in quanto
# dalla analisi sul least square già si poteva osservare come il polinomio dominava gli altri
# coefficienti che erano molto vicino a zero, nel caso del Lasso, ci aspettiamo la selezione del solo polinomio
# dato comunque un fattore di smorzamento basso
x_train_poly  = model.matrix(CO~.-TIT+poly(TIT,3), ds_train)[, -1]
y_train_poly = ds_train$CO

x_test_poly  = model.matrix(CO~.-TIT+poly(TIT,3), ds_test)[, -1]
y_test_poly = ds_test$CO

#### ---- Ridge + Polinomio ---- ####
# K = 10
set.seed(1)
cvridge_poly.mod = cv.glmnet(x_train_poly, y_train_poly, alpha = 0, lambda = grid)
# come era lecito aspettarsi il lambda migliore si trova per valori del log(lambda) negativi
# cioè per valori di lambda vicini allo zero
dev.new()
plot(cvridge_poly.mod)

bestlambda_ridge_poly = cvridge_poly.mod$lambda.min #  0.01
lambda1SE_ridge_poly = cvridge_poly.mod$lambda.1se # 1.149757
# Coefficienti praticamente uguali a quelli del modello lineare
coef(cvridge_poly.mod, bestlambda_ridge_poly) # coefficienti del modello per il lambda migliore

ridge_poly.mod = glmnet(x_train_poly, y_train_poly, alpha = 0, lambda = grid)
dev.new()
plot(ridge.mod, label = T, xvar = "lambda")

#### ---- Lasso + Polinomio ---- ####
# K = 10
set.seed(1)
cvlasso_poly.mod = cv.glmnet(x_train_poly, y_train_poly, alpha = 1, lambda = grid)
# Osserviamo come il lambda migliore si ottiene per valori piccoli, interessante è osservare
# il lambda+1SE, infatti per quel valore di lambda otteniamo prestazioni sul test set paragonabili
# ma con un modello con soli 3 regressori
dev.new()
plot(cvlasso_poly.mod)

bestlambda_lasso_poly = cvlasso_poly.mod$lambda.min # 0.01
lambda1SE_lasso_poly = cvlasso_poly.mod$lambda.1se # 0.2154435

# Osserviamo come i coefficienti diversi da zero sono proprio quelli legati al polinomio
coef(cvlasso_poly.mod, lambda1SE_lasso_poly) 

lasso_poly.mod = glmnet(x_train_poly, y_train_poly, alpha = 1, lambda = grid)
dev.new()
plot(lasso.mod, label = T, xvar = "lambda")

#### ---- Best Lasso+Poly vs Best Ridge+Poly ---- ####
# Osserviamo ancora come con l'aggiunta del polinomio ci permette di ottenere ancora
# un lieve miglioramento delle performance a giustificazione ancora del fatto come
# il regressore TIT è fortemente legato all'uscita e come questo è legato in maniera non lineare
# alla stessa.
# Osserviamo come il lasso con la presenza di soli 3 preditorri (il polinomio cubico) ha
# prestazioni che si pongono tra quelle del ridge con tutti i regressori (bestlambda_ridge_poly)
# e quello con un termine di regolarizzazione pari al migliore più 1SE 
lasso_poly.pred = predict(lasso_poly.mod, s = lambda1SE_lasso_poly, newx = x_test_poly)
lasso_poly.pred_mse = mean((lasso_poly.pred-y_test_poly)^2) # 2.21 

ridge_poly.pred = predict(ridge_poly.mod, s = lambda1SE_ridge_poly, newx = x_test_poly)
ridge_poly.pred_mse = mean((ridge_poly.pred-y_test_poly)^2) # 1.967836

ridge_poly.pred = predict(ridge_poly.mod, s = bestlambda_ridge_poly, newx = x_test_poly)
ridge_poly.pred_mse = mean((ridge_poly.pred-y_test_poly)^2) # 2.33
