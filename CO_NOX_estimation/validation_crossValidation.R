#### ---- Validation, Cross Validation ---- ####
# Dal file least_square abbiamo prodotto 4 modelli: fit_all, fit1, fit_poly, fit1.poly2, fit1.poly3
# Questi modelli sono stati valutati attraverso un Validation Set Approach al fine di stimare MSE di test
# I problemi del Validation Set approach sono: 1. Elevata Variabilità del MSE test, 2. Riduzione del set di training
# Adesso si vogliono utilizzare le tecniche di Leave-one-out e k-fold Cross Validation al fine di mettere a confronto
# i 4 modelli di sopra
# Validation Set Approach, Leave-one-out, K-fold CV (sovrastima MSE di test)

