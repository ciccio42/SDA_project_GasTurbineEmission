#### --- Data Analysis ---- ####
ds1 = read.csv("Gas_turbine/gt_2011.csv")
ds2 = read.csv("Gas_turbine/gt_2012.csv")
ds3 = read.csv("Gas_turbine/gt_2013.csv")
ds4 = read.csv("Gas_turbine/gt_2014.csv")
ds5 = read.csv("Gas_turbine/gt_2015.csv")

ds = rbind(ds1, ds2)
ds = rbind(ds, ds3)
ds = rbind(ds, ds4)
ds = rbind(ds, ds5)


write.csv(ds,"dataset.csv", row.names = FALSE)

attach(ds)
#### --- Features list ---- ####
# Predictors: 
# AT -> Ambient Temperature [°C]
# AP -> Ambient Pressure [mbar]
# AH -> Ambient Humidity [%]
# AFDP -> Air filter difference pressure [mbar]
# GTEP -> Gas turbine exhaust pressure [mbar]
# TIT -> Turbine inlet temperature [°C]
# TAT -> Turbine after temperature [°C]
# CDP -> Compressor discharge pressure [mbar]
# TEY -> Turbine energy yield  [MWH]
# Responses:
# CO -> Carbon Monoxide [mg/m^3]
# NOx -> Nitrogen Oxides [mg/m^3]
names(ds)

#### ---- Summary ---- ####
summary(ds)

#### ---- Histograms --- ####
dev.new()
par(mfrow = c(2, 3))
hist(ds$AT);hist(ds$AP);hist(ds$AH);hist(ds$AFDP);hist(ds$GTEP);hist(ds$TIT);

dev.new()
par(mfrow = c(2,3))
hist(ds$TAT);hist(ds$TEY);hist(ds$CDP);hist(ds$CO);hist(ds$NOX)

#### ---- Boxplots ---- ####
# Dai boxplot si ricavano informazioni simili a quelle degli istogrammi, in particolare
# si può osservare la presenza di eventuali outliers. Diciamo eventuali in quanto la presenza di quei punti
# non fa riferimento ad errori di prelievo, ma a condizioni di misurazione che differiscono da quelle nominali (più frequenti)
# della turbina
dev.new()
par(mfrow = c(2, 3))
boxplot(ds$AT, ylab = "AT", horizontal = TRUE);boxplot(ds$AP, ylab = "AP", horizontal = TRUE);boxplot(ds$AH, ylab = "AH", horizontal = TRUE);boxplot(ds$AFDP, ylab = "AFDP", horizontal = TRUE);
boxplot(ds$GTEP, ylab = "GTEP", horizontal = TRUE);boxplot(ds$TIT, ylab = "TIT", horizontal = TRUE);

dev.new()
par(mfrow = c(2,3))
boxplot(ds$TAT, ylab = "TAT", horizontal = TRUE);boxplot(ds$TEY, ylab = "TEY", horizontal = TRUE);boxplot(ds$CDP, ylab = "CDP", horizontal = TRUE);
boxplot(ds$CO, ylab = "CO", horizontal = TRUE);boxplot(ds$NOX, ylab = "NOX", horizontal = TRUE)

#### ---- Correlation Matrix ---- ####
# Si osserva la preseza di legami, anche forti tra i predittori stessi, e tra predittori e risposte
# In particolare, come era intuibile, le due risposte sono legate a fattori meccanici della turbina
# e non a quelli ambientali. 
# Intuizione #1: I regressori relativi a variabili ambientali possono introdurre rumore, degradazione della qualità della predizione
# Intuizione #2: Eseguendo la regressione senza tener conto delle correlazioni, che esistono già a livello
# di solo coppie di variabili, andiamo incontro al problema della collinearità (VIF elevato)
# C0 è correlato a: AFDP, TIT, GTEP, TEY, CDP
# NOX particolarmetnte correlato con AT ed in maniera minore con i predittori elencati sopra
library(corrplot)
library(RColorBrewer)
cor <-cor(ds)
dev.new()
corrplot(cor, type="upper", order="hclust",col=brewer.pal(n=8, name="RdYlBu"))

#### ---- Scatter Plots ---- ####
# Conferma dei ragionamenti di sopra.
# Si osserva che tra la risposta e i regressori può esistere un qualche legame non strettamente lineare
dev.new()
pairs(ds)

dev.new()
plot(TIT, CO)
dev.new()
plot(GTEP, CO)
dev.new()
plot(TEY, CO)
dev.new()
plot(CDP, CO)

x = c(1:nrow(ds))
train = (nrow(ds)*70)/100
test = (nrow(ds)*30)/100

set.seed(1)
train_set = sample(x, train, rep = FALSE)
test_set = sample(x[-train_set], test, rep = FALSE)

ds_train = ds[train_set, ]
ds_test = ds[test_set, ]

write.csv(ds_train, "ds_train.csv", row.names = FALSE)
write.csv(ds_test, "ds_test.csv", row.names = FALSE)
