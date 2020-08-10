ds_1_year = read.csv("Gas_turbine/gt_2011.csv")
attach(ds_1_year)


#### ---- 1. Data Set Analysis ---- ####
summary(ds_1_year)
dev.new()
pairs(ds)

#### ---- Histograms ----- ####
dev.new()
pairs(ds)
summary(ds)
dev.new()
hist(AT)
dev.new()
hist(TAT)
dev.new()
hist(AH)
dev.new()
hist(AP)
dev.new()
hist(TIT)
dev.new()
hist(AFDP)
dev.new()
hist(TEY)
dev.new()
hist(GTEP)
dev.new()
hist(CDP)

#### ---- Box plots ---- ####

dev.new()
boxplot(AT)
dev.new()
boxplot(TAT)
dev.new()
boxplot(AH)
dev.new()
boxplot(AP)
dev.new()
boxplot(TIT)
dev.new()
boxplot(AFDP)
dev.new()
boxplot(TEY)
dev.new()
boxplot(GTEP)
dev.new()
boxplot(CDP)

#### ---- Correlation-matrix ---- ####
library(corrplot)
library(RColorBrewer)
cor <-cor(ds)
dev.new()
corrplot(cor, type="upper", order="hclust",col=brewer.pal(n=8, name="RdYlBu"))


#### ---- 2. Linear Fit ---- ####
fit = lm(CO~.-NOX, data = ds_1_year)
summary(fit)
library(car)

# Problema di collinearità
vif(fit)

# Problema di punti di leverage che outliners
dev.new()
par(mfrow = c(2,2))
plot(fit)
