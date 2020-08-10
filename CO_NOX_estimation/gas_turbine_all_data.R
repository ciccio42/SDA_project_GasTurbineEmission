ds1 = read.csv("Gas_turbine/gt_2011.csv")
ds2 = read.csv("Gas_turbine/gt_2012.csv")
ds3 = read.csv("Gas_turbine/gt_2013.csv")
ds4 = read.csv("Gas_turbine/gt_2014.csv")
ds5 = read.csv("Gas_turbine/gt_2015.csv")

ds = rbind(ds1, ds2)
ds = rbind(ds, ds3)
ds = rbind(ds, ds4)
ds = rbind(ds, ds5)

#### ---- 1. Data Set Analysis ---- ####
#### ---- Summary ---- ####
summary(ds)
attach(ds)
#### ---- Pairs ---- ####
dev.new()
pairs(ds)

#### ---- Correlation Matrix ---- ####
library(corrplot)
library(RColorBrewer)
cor <-cor(ds)
dev.new()
corrplot(cor, type="upper", order="hclust",col=brewer.pal(n=8, name="RdYlBu"))


fit = lm(CO~.-NOX, data = ds)
summary(fit)
