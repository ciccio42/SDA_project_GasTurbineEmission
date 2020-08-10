# Obiettivo predirre le vendite sulla base degli investimenti 
# sulle piattaforme televisive, radio e giornali.

advertising = read.csv("Advertising_FP.csv")

attach(advertising)

names(advertising)

pairs(advertising)

# iniziamo a stimare le vendite con il primo regressore che sembra avere un qualche
# legame con le vendite, quindi gli investimenti in pubblicità televisiva.

fit_sales_tv = lm(formula = sales~TV, data = advertising)

dev.new()
plot(TV,sales)
abline(fit_sales_tv)
summary(fit_sales_tv)

fit_sales_radio = lm(formula = sales~radio, data = advertising)
fit_sales_newspapre = lm(formula = sales~newspaper, data = advertising)
summary(fit_sales_radio)