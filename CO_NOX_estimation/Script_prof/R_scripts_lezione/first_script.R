x = c(1,3,2,5) #create a vector
x

x <- c(1,3,2,5) #create a vector
x

y = seq(from=4, length=4, by=1)
y
y = seq(4,7)
y
y = 4:7
y
?seq

length(x)
length(y)
x+y #sum
x/y #division element-wise
rm(x)
ls() #var in memory
rm(list=ls()) # eliminate all vars in memory
ls()

?matrix
x = matrix(data=c(1,2,3,4), nrow=2, ncol=2)
x = matrix(c(1,2,3,4),2,2)
x
matrix(c(1,2,3,4),2,2,byrow = TRUE)
sqrt(x)
x^2

x = rnorm(50)
y = x + rnorm(50, mean=50, sd = .1)
cor(x,y)
set.seed(1303) # to reproduce a specific of pseudo-random numbers
rnorm(50)

## Graphics

x = rnorm(100)
y = rnorm(100)
plot(x,y)
plot(x,y,xlab="this is the x-axis", ylab="this is the y-axis",
     main="Plot of X vs Y")
pdf("Figure.pdf")
plot(x, y, col="green")
dev.off()

x = seq(-pi, pi, length=50)
y=x
f = outer(x, y, function (x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels = 45, add = T)
fa = (f-t(f))/2
contour(x,y,fa, nlevels = 15)

image(x,y,fa)
persp(x,y,fa, theta=30, phi=30)


# Bivariate Normal
#install.packages("mvtnorm")
library(mvtnorm)
library(MASS)
x = seq(-12, 12, length=50)
y = x
# the first letter refers to the kind of function
# rnomr -> random number taken from a normal distriution;
# dnorm -> density normal distribution

fbn = outer(x, y, function(x,y)dmvnorm(cbind(x,y), mean = rep(0,2), 
            sigma = matrix(c(1, .5, .5, 1),2)))

fbn1 = outer(x, y, function(x,y)dmvnorm(cbind(x,y), mean = rep(0,2), 
                                       sigma = matrix(c(1, .1, .1, 1),2)))

fbn2 = outer(x, y, function(x,y)dmvnorm(cbind(x,y), mean = c(0,0), 
                                       sigma = diag(2)))

dev.new()

par(mfrow = c(3,2) )
contour(x,y,fbn)
persp(x,y,fbn,theta = 30)

contour(x,y,fbn1)
persp(x,y,fbn1,theta = 30)

contour(x,y,fbn2)
persp(x,y,fbn2,theta = 30)


dev.off()

## Indexing Data
A = matrix(1:16,4,4)
View(A)
A[2,3]
A[c(1,3),c(2,4)]
A[1:3,2:4]
A[1:2,]
A[1,]
A[1,1:4,drop=FALSE]
A[-c(1,3),]
A
dim(A)

# Loading Data

Auto = read.table("Auto.data") # problem, names of values are inside data  
Auto1 = read.table("Auto.data", header = T, na.strings = "?") # solve the problem above
                                                             # in the dataset not available values are marked with the "?"
Auto = read.csv("Auto.cvs", header = T, na.strings = "?") 
library(ISLR)
head(Auto)
attach(Auto)
dim(Auto)
dim(na.omit(Auto))

unique(unlist (lapply (Auto, function (x) which (is.na (x)))))
Auto = na.omit(Auto)
dim(Auto)
names(Auto) # returns the names of the culoums

#### Additional Graphical and Numerical Summaries
plot(cylinders , mpg)
plot(Auto$cylinders , Auto$mpg , xlab = "cylinders", ylab = "mpg")
attach(Auto) # the database is searched by R when evaluating a variable
plot(cylinders , mpg)
cylinders = as.factor(cylinders) # convert a number value to a categorical value (qualitaty varaible)
plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T, horizontal=T)
plot(cylinders, mpg, col="red", varwid=T, xlab="cylinders", ylab="MPG")
hist(mpg)
hist(mpg, breaks=15)
plot(horsepower,mpg)
pairs(Auto)
pairs(~mpg+displacement+horsepower+weight+acceleration, data=Auto)
plot(horsepower, mpg)
identify(horsepower, mpg, name)

summary(Auto)

summary (mpg)
