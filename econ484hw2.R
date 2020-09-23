# 12(a)
Power <- function(){
  2^3
}

# 12(b)
Power2 <- function(x, a){
  x^a
}
Power2(3, 8) # [1] 6561

# 12(c)
Power2(10, 3) # [1] 1000
Power2(8, 17) # [1] 2.2518e+15
Power2(131, 3) # [1] 2248091

# 12(d)
Power3 <- function(x, a){
  result <- x^a
  return(result)
}

# 12(e)
x = seq(1:10)
y = Power3(x, 2)
plot(x, y, log = "xy", xlab = "x in log-scale", ylab = "y in log-scale",
     main = "y = x^2 in log-scale")

# 12(f)
PlotPower <- function(x, a){
  y = x^a
  plot(x, y, xlab = "x", ylab = "y")
}
PlotPower(1:10, 3)

# 13(a)
library(MASS)
library(class)
attach(Boston)
summary(Boston)

glm.crim1 <- rep(0, length(crim)) # defines an array with all 0, same length with crim
glm.crim1[crim > median(crim)] <- 1 # if larger than median, set equal to 1
table(glm.crim1)
Boston <- data.frame(Boston, glm.crim1)
# split data
train = 1:(length(glm.crim1)/2)
Boston.train = Boston[train, ]
test = 1:(length(glm.crim1)/2)
Boston.test = Boston[test, ]
crime.test = glm.crim1[test]
# logistic regression
fit1 <- glm(glm.crim1 ~ indus + nox + age + dis + rad + tax, 
            data=Boston.train, family = binomial)
summary(fit1)$coef
# prediction
fit1.pred <- predict(fit1, Boston.test, type = "response")
glm.pred <- rep(0, length(fit1.pred))
glm.pred[fit1.pred > 0.55] <- 1
mean(glm.pred != crime.test)

# LDA
fit2 <- lda(glm.crim1 ~ indus + nox + age + dis + rad + tax, 
            data = Boston.train, family = binomial)
fit2.pred <- predict(fit2, Boston.test, type = "response")

# KNN
train.X = cbind(indus, nox, age, dis, rad, tax)[train, ]
test.X = cbind(indus, nox, age, dis, rad, tax)[test, ]
knn.pred = knn(train.X, test.X, crime.test, k = 1)
table(knn.pred, crime.test)