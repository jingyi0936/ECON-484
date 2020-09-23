library(ISLR)
library(boot)
library(MASS)
library(splines)

attach(Wage)

set.seed(1)
degree <- 10
cv.errs <- rep(NA, degree)

for (i in 1:degree) {
  fit <- glm(wage ~ poly(age, i), data = Wage)
  cv.errs[i] <- cv.glm(Wage, fit)$delta[1]
}

plot(1:degree, cv.errs, xlab = 'Degree', ylab = 'Test MSE', type = 'l')

fit1 <- glm(wage ~ poly(age, 1), data = Wage)
fit2 <- glm(wage ~ poly(age, 2), data = Wage)
fit3 <- glm(wage ~ poly(age, 3), data = Wage)
fit4 <- glm(wage ~ poly(age, 4), data = Wage)
fit5 <- glm(wage ~ poly(age, 5), data = Wage)
anova(fit1, fit2, fit3, fit4, fit5)

agelims = range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds = predict(fit4, newdata = list(age = age.grid), se = T)
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
plot(age, wage, xlim = agelims, cex = 0.5, col = "black")
title("Degree - 4 Polynomial")
lines(age.grid, preds$fit, lwd = 1.5, col = "blue")


step_err <- rep(NA, 10)
for(i in 2:10){
  Wage$cut <- cut(age, i)
  cut_fit <- glm(wage~cut, data = Wage)
  step_err[i] <- cv.glm(Wage, cut_fit)$delta[1]
}
plot(2:degree, step_err, xlab = 'Degree', ylab = 'Test MSE', type = 'l')

fit8 <- glm(wage ~ cut(age, 8), data = Wage)
preds <- predict(fit8, list(age = age.grid))
plot(age, wage, xlim = agelims, cex = 0.5, col = "black")
title("8-cuts step function")
lines(age.grid, preds, col = "red", lwd = 1.5)

detach(Wage)

attach(Boston)
fit <- lm(nox~poly(dis, 3))
summary(fit)

dislims = range(dis)
dis.grid <- seq(from = dislims[1], to = dislims[2])
preds = predict(fit, newdata = list(dis = dis.grid), se = T)
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
plot(dis, nox, xlim = dislims, cex = 0.5, col = "black")
lines(dis.grid, preds$fit, lwd = 1.5, col = "blue")

rss <- rep(NA, 10)
for (i in 1:10) {
  fit <- lm(nox ~ poly(dis, i))
  rss[i] <- sum(fit$residuals ^ 2)
}
plot(1:10, rss, type = 'l', xlab = "Degree", ylab = "RSS")
title("Rss vs polynomial degrees")

err <- rep(NA, 10)
for (i in 1:10) {
  fit <- glm(nox ~ poly(dis, i), data = Boston)
  err[i] <- cv.glm(Boston, fit, K = 10)$delta[1]
}
plot(1:10, err, type = 'l', xlab = "Degree", ylab = "Test MSE")

df = 4
fit = lm(nox ~ bs(dis, df))
dim(bs(dis, df))
attr(bs(dis, df), "knots")

preds <- predict(fit, list(dis = dis.grid), se = TRUE)
se.bands <- cbind(preds$fit + 2* preds$se.fit, preds$fit - 2 * preds$se.fit)
plot(nox ~ dis, xlim = dislims, cex = 0.5, col = "black")
lines(dis.grid, preds$fit, lwd = 2, col = "red")

#(e)
res <- c()
df.range <- 3:16
for (val in df.range) {
  fit <- lm(nox ~ bs(dis, df = val))
  res <- c(res, sum(fit$residuals ^ 2))
}
plot(df.range, res, type = 'l', xlab = 'degree of freedom', ylab = 'RSS')
title("Degress of freedom vs RSS")

#(f)
res <- c()
for (val in df.range) {
  fit <- glm(nox ~ bs(dis, df = val), data = Boston)
  testMSE <- cv.glm(Boston, fit, K = 10)$delta[1]
  res <- c(res, testMSE)
}
plot(df.range, res, type = 'l', xlab = 'degree of freedom', ylab = 'Test MSE')
