###oringinal data
y = c(-0.97, 2.51, -0.19, 6.53, 1.00)
x1 = c(-0.63, 0.18, -0.84, 1.60, 0.33)
x2 = c(-0.82, 0.49, 0.74, 0.58, -0.31)
###first order model
#model
hw3 = data.frame(Y = y, X1 = x1, X2 = x2)
x = data.frame(x0 = rep(1, length(y)), x1, x2)
x = as.matrix(x)
#preparation
txx = round(t(x) %*% x, digits = 2)
txy = round(t(x) %*% y, digits = 2)
txxr = round(solve(t(x) %*% x), digits = 2)
#calculation
beta = txxr %*% txy
Hat = function(xx, yy){
  xx = as.matrix(xx)
  txx = round(t(xx) %*% xx, digits = 2)
  txy = round(t(xx) %*% yy, digits = 2)
  txxr = round(solve(t(xx) %*% xx), digits = 2)
  hat = round(xx %*% txxr %*% t(xx), digits = 2)
  return(hat)
}
hat1 = Hat(x, y)
#rank
install.packages("Matrix")
library(Matrix)
rankMatrix(hat)
rankMatrix(diag(1,5) - hat)
rankMatrix(x)
#lm
fit = lm(y ~ x1 + x2, data = hw3)
fit$coefficients
yhat = round(fit$fitted.values, digits = 2)
summary(fit)
#anova
round(fit$residuals, digits = 2)
round(fit$df.residual, digits = 2)
sse = sum((y - yhat)^2)
mse = sse/2
mse
###interaction model
xint = as.matrix(cbind(x, "x1*x2" = round(x$x1*x$x2, digits = 2)))
hw3int = as.data.frame(cbind(y, xint))
hat2 = Hat(xint, y)
rankMatrix(hat2)
rankMatrix(diag(1,5) - hat2)
#lm
fitint = lm(y ~ x1 + x2 + x1*x2, data = hw3int)
summary(fitint)
round(fitint$fitted.values, digits = 2)
round(fitint$residuals, digits = 2)
#anova
yhatint = fitint$fitted.values
sseint = sum((y - yhatint)^2)
mseint = 0.1106^2
