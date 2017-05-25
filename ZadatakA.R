# --------------------- a) ---------------------

# učitavanje podataka
A.data <- read.table("zad51r.dat", header = TRUE, sep = " ")
A.data <- data.frame(A.data)

# dijagram raspršenja
plot(A.data)

# --------------------- b) ---------------------

# kvadratični model
X <- A.data$x
Y <- A.data$y
X.squared <- X^2

model <- lm(Y ~ X +  X.squared) # Y = beta0 + X * beta1 + X^2 * beta2 + epsilon, X.squared je isto kao I(X^2)

# crtanje modela
predict.original <- function(x){
  beta0 <- model$coefficients["(Intercept)"]
  beta1 <- model$coefficients["X"]
  beta2 <- model$coefficients["X.squared"]
  return(beta0 + beta1 * x + beta2 * x^2)
}

x.draw <- min(X):max(X)
y.draw <- predict.original(x.draw)
lines(x.draw, y.draw, col="red")

# R^2 i testiranje hipoteze beta2=0 (parametar uz X^2) uz dvostranu alternativu
summary(model)

# --------------------- c) ---------------------

# graf reziduala i standardiziranih reziduala
plot(X, model$residuals)
residuals.standardized <- scale(model$residuals)
plot(X, residuals.standardized)

# provjera normalnosti
# grafički: QQ plot
qqnorm(residuals.standardized)
qqline(residuals.standardized)

# Kolmogorov-Smirnov test
ks.test(model$residuals, 'pnorm')

# --------------------- d) --------------------- 

# transformacija podataka
Y0 <- log(Y)
plot(X, Y0)
model.ln <- lm(Y0 ~ X) # Y0 = beta0 + X * beta1 + epsilon
summary(model.ln)

# crtanje modela
predict.ln <- function(x){
  beta0 <- model.ln$coefficients["(Intercept)"]
  beta1 <- model.ln$coefficients["X"]
  return(beta0 + beta1 * x)
}

y.ln.draw <- predict.ln(x.draw)
lines(x.draw, y.ln.draw, col="red")

# graf reziduala i standardiziranih reziduala
plot(X, model.ln$residuals)
residuals.ln.standardized <- scale(model.ln$residuals)
plot(X, residuals.ln.standardized)

# provjera normalnosti
# grafički: QQ plot
qqnorm(residuals.ln.standardized)
qqline(residuals.ln.standardized)

# Kolmogorov-Smirnov test
ks.test(model.ln$residuals, 'pnorm')

# --------------------- e) ---------------------

# U .rmd izvještaju treba napisati formulu modela za originalne (netransformirane) podatke
# Regresijska funkcija i originalni podaci već su nacrtani pod a)

Y.predicted <- predict.original(X)
plot(Y, Y.predicted)
abline(a=0, b=1) # y = a + b*x

# --------------------- f) ---------------------

X.new <- seq(min(X), max(X))
X.squared.new <- X.new^2

# originalni model
prediction <- predict.lm(model, newdata = data.frame(X=X.new, X.squared=X.squared.new), interval = 'prediction', level=0.95) # Y
confidence <- predict.lm(model, newdata = data.frame(X=X.new, X.squared=X.squared.new), interval = 'confidence', level=0.95) # Y srednje
plot(X, Y)
lines(x.draw, y.draw, col="red")
lines(X.new, prediction[,2])
lines(X.new, prediction[,3])
lines(X.new, confidence[,2], col="green")
lines(X.new, confidence[,3], col="green")

# model sa transformiranim podacima
prediction.ln <- predict.lm(model.ln, newdata = data.frame(X=X.new), interval = 'prediction', level=0.95) # Y0
confidence.ln <- predict.lm(model.ln, newdata = data.frame(X=X.new), interval = 'confidence', level=0.95) # Y0 srednje
plot(X, Y0)
lines(x.draw, y.ln.draw, col="red")
lines(X.new, prediction.ln[,2])
lines(X.new, prediction.ln[,3])
lines(X.new, confidence.ln[,2], col="green")
lines(X.new, confidence.ln[,3], col="green")

# Bolji je model sa transformiranim podacima
