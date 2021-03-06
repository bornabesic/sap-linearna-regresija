# --------------------- a) ---------------------

# učitavanje podataka
B.data <- read.table("zad55r.dat", header = TRUE, sep = " ")
B.data <- data.frame(B.data)

# dijagram raspršenja
plot(B.data, xlab = "broj dana proteklih od sjetve žita", ylab = "Urod, (kg/ha)", main="Utjecaj datuma žetve na urod")

# --------------------- b) ---------------------

X <- B.data$x
Y <- B.data$y
X.squared <- X^2

model <- lm(Y ~ X +  X.squared)
  
  predict.original <- function(x){
    theta_0 <- model$coefficients["(Intercept)"]
    theta_1 <- model$coefficients["X"]
    theta_2 <- model$coefficients["X.squared"]
    return(theta_0 + theta_1 * x + theta_2 * x^2)
  }

plot(X, Y, xlab = "broj dana proteklih od sjetve žita", ylab = "Urod, (kg/ha)",
     main="Utjecaj datuma žetve na urod")
x.draw <- min(X):max(X)
y.draw <- predict.original(x.draw)
lines(x.draw, y.draw, col="red")

summary(model)

# TODO testiranje hipoteze beta2=0 uz dvostranu alternativu

# --------------------- c) ---------------------

par(mfrow=c(1,2))
plot(X, model$residuals, xlab='Redni broj reziduala', ylab = 'Iznos reziduala',
     main = 'Graf reziduala')

plot(X, rstandard(model), xlab='Redni broj reziduala', ylab = 'Iznos reziduala',
     main = 'Graf standardiziranih reziduala')


qqnorm(rstandard(model), xlab = 'Teoretski kvantili', ylab = 'Kvantili iz uzorka',
       main = 'QQ plot')
qqline(rstandard(model))

ks.test(rstandard(model), 'pnorm')


# --------------------- d) --------------------- 

# transformacija podataka
Y0 <- log(Y)
plot(X, Y0)
model.ln <- lm(Y0 ~ X) # Y0 = beta0 + X * beta1 + epsilon
lines(X, model.ln$fitted.values, col="red") # crtanje modela

# graf reziduala i standardiziranih reziduala
plot(X, model.ln$residuals)
residuals.ln.standardized <- scale(model.ln$residuals)
plot(X, residuals.ln.standardized)

# provjera normalnosti
# grafički: QQ plot
qqnorm(residuals.ln.standardized)
qqline(residuals.ln.standardized)

# TODO Kolmogorov-Smirnov test

# --------------------- e) ---------------------
# TODO

# --------------------- f) ---------------------
# TODO
