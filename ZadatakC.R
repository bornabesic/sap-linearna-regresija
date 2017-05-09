# --------------------- a) ---------------------

# učitavanje podataka
C.data <- read.table("zad57r.dat", header = TRUE, sep = " ")
C.data <- data.frame(C.data)

X <- C.data$x
Y <- C.data$y
Z <- C.data$z

# dijagram raspršenja
plot(X, Z)
model1 <- lm(Z ~ X) # Z = alpha0 + alpha1 * X + epsilon
summary(model1) # Cdjusted R-squared == R^2 ?
lines(X, model1$fitted.values, col="orange") # crtanje modela

plot(Y, Z)
model2 <- lm(Z ~ Y) # Z = alpha0 + alpha1 * X + epsilon
summary(model2) # Cdjusted R-squared == R^2 ?
lines(Y, model2$fitted.values, col="purple") # crtanje modela

plot(C.data$x, C.data$y)
library(rgl)
plot3d(C.data)

# --------------------- b) ---------------------

model1 <- lm(Z ~ X) # Z = alpha0 + alpha1 * X + epsilon
model2 <- lm(Z ~ Y) # Z = beta0 + beta1 * Y + epsilon

summary(model1) # Cdjusted R-squared == R^2 ?
summary(model2)

lines(X, model$fitted.values, col="red") # crtanje modela

# TODO testiranje hipoteze beta2=0 uz dvostranu alternativu

# --------------------- c) ---------------------

# graf reziduala i standardiziranih reziduala
plot(X, model$residuals)
residuals.standardized <- scale(model$residuals)
plot(X, residuals.standardized)

# provjera normalnosti
# grafički: QQ plot
qqnorm(residuals.standardized)
qqline(residuals.standardized)

# TODO Kolmogorov-Smirnov test

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
