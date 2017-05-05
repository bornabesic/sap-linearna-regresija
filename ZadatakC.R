# --------------------- a) ---------------------

# učitavanje podataka
C.data <- read.table("zad57r.dat", header = TRUE, sep = " ")
C.data <- data.frame(C.data)

# dijagram raspršenja
plot(C.data$x, C.data$z)
plot(C.data$y, C.data$z)
plot(C.data$x, C.data$y)
update.packages("rgl")
plot3d(C.data)

# --------------------- b) ---------------------

# kvadratični model
X <- C.data$x
Y <- C.data$y
Z <- C.data$z
X.squared <- X^2

model <- lm(Y ~ X + X.squared) # Y = beta0 + X * beta1 + X^2 * beta2 + epsilon
summary(model) # Cdjusted R-squared == R^2 ?

lines(X, model$fitted.values, col="red") # crtanje modela

# TODO testiranje hipoteze beta2=0 uz dvostranu alternativu

# --------------------- c) ---------------------

# graf reziduala i standardiziranih reziduala
plot(model$fitted.values, model$residuals)
residuals.standardized <- scale(model$residuals)
plot(model$fitted.values, residuals.standardized)

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
plot(model.ln$fitted.values, model.ln$residuals)
residuals.ln.standardized <- scale(model.ln$residuals)
plot(model.ln$fitted.values, residuals.ln.standardized)

# provjera normalnosti
# grafički: QQ plot
qqnorm(residuals.ln.standardized)
qqline(residuals.ln.standardized)

# TODO Kolmogorov-Smirnov test

# --------------------- e) ---------------------
# TODO

# --------------------- f) ---------------------
# TODO
