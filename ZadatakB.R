# --------------------- a) ---------------------

# učitavanje podataka
B.data <- read.table("zad55r.dat", header = TRUE, sep = " ")
B.data <- data.frame(B.data)

# dijagram raspršenja
plot(B.data)

# --------------------- b) ---------------------

# kvadratični model
X <- B.data$x
Y <- B.data$y
X.squared <- X^2

model <- lm(Y ~ X + X.squared) # Y = beta0 + X * beta1 + X^2 * beta2 + epsilon
summary(model) # Bdjusted R-squared == R^2 ?

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
