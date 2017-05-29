# --------------------- a) ---------------------

# učitavanje podataka
B.data <- read.table("zad55r.dat", header = TRUE, sep = " ")
B.data <- data.frame(B.data)

# dijagram raspršenja
plot(B.data, xlab = "broj dana proteklih od sjetve žita", ylab = "Urod, (kg/ha)", main="Utjecaj datuma žetve na urod")

# --------------------- b) ---------------------

urod <- B.data$y
N <- B.data$x
N.squared <- I(N^2)

model <- lm(urod ~ N +  N.squared)

f = function(x, koeficijenti)
  return(koeficijenti[[1]] + koeficijenti[[2]] * x + koeficijenti[[3]] * x^2)

plot(N, urod, xlab = "broj dana proteklih od sjetve žita", ylab = "Urod, (kg/ha)",
     main="Utjecaj datuma žetve na urod")
curve(f(x, model$coefficients), add = TRUE, col = "red")

summary(model)

# TODO testiranje hipoteze beta2=0 uz dvostranu alternativu

# --------------------- c) ---------------------

par(mfrow=c(1,2))
plot(N, model$residuals, xlab='Redni broj reziduala', ylab = 'Iznos reziduala',
     main = 'Graf reziduala')

plot(N, rstandard(model), xlab='Redni broj reziduala', ylab = 'Iznos reziduala',
     main = 'Graf standardiziranih reziduala')


qqnorm(rstandard(model), xlab = 'Teoretski kvantili', ylab = 'Kvantili iz uzorka',
       main = 'QQ plot')
qqline(rstandard(model))

ks.test(rstandard(model), 'pnorm')

plot(N,urod)
prediction = predict.lm(model,B.data,interval = "prediction")
confidence = predict.lm(model,B.data,interval = "confidence")

curve(f(x, model$coefficients), add = TRUE, col = "red")
lines(N, prediction[,2])
lines(N, prediction[,3])
lines(N, confidence[,2], col="green")
lines(N, confidence[,3], col="green")


# --------------------- d) --------------------- 

# transformacija podataka
urod0 <- log(urod)
plot(N, urod0)
model.ln <- lm(urod0 ~ N) # urod0 = beta0 + N * beta1 + epsilon
lines(N, model.ln$fitted.values, col="red") # crtanje modela

# graf reziduala i standardiziranih reziduala
plot(N, model.ln$residuals)
residuals.ln.standardized <- scale(model.ln$residuals)
plot(N, residuals.ln.standardized)

# provjera normalnosti
# grafički: QQ plot
qqnorm(residuals.ln.standardized)
qqline(residuals.ln.standardized)

# TODO Kolmogorov-Smirnov test

# --------------------- e) ---------------------
# TODO

# --------------------- f) ---------------------
# TODO
