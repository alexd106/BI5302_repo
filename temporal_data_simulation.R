set.seed(16)
n = 48
a1 <- 100  #intercept
a2 <- 170
b1 <- 3.1  #slope
b2 <- 4.2
x1 <- round(runif(n, 1, n), 1)  #values of the year covariate
x2 <- round(runif(n, 1, n), 1)  #values of the year covariate
n.location <- 2
location <- rep(c("Maui", "Oahu"), each = 48) 
year1 <- 1956:2003
year2 <- 1956:2003
sigma1 <- 30
sigma2 <- 70
rho1 <- 0.8
rho2 <- 0.75

# site1

## define a constructor for a first-order
## correlation structure
ar1.1 <- corAR1(form = ~ year1, value = rho1)
## initialize this constructor against our data
AR1.1 <- Initialize(ar1.1, data = data.frame(year1))
## generate a correlation matrix
V.1 <- corMatrix(AR1.1)
## Cholesky factorization of V
Cv.1 <- chol(V.1)
## simulate AR1 errors
e.1 <- t(Cv.1) %*% rnorm(n, 0, sigma1)  # cov(e) = V * sig^2
## generate response
y.1 <- a1 + b1 * x1 + e.1
data.temporalCor.1 <- data.frame(y = y.1, x = x1, year = year1, location = location[1:48])


# site2

## define a constructor for a first-order
## correlation structure
ar1.2 <- corAR1(form = ~ year2, value = rho2)
## initialize this constructor against our data
AR1.2 <- Initialize(ar1.2, data = data.frame(year2))
## generate a correlation matrix
V.2 <- corMatrix(AR1.2)
## Cholesky factorization of V
Cv.2 <- chol(V.2)
## simulate AR1 errors
e.2 <- t(Cv.2) %*% rnorm(n, 0, sigma2)  # cov(e) = V * sig^2
## generate response
y.2 <- a2 + b2 * x2 + e.2
data.temporalCor.2 <- data.frame(y = y.2, x = x2, year = year2, location = location[49:96])



dataf <- rbind(data.temporalCor.1, data.temporalCor.2)
names(dataf)[1:2] <- c("abund", "rainfall") 
dataf$abund <- round(dataf$abund, digits = 0)

pairs(dataf)

xyplot(abund ~ year | location, data = dataf)
xyplot(abund ~ rainfall | location, data = dataf)



bird_lm <- lm(abund ~ rainfall + location + year +rainfall:location, data = dataf)

plot(resid(bird_lm) ~
       fitted(bird_lm))

plot(resid(bird_lm) ~
       dataf$rainfall)

plot(resid(bird_lm) ~
       dataf$year)

plot(resid(bird_lm) ~
       dataf$location)


xyplot(resid(bird_lm) ~ fitted(bird_lm) | location, data = dataf)

xyplot(resid(bird_lm) ~ year | location, data = dataf)

write.table(dataf, 'data/hawaii3.txt', col.names = TRUE, row.names = FALSE,
            quote = FALSE, sep = "\t")
