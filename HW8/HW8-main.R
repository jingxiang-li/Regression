rm(list = ls())
require(faraway)
require(MASS)


# problem 1 ---------------------------------------------------------------

data <- discoveries
E_x <- sum(data) / length(data)
Chi0 <- sum((abs(data - E_x) - 0.5)^2 / E_x)
df0 <- length(data) - 1

pchisq(q = Chi0, df = df0, lower.tail = FALSE)



k = 1
x = vector(mode = "numeric", length = length(data))
y = x
for (i in 1:length(data))
{
    x[k] <- x[k] + data[i]
    y[k] <- y[k] + 1
    
    if (x[k] > 15)
    {
        k <- k + 1
    }
}

n <- sum(x > 0)
x <- x[1 : n]
y <- y[1 : n]

EE_x <- y * E_x
# write.table(x =  cbind(x, EE_x), file = "table_comb")

chisq.test(x = x, p = EE_x, rescale.p = TRUE)


# problem 2 ---------------------------------------------------------------

data <- salmonella
m1 <- glm(colonies ~ dose, family = poisson, data = data)
summary(m1)
par(mfrow = c(1, 2))
halfnorm(residuals(m1), main = "Half-normal Plot")
plot(log(fitted(m1)), log((data$colonies - fitted(m1))^2),
     xlab = expression(hat(mu)), ylab = expression((y - hat(mu))^2), 
     main = "Variance VS. Mean")
abline(0,1)


data1 <- data[-12, ]
m1 <- update(m1, data = data1)
summary(m1)

dp <- sum(residuals(m1, type = "pearson") ^ 2) / m1$df.res
summary(m1, dispersion = dp)
drop1(m1, test = "F")

plot(log(fitted(m1)),log((data1$colonies - fitted(m1))^2),
     xlab=expression(hat(mu)),ylab=expression((y - hat(mu))^2))
abline(0,1)


# problem 7 ---------------------------------------------------------------

data <- esdcomp
m1 <- glm(complaints ~ ., family = poisson, data = data)
#summary(m1)
#drop1(m1,test = "F")

m0 <- update(m1, formula = complaints ~ 1)
m_AIC <- step(m1, scope = list(upper = m1, lower = m0), trace = FALSE)



halfnorm(residuals(m1), main = "Half-normal Plot")
plot(log(fitted(m1)), log((data$complaints - fitted(m1))^2),
     xlab = expression(hat(mu)), ylab = expression((y - hat(mu))^2), 
     main = "Variance VS. Mean")
abline(0,1)


m_nb1 <- glm.nb(complaints ~ ., data = data)
summary(m1)
drop1(m1)

m_nb0 = glm.nb(complaints ~ 1, data = data)

m_AIC <- step(m1, scope = list(upper = m1, lower = m0), 
              trace = FALSE, direction = "both")
summary(m_AIC)

####################33

m_nb1 <- glm.nb(complaints ~ (visits + residency + gender + revenue + hours)^2, data = data)
summary(m_nb1)
m_AIC <- step(m_nb1, scope = list(upper = m_nb1, lower = m_nb0), 
              trace = FALSE, direction = "both")
summary(m_AIC)
drop1(m1)



