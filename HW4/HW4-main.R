rm(list = ls())
require(alr4)
require(ggplot2)
require(gridExtra)

# 7.1 ---------------------------------------------------------------------

data <- fuel2001
data$Dlic <- data$Drivers / data$Pop
data$Fuel <- 1000 * data$FuelC / data$Pop
n <- nrow(data)
m2 <- lm(Fuel ~ Tax + Dlic + Income + log(Miles), data = data, 
         weights = rep(x = 1, times = n))
m1 <- lm(Fuel ~ Tax + Dlic + Income + log(Miles), data = data, 
         weights = rep(x = 2, times = n))

summary(m1)
summary(m2)


# 7.7 ---------------------------------------------------------------------
require(alr4)
require(ggplot2)

data <- galtonpeas
# plot(x = data$Parent, y = data$Progeny)
p <- ggplot(data = data)
p <- p + geom_point(aes(x = Parent, y = Progeny))
p <- p + theme_bw() + theme(text = element_text(size = 12))
p <- p + ggtitle("Progeny VS Parent")
p

model <- lm(Progeny ~ Parent, data = data, weights = SD)
coef <- model$coefficients

p <- p + geom_abline(intercept = coef[1], slope = coef[2])
p


# 7.8 ---------------------------------------------------------------------
require(gridExtra)
data <- jevons

p1 <- ggplot(data = data, aes(x = Age, y = Weight))
p1 <- p1 + geom_point()
p1 <- p1 + geom_smooth(formula = y ~ x, method = "lm", se = FALSE)
p1 <- p1 + theme_bw() + theme(text = element_text(size = 14))
p1 <- p1 + ggtitle("Weight VS Age")

p2 <- ggplot(data = data, aes(x = Age, y = SD))
p2 <- p2 + geom_point()
p2 <- p2 + geom_smooth(formula = y ~ x, method = "lm", se = FALSE)
p2 <- p2 + theme_bw() + theme(text = element_text(size = 14))
p2 <- p2 + ggtitle("SD VS Age")

grid.arrange(p1, p2, ncol=2)

w <- data$SD^2 / data$n
m <- lm(Weight ~ Age, data = data, weight = w)
summary(m)

predict(object = m, newdata = data.frame(Age = 0), interval="predict", level = 0.95) 




m_var <- vcov(m)
m_var <- m_var[1,1] + (1:5)^2 * m_var[2,2] + 2 * (1:5) * m_var[1,2]
m_sd <- sqrt(m_var + data$SD^2)
y_hat <- predict(object = m, newdata = data.frame(Age = 1:5))
pnorm((7.9379 - y_hat) / m_sd)



# 7.10 --------------------------------------------------------------------

data <- fuel2001
data$Dlic <- data$Drivers / data$Pop
data$Fuel <- 1000 * data$FuelC / data$Pop
m <- lm(Fuel ~ Tax + Dlic + Income + log(Miles), data = data)
data_ori <- data

library(boot) 
confint_bootstrap <- function(data, indices) {
    d <- data[indices, ] # allows boot to select sample
    data_tmp <- data_ori
    data_tmp$Fuel <- data_tmp$Fuel + d
    fit <- lm(Fuel ~ Tax + Dlic + Income + log(Miles), data_tmp)
    return(coef(fit))
} 
results <- boot(data = as.matrix(m$residuals), 
                statistic = confint_bootstrap, R = 3000)
result <- results$t
colnames(result) <- names(coef(m))
apply(X = result, MARGIN = 2, FUN = quantile, probs = c(0.025, 0.975))
t(confint(m))

colnames(result)[1] <- "Intercept"
colnames(result)

colnames(result)[1] <- "Intercept"

colnames(result)[5] <- "log_Miles"

p1 <- ggplot(data = as.data.frame(result), aes(x = Intercept)) +
    geom_histogram(aes(y = ..density..)) + 
    geom_density(size = 1) + theme_bw() + theme(text = element_text(size = 14)) + 
    ggtitle("Intercept")

p2 <- ggplot(data = as.data.frame(result), aes(x = Tax)) +
    geom_histogram(aes(y = ..density..)) + 
    geom_density(size = 1) + theme_bw() + theme(text = element_text(size = 14)) +
    ggtitle("Tax")

p3 <- ggplot(data = as.data.frame(result), aes(x = Dlic)) +
    geom_histogram(aes(y = ..density..)) + 
    geom_density(size = 1) + theme_bw() + theme(text = element_text(size = 14)) +
    ggtitle("Dlic")

p4 <- ggplot(data = as.data.frame(result), aes(x = Income)) +
    geom_histogram(aes(y = ..density..)) + 
    geom_density(size = 1) + theme_bw() + theme(text = element_text(size = 14)) +
    ggtitle("Income")

p5 <- ggplot(data = as.data.frame(result), aes(x = log_Miles)) +
    geom_histogram(aes(y = ..density..)) + 
    geom_density(size = 1) + theme_bw() + theme(text = element_text(size = 14)) +
    ggtitle("log_Miles")


# 10.2 --------------------------------------------------------------------

data <- Highway
data$sigs1 <- (data$sigs * data$len + 1) / data$len
m <- lm(log(rate) ~ log(len) + log(adt) + log(trks) + log(shld) + log(sigs1) + 
            lane + slim + shld + lwid + acpt + itg + htype, data = data)
m_min <- lm(log(rate) ~ shld + log(len), data = data)
m_forward <- step(m_min, scope = formula(m), direction = "forward", trace = FALSE)
m_backward <- step(m, scope = list(lower = formula(m_min)), direction = "backward", trace = FALSE)

ceof(m_forward)

coef(m_backward)

###
data$log_rate_len <- log(data$rate * data$len)
m_max <- lm(log_rate_len ~ log(len) + log(adt) + log(trks) + log(shld) + log(sigs1) + 
                lane + slim + shld + lwid + acpt + itg + htype, data = data)
m_min <- lm(log_rate_len ~ lwid, data = data)

m_forward <- step(m_min, scope = formula(m_max), direction = "forward", 
                  trace = FALSE)
m_backward <- step(m_max, scope = list(lower = formula(m_min)), 
                   direction = "backward", trace = FALSE)

summary(m_forward)
summary(m_backward)

###
m_max <- lm(log(rate) ~ log(len) + log(adt) + log(trks) + log(shld) + log(sigs1) + 
                lane + slim + shld + lwid + acpt + itg + htype, data = data, offset = -log(len))
m_min <- lm(log(rate) ~ lwid, data = data, offset = log(len))
m_forward <- step(m_min, scope = formula(m_max), direction = "forward", 
                  trace = FALSE)
m_backward <- step(m_max, scope = list(lower = formula(m_min)), 
                   direction = "backward", trace = FALSE)
