rm(list = ls())
require(alr4)
require(ggplot2)

# 12.5 --------------------------------------------------------------------

data <- na.omit(Donner)
summary(data$sex)
tb_yVsSex <- table(data$sex, data$y)
prop.table(tb_yVsSex, 1)
chisq.test(tb_yVsSex, correct = FALSE)


m1 <- glm(y ~ age, data = data, family = binomial())
summary(m1)

plot(data$age, m1$residuals, col = data$y)
abline(a = 0, b = 0, lty = 2)

foo <- data.frame(age = data$age, residuals = m1$residuals, survival = data$y)
p <- ggplot(data = foo)
p <- p + geom_point(aes(x = age, y = residuals, shape = survival), size = 2)
p <- p + theme_bw() + theme(text = element_text(size = 14))
p <- p + ggtitle("Residuals VS Age")


residualPlot(m1, grid = TRUE)


m2 <- update(m1, ~ age + I(age^2) + sex + status)

glm(y ~ poly(age, 2) + sex + status, binomial,
    data=na.omit(Donner))


residualPlot(m2, grid = TRUE)

m3 <- update(m2, ~ sex + status)



# 12.6 --------------------------------------------------------------------

data <- Challeng
m1 <- glm(cbind(fail, n - fail) ~ temp, family = binomial, data = data)
m2 <- glm(cbind(fail, n - fail) ~ temp + pres, family = binomial, data = data)
anova(m1, m2, test="Chisq")

foo <- data.frame(temp = 31)
tmp <- predict(m1, newdata = foo)
exp(tmp) / (1 + exp(tmp))

ilogit(tmp)

data <- Challeng
cbind(data[sort(Challeng[,1], index.return = TRUE)$ix, c(1,3)], orings)

# 12.7 --------------------------------------------------------------------


data <- Whitestar
m1 <- glm(cbind(surv, m - surv) ~ class + age + sex, 
          data = data, family = binomial)
summary(m1)

m2 <- update(m1, ~ (class + age + sex) ^ 2)
Anova(m2)
