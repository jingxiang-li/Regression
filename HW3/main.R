rm(list = ls())
require(alr4)


# 6.7 ---------------------------------------------------------------------

data <- fuel2001
data$Dlic <- data$Drivers / data$Pop
m1 <- lm(FuelC ~ Tax + Dlic + Income + log(Miles), data = data)
m2 <- lm(FuelC ~ log(Miles) + Income + Dlic + Tax, data = data)
anova(m1)
anova(m2)

###
Anova(mod = m1, type = 2)
Anova(mod = m2, type = 2)


# 6.9 ---------------------------------------------------------------------

data <- cakes
m_full <- lm(Y ~ (X1 + X2) ^ 2 + I(X1 ^ 2) + I(X2 ^ 2), data = data)
summary(m_full)

m1 <- lm(Y ~ X1 + X2 + I(X1 ^ 2) + I(X2 ^ 2), data = data)
anova(m1, m_full)

m2 <- lm(Y ~ (X1 + X2) ^ 2 + I(X2 ^ 2), data = data)
anova(m2, m_full)

m_3 <- lm(Y ~ X2 + I(X2 ^ 2), data = data)
anova(m_full, m_test)


# 6.14 --------------------------------------------------------------------

data <- MinnLand
m_A <- lm(log(acrePrice) ~ year, data = data)
coef(m_A)

data$fyear <- factor(data$year)
m_B <- lm(log(acrePrice) ~ fyear, data = data)

mean_value <- tapply(X = log(data$acrePrice), INDEX = data$year, FUN = mean)

plot(x = data$year, y = log(data$acrePrice), col = "#424242",
     xlab = "year", ylab = "log(acrePrice)", main = "Model A VS. Model B")
lines(x = 2002:2011, y = mean_value, type = 'b', pch = 18, cex = 2.5, lwd = 2)
abline(coef(m_A), lwd = 2, lty = 2)
legend("topright", c("Model A", "Model B"), lty = c(2,1), lwd = c(2,2))

p1 <- ggplot(data.frame(x = e_pU2lp, y = e_f2lp), aes(x = x, y = y))
p1 <- p1 + geom_point()
p1 <- p1 + geom_smooth(formula = y ~ x, method = "lm", se = FALSE)
p1 <- p1 + theme_bw() + theme(text = element_text(size = 18))
p1 <- p1 + ggtitle("pctUrban after log(ppgdp)")

# 8.2 ---------------------------------------------------------------------

require(package = "gridExtra")

data <- stopping
# 
# p1 <- ggplot(data, aes(x = Speed, y = Distance))
# p1 <- p1 + geom_point()
# p1 <- p1 + geom_smooth(formula = y ~ x, method = "lm")
# p1 <- p1 + theme_bw() + theme(text = element_text(size = 18))
# p1 <- p1 + ggtitle("Before Transformation")
# p1
# 
# p2 <- ggplot(data, aes(x = Speed, y = sqrt(Distance)))
# p2 <- p2 + geom_point()
# p2 <- p2 + geom_smooth(formula = y ~ x, method = "lm")
# p2 <- p2 + theme_bw() + theme(text = element_text(size = 18))
# p2 <- p2 + ggtitle("After Transformation")
# p2
# 
# grid.arrange(p1, p2, ncol=2)
# 
# p1 <- ggplot(data, aes(x = Speed, y = Distance))
# p1 <- p1 + geom_point()
# p1 <- p1 + geom_smooth(formula = y ~ x, method = "lm")
# p1 <- p1 + theme_bw() + theme(text = element_text(size = 14))
# p1 <- p1 + ggtitle("(a) Before Transformation")
# 
# p2 <- ggplot(data, aes(x = Speed - 1, y = Distance))
# p2 <- p2 + geom_point()
# p2 <- p2 + geom_smooth(formula = y ~ x, method = "lm")
# p2 <- p2 + theme_bw() + theme(text = element_text(size = 14))
# p2 <- p2 + ggtitle("(b) lambda = 1")
# 
# p3 <- ggplot(data, aes(x = log(Speed), y = Distance))
# p3 <- p3 + geom_point()
# p3 <- p3 + geom_smooth(formula = y ~ x, method = "lm")
# p3 <- p3 + theme_bw() + theme(text = element_text(size = 14))
# p3 <- p3 + ggtitle("(b) lambda = 0")
# 
# p4 <- ggplot(data, aes(x = -(Speed ^ (-1) - 1), y = Distance))
# p4 <- p4 + geom_point()
# p4 <- p4 + geom_smooth(formula = y ~ x, method = "lm")
# p4 <- p4 + theme_bw() + theme(text = element_text(size = 14))
# p4 <- p4 + ggtitle("(b) lambda = -1")
# 
# grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

require(reshape2)

data <- stopping
m_h <- lm(Distance ~ I(Speed^2), data = data, weights = Speed^2)
m_1 <- lm(sqrt(Distance) ~ Speed, data = data)

x = 0:45
y_h = x^2 * coef(m_h)[2] + coef(m_h)[1]
y_1 = (x * coef(m_1)[2] + coef(m_1)[1])^2

data_plot <- data.frame(x, y_Hald = y_h, y_P1 = y_1)
data_plot <- melt(data_plot, id.vars = 1)
colnames(data_plot)[2] <- "group"

p <- ggplot()
p <- p + geom_point(aes(x = data$Speed, y = data$Distance))
p <- p + geom_line(aes(x = data_plot$x, y = data_plot$value, 
                       linetype = data_plot$group, 
                       colour = data_plot$group))
p <- p + theme_bw() + theme(text = element_text(size = 14))
p <- p + ggtitle("Distance VS. Speed") + xlab("Speed") + ylab("Distance")
p <- p + scale_linetype_discrete(name  ="Model",
                              breaks=c("y_Hald", "y_P1"),
                              labels=c("Hald Model", "Problem 8.2.1"))
p



# 9.11 --------------------------------------------------------------------
require(alr4)
data <- fuel2001
data$Dlic <- data$Drivers / data$Pop
data$Fuel <- 1000 * data$FuelC / data$Pop

test_outlier <- function(data, index)
{
    m_tmp <- lm(Fuel ~ Tax + Dlic + Income + log(Miles), data = data, x = TRUE, y = TRUE)
    X = m_tmp$x
    y = m_tmp$y
    
    m <- lm(Fuel ~ Tax + Dlic + Income + log(Miles), data = data, subset = -index)
    y_predict <- predict(object = m, newdata = data[index, ])
    
    sigma <- sqrt(sum(m$residuals^2) / m$df.residual)
    t = (y[index] - y_predict) / 
        (sigma * sqrt(1 + t(X[index, ]) %*% solve(t(X[-index, ]) %*% X[-index, ]) %*% X[index, ]))
    p_value = (1 - pt(abs(t), df = m$df.residual)) * 2 * nrow(data)
    data.frame(t, p_value)

}

test_influence <- function(data, index)
{
    m_full <- lm(Fuel ~ Tax + Dlic + Income + log(Miles), data = data)
    m_noindex <- lm(Fuel ~ Tax + Dlic + Income + log(Miles), data = data, subset = -index)
    
    y_predict_full <- predict(object = m_full, newdata = data)
    y_predict_noindex <- predict(object = m_noindex, newdata = data)
    
    sigma_2 <- sum(m_full$residuals^2) / m_full$df.residual
    
    D <- t(y_predict_full - y_predict_noindex) %*% (y_predict_full - y_predict_noindex) / 
        (length(coef(m_full)) * sigma_2)
        
    data.frame(D)
}

test_outlier(data, match("HI", rownames(data)))
test_influence(data, match("HI", rownames(data)))

rstudent(lm(Fuel ~ Tax + Dlic + Income + log(Miles), data = data))["AL"]
cooks.distance(lm(Fuel ~ Tax + Dlic + Income + log(Miles), data = data))



