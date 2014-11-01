rm(list = ls())
require(faraway)
require(MASS)


# Problem 2 ---------------------------------------------------------------


data(wbca)
m1 <- glm(Class ~ ., data = wbca, family = binomial)
## anova(m1, test = "Chisq")


m0 <- glm(Class ~ 1, data = wbca, family = binomial)
m_AIC <- step(m1, scope = list(lower = m0, upper = m1), 
     trace = FALSE, direction = "both")

newx <- wbca[1, -1]
newx[1, ] <- c(1, 1, 3, 2, 1, 1, 4, 1, 1)
foo <- predict(m_AIC, newdata = newx, se.fit = TRUE)
ConfInterval <- ilogit(c(foo$fit - 1.96 * foo$se.fit, 
                         foo$fit + 1.96 * foo$se.fit))
ConfInterval




cutoff <- 0.5
m_AIC_pred <- predict(m_AIC, type = "response") > cutoff
predictive <- factor(as.numeric(m_AIC_pred), labels = c("Malignant", "Benign"))
actual <- factor(wbca$Class, labels = c("Malignant", "Benign"))
table(Actual = actual, Predictive = predictive)
prop.table(table(Actual = actual, Predictive = predictive))


cutoff <- 0.9
m_AIC_pred <- predict(m_AIC, type = "response") > cutoff
foobar <- table(Actual = as.logical(wbca$Class), Predictive = m_AIC_pred)
prop.table(foobar, 1)


ix <- seq(from = 1, to = nrow(wbca), by = 3)
data_train <- wbca[-ix, ]
data_test <- wbca[ix, ]

m0 <- glm(Class ~ 1, data = data_train, family = binomial)
m1 <- glm(Class ~ ., data = data_train, family = binomial)

m_AIC <- step(m1, scope = list(lower = m0, upper = m1), 
              trace = FALSE, direction = "both")


cutoff <- 0.5
m_AIC_pred <- predict(m_AIC, newdata = data_test, type = "response") > cutoff
predictive <- factor(as.numeric(m_AIC_pred), labels = c("Malignant", "Benign"))
actual <- factor(data_test$Class, labels = c("Malignant", "Benign"))
table(Actual = actual, Predictive = predictive)
prop.table(table(Actual = actual, Predictive = predictive), 1)


cutoff <- 0.9
m_AIC_pred <- predict(m_AIC, newdata = data_test, type = "response") > cutoff
predictive <- factor(as.numeric(m_AIC_pred), labels = c("Malignant", "Benign"))
actual <- factor(data_test$Class, labels = c("Malignant", "Benign"))
table(Actual = actual, Predictive = predictive)
prop.table(table(Actual = actual, Predictive = predictive), 1)




# Problem 3 ---------------------------------------------------------------


# summary(pima)
# par(mfrow = c(3, 2))
# hist(pima$pregnant)
# hist(pima$glucose)
# hist(pima$diastolic)
# hist(pima$triceps)
# hist(pima$insulin)
# hist(pima$bmi)
data(pima)
data <- pima
data <- data[data$glucose != 0, ]
data <- data[data$diastolic != 0, ]
data <- data[data$bmi != 0, ]
m1 <- glm(test ~ ., data = data, family = binomial)



plot(data$test, data$diastolic)
diastolic_pos <- data$diastolic[data$test == 1]
diastolic_neg <- data$diastolic[data$test == 0]
boxplot(diastolic_pos, diastolic_neg,
        names = c("Positive", "Negative"), main = "Boxplot of Diastolic")

m0 <- glm(test ~ 1, data = data, family = binomial)
m_AIC <- step(m1, scope = list(lower = m0, upper = m1), 
              trace = FALSE, direction = "both")
summary(m_AIC)



newx <- data[1, -ncol(data)]
newx[1, ] <- c(1, 99, 64, 22, 76, 27, 0.25, 25)


# tmp = data[, -ncol(data)]
# minnorm = 123123123
# index = 0
# for (i in 1 : nrow(tmp)){
#     foonorm <- norm(tmp[i, ] - newx,  type = "2")
#     if (foonorm < minnorm)
#     {
#         minnorm = foonorm
#         index = i
#     }
# }



foo <- predict(m_AIC, newdata = newx, se.fit = TRUE)
ConfInterval <- ilogit(c(foo$fit - 1.96 * foo$se.fit, 
                         foo$fit + 1.96 * foo$se.fit))
ConfInterval


