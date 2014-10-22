rm(list = ls())
require(alr4)


# 11.3  - --------------------------------------------------------------------

data (walleye)

Linf <- mean(walleye$length) + 4 * sd(walleye$length)
m0 <- lm(log(1 - length / Linf) ~ age, data = walleye)
K  <-  - coef(m0)[2]
t0 <- coef(m0)[1] / coef(m0)[2]

c1 <- nls(length ~ Linf * (1 - exp( - K * (age - t0))),
          start = list(Linf = Linf, K = K, t0 = t0),
          data = walleye)

c2 <- nls(length ~ (period == 1) * Linf1 * (1 - exp( - K1 * (age - t01))) +
                   (period == 2) * Linf2 * (1 - exp( - K2 * (age - t02))) +
                   (period == 3) * Linf3 * (1 - exp( - K3 * (age - t03))),
          start = list(Linf1 = Linf, Linf2 = Linf, Linf3 = Linf,
                       K1 = K, K2 = K, K3 = K,
                       t01 = t0, t02 = t0, t03 = t0),
          data = walleye)

c3 <- nls(length ~ (period == 1) * Linf * (1 - exp( - K1 * (age - t01))) +
                   (period == 2) * Linf * (1 - exp( - K2 * (age - t02))) +
                   (period == 3) * Linf * (1 - exp( - K3 * (age - t03))),
          start = list(Linf = Linf,
                       K1 = K, K2 = K, K3 = K,
                       t01 = t0, t02 = t0, t03 = t0),
          data = walleye)

c4 <- nls(length ~ (period == 1) * Linf1 * (1 - exp( - K * (age - t01))) +
                   (period == 2) * Linf2 * (1 - exp( - K * (age - t02))) +
                   (period == 3) * Linf3 * (1 - exp( - K * (age - t03))),
          start = list(Linf1 = Linf, Linf2 = Linf, Linf3 = Linf,
                       K = K,
                       t01 = t0, t02 = t0, t03 = t0),
          data = walleye)

c5 <- nls(length ~ (period == 1) * Linf1 * (1 - exp( - K1 * (age - t0))) +
                   (period == 2) * Linf2 * (1 - exp( - K2 * (age - t0))) +
                   (period == 3) * Linf3 * (1 - exp( - K3 * (age - t0))),
          start = list(Linf1 = Linf, Linf2 = Linf, Linf3 = Linf,
                       K1 = K, K2 = K, K3 = K,
                       t0 = t0),
          data = walleye)


anova(c1,c3,c2)
anova(c1,c4,c2)
anova(c1,c5,c2)


# 2 -----------------------------------------------------------------------
rm(list = ls())
require(MASS)
require(leaps) # a library for all subset selection

data(Boston)
nm <- colnames(Boston)
n <- nrow(Boston)
p <- ncol(Boston)

## full model, all subset selection based on AIC, 
## all subset selection based on BIC, Lasso, and ridge regression. 

## Full Model
m_full <- lm(medv ~ ., data = Boston)


## subset selection based on AIC and BIC
m_full <- lm(medv ~ ., data = Boston)
x <- Boston[, 1 : (p - 1)]
y <- Boston$medv
n <- nrow(Boston)
p <- ncol(Boston)

outs <- summary(regsubsets(x = x, y = y, nvmax = p, method = "exhaustive"))
RSS <- outs$rss

AIC <- n * log(RSS / (n)) + 2 * (apply(outs$which, 1, sum))
BIC <- n * log(RSS / (n)) + log(n) * (apply(outs$which, 1, sum))
ix_AIC <- which.min(AIC)
ix_BIC <- which.min(BIC)
x_AIC <- x[, outs$which[ix_AIC, -1]]
x_BIC <- x[, outs$which[ix_BIC, -1]]
data_AIC <- data.frame(x_AIC, medv = y)
data_BIC <- data.frame(x_BIC, medv = y)

m_AIC <- lm(medv ~ ., data = data_AIC)
m_BIC <- lm(medv ~ ., data = data_BIC)

## Lasso
require(lars)

X <- as.matrix(Boston[, 1 : (p - 1)])
y <- Boston$medv
n <- nrow(Boston)
p <- ncol(Boston)

m_lasso <- lars(x = X, y = y, type = "lasso")

## ridge regression
require(glmnet)

X <- as.matrix(Boston[, 1 : (p - 1)])
y <- Boston$medv
n <- nrow(Boston)
p <- ncol(Boston)

m_ridge <- glmnet(X, y, family = "gaussian", alpha = 0)  # defaut choice for lambda 
plot(ridge.mod, type.coef = "2norm")
cv.out <- cv.glmnet(x,y,alpha=0)  # default is 10 fold. Can change using, e.g., nfold=5
cv.glmnet(x,y,alpha=0,nfold=5)
cv.out$lambda.min   # best lambda
plot(cv.out)    # all the 200 predictors are involved at the different lambda values
ridge.pred=predict(ridge.mod,s=cv.out$lambda.min,newx=x)
predict(ridge.mod,type="coefficients",s=cv.out$lambda.min) # Look at the estimated coefficients
predict(ridge.mod,s=cv.out$lambda.min,newx=x[1,,drop=FALSE]) # predict for case 1


## Half Half CV
require(sampling)
require(MASS)
require(glmnet)
require(lars)
require(leaps)

rm(list = ls())

data(Boston)
n <- nrow(Boston)
p <- ncol(Boston)
X <- as.matrix(Boston[, 1 : (p - 1)])
y <- Boston$medv

set.seed(123123)
index <- as.logical(srswor(n / 2, n))
index_1 <- as.logical(1 - index)
index <- list(index, index_1)

error <- matrix(nrow = 5, ncol = 2)

## Half Half CV
for (i in 1 : 2)
{
    data_train <- Boston[index[[i]], ]
    data_test <- Boston[index[[3 - i]], ]
    
    X_train <- data_train[, 1 : (p - 1)]
    y_train <- data_train[, p]
    X_test <- data_test[, 1 : (p - 1)]
    y_test <- data_test[, p]
    n <- nrow(X_train)
    p <- ncol(X_train) + 1
    
    ## Full model
    m_full <- lm(medv ~ ., data = data_train)
    y_pred <- predict(object = m_full, newdata = X_test)
    error[1, i] <- mean((y_pred - y_test) ^ 2)
    
    cbind(1, as.matrix(X_test)) %*% as.matrix(coef(m_full))
    
    ## All Subset
    outs <- summary(regsubsets(x = X_train, y = y_train, 
                               nvmax = p, method = "exhaustive"))
    RSS <- outs$rss
    
    AIC <- n * log(RSS / (n)) + 2 * (apply(outs$which, 1, sum))
    BIC <- n * log(RSS / (n)) + log(n) * (apply(outs$which, 1, sum))
    ix_AIC <- which.min(AIC)
    ix_BIC <- which.min(BIC)
    x_AIC <- X_train[, outs$which[ix_AIC, -1]]
    x_BIC <- X_train[, outs$which[ix_BIC, -1]]
    data_AIC <- data.frame(x_AIC, medv = y_train)
    data_BIC <- data.frame(x_BIC, medv = y_train)
    m_AIC <- lm(medv ~ ., data = data_AIC)
    m_BIC <- lm(medv ~ ., data = data_BIC)
    y_pred <- predict(object = m_AIC, newdata = data.frame(X_test))
    error[2, i] <- mean((y_pred - y_test) ^ 2) 
    y_pred <- predict(object = m_BIC, newdata = data.frame(X_test))
    error[3, i] <- mean((y_pred - y_test) ^ 2)     
    
    ## Lasso
    m_lasso <- lars(x = as.matrix(X_train), y = y_train, type = "lasso")
    foo <- summary(m_lasso)
    RSS <- foo$Rss
    BIC <- n * log(RSS / n) + log(n) * foo$Df
    ix_BIC <- which.min(BIC)
    y_pred <- predict(object = m_lasso, newx = as.matrix(X_test), s = ix_BIC)
    error[4, i] <- mean((y_pred$fit - y_test) ^ 2)
    
    ## ridge
    m_ridge <- glmnet(as.matrix(X_train), y_train, 
                      family = "gaussian", alpha = 0, 
                      intercept=TRUE, lambda = seq(10, 0.01, -0.01), 
                      standardize = FALSE)
    tmp <- predict(m_ridge, newx = as.matrix(X_train))
    foo <- tmp - as.matrix(y_train) %*% matrix(1, nrow = 1, ncol = ncol(tmp))
    k <- function(x) {sum(x ^ 2)}
    RSS <- apply(foo, 2, k)
    BIC <- n * log(RSS / n) + log(n) * (m_ridge$df + 1)
    ix_BIC <- which.min(BIC)
    y_pred <- predict(m_ridge, newx = as.matrix(X_test), s = m_ridge$lambda[ix_BIC])
    error[5, i] <- mean((y_pred - y_test) ^ 2)
}

rownames(error) <- c("Full Model", "All Subset AIC", 
                     "All Subset BIC", "Lasso BIC", 
                     "Ridge BIC")
colnames(error) <- c("CV1", "CV2")
error
