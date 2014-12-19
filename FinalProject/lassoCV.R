require(glmnet)

lassoCV <- function(X, y, X_test, y_test, family = "gaussian")
{
    result <- list(prediction = NULL, error = NULL, coef = NULL, model = NULL)
    ## set.seed(1000)
    X <- as.matrix(X)
    y <- as.matrix(y)
    X_test <- as.matrix(X_test)
    y_test <- as.matrix(y_test)
    
    if (family == "gaussian")
    {
        mCv <- cv.glmnet(X, y, nfolds = 10, family = "gaussian")
        m <- glmnet(X, y, family = "gaussian", 
                    lambda = mCv$lambda[order(mCv$cvm)[1]])
        result$model <- m
        result$coef <- m$beta
        result$prediction <- predict(m, newx = X_test)
        result$error <- mean((result$prediction - y_test)^2)
    }
    else
    {
        mCv <- cv.glmnet(X, y, family = "binomial", nfolds = 10)
        m <- glmnet(X, y, family = "binomial", 
                    lambda = mCv$lambda[order(mCv$cvm)[1]])
        result$model <- m
        result$coef <- m$beta
        result$prediction <- as.numeric(predict(m, newx = X_test) > 0)
        tmp <- table(result$prediction, y_test)
        result$error <- tmp[1,2] + tmp[2,1]
    }
    result
}