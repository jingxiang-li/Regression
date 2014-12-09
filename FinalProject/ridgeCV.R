ridgeCV <- function(X, y, family = "gaussian")
{
    ## set.seed(1000)
    if (family == "gaussian")
    {
        X <- as.matrix(X)
        y <- as.matrix(y)
        mCv <- cv.glmnet(X, y, nfolds = 10, family = "gaussian", alpha = 0)
        m <- glmnet(X, y, family = "gaussian", 
                    lambda = mCv$lambda[order(mCv$cvm)[1]], alpha = 0)
        m
    }
    else
    {
        X <- as.matrix(X)
        y <- as.factor(y)
        mCv <- cv.glmnet(X, y, family = "binomial", nfolds = 10, alpha = 0)
        m <- glmnet(X, y, family = "binomial", 
                    lambda = mCv$lambda[order(mCv$cvm)[1]], alpha = 0)
        m
    }
}