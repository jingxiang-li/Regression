lassoCV <- function(X, y, family = "gaussian")
{
    set.seed(1000)
    if (family == "gaussian")
    {
        X <- as.matrix(X)
        y <- as.matrix(y)
        mCv <- cv.glmnet(X, y, nfolds = 10, family = "gaussian")
        lambdaLasso <- mCv$lambda[order(mCv$cvm)[1]]
        mCv <- cv.glmnet(X, y, nfolds = 10, family = "gaussian", alpha = 0)
        lambdaRidge <- mCv$lambda[order(mCv$cvm)[1]]
        alpha <- lambdaLasso / (lambdaLasso + lambdaRidge)
    
    }
}