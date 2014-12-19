require(earth)

MARS <- function(X, y, X_test, y_test, family = "gaussian", degree = 1)
{
    result <- list(prediction = NULL, error = NULL, model = NULL)
    data <- data.frame(X, y)
    if (family == "gaussian")
    {
        penalty <- seq(1.5, 4.5, .5)
        tmp <- vector("numeric", length = length(penalty))
        for (i in 1:length(penalty))
        {
            m <- earth(y ~ ., data = data, degree = degree, 
                       penalty = penalty[i], nfold = 10)
            tmp[i] = m$cv.rsq.tab[11,2]
        }
        m <- earth(y ~ ., data = data, degree = degree, 
                   penalty = penalty[which.max(tmp)])
        result$model <- m
        result$prediction <- predict(m, X_test)
        result$error <- mean((result$prediction - y_test)^2)
    }
    else
    {
        penalty <- seq(1.5, 4.5, .5)
        tmp <- vector("numeric", length = length(penalty))
        for (i in 1:length(penalty))
        {
            m <- earth(y ~ ., data = data, degree = degree, glm = list(family = binomial),
                       penalty = penalty[i], nfold = 10)
            tmp[i] = m$cv.auc.tab[11,2]
        }
        m <- earth(y ~ ., data = data, glm = list(family = binomial), 
                   penalty = penalty[which.max(tmp)], degree = degree)
        result$model <- m
        result$prediction <- as.numeric(predict(m, X_test) > 0)
        tmp <- table(result$prediction, y_test)
        result$error <- tmp[1,2] + tmp[2,1]
    }
    result
}