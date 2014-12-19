require(randomForest)

RF <- function(X, y, X_test, y_test, family = "gaussian")
{
    result <- list(prediction = NULL, error = NULL, model = NULL)
    data <- data.frame(X, y)
    if (family == "gaussian")
    {
        m <- randomForest(y ~ ., data = data, ntree = 400)
        result$model <- m
        result$prediction <- predict(m, X_test)
        result$error <- mean((result$prediction - y_test)^2)
    }
    else
    {
        m <- randomForest(as.factor(y) ~ ., data = data, ntree = 400)
        result$model <- m
        result$prediction <- predict(m, X_test)
        tmp <- table(result$prediction, y_test)
        result$error <- tmp[1,2] + tmp[2,1]
    }
    result
}