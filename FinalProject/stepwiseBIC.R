stepwiseBIC <- function(X, y, family = "gaussian", outlier = FALSE)
{
    if (family == "gaussian")
    {
        data <- data.frame(X, y)
        ## Establish a model
        m0 <- lm(y ~ 1, data = data)
        m1 <- lm(y ~ ., data = data)
        m_BIC <- step(m1, scope = list(lower = m0, upper = m1),
                      direction = "both", trace = FALSE, k = log(nrow(data)))
        ## Rremove Outliers
        if (outlier)
        {
            data <- data[-as.numeric(names(outlierTest(m_BIC)$bonf.p)), ]
            m0 <- lm(y ~ 1, data = data)
            m1 <- lm(y ~ ., data = data)
            m_BIC <- step(m1, scope = list(lower = m0, upper = m1),
                          direction = "both", trace = FALSE, k = log(nrow(data)))
            m_BIC
        }
        else
        {
            m_BIC
        }
    }
    else 
    {
        data <- data.frame(X, y = as.factor(y))
        ## Establish a model
        m0 <- glm(y ~ 1, data = data, family = binomial)
        m1 <- glm(y ~ ., data = data, family = binomial)
        m_BIC <- step(m1, scope = list(lower = m0, upper = m1),
                      direction = "both", trace = FALSE)
        ## Rremove Outliers
        if (outlier)
        {
            data <- data[-as.numeric(names(outlierTest(m_BIC)$bonf.p)), ]
            m0 <- glm(y ~ 1, data = data, family = binomial)
            m1 <- glm(y ~ ., data = data, family = binomial)
            m_BIC <- step(m1, scope = list(lower = m0, upper = m1),
                          direction = "both", trace = FALSE, k = log(nrow(data)))
            m_BIC
        }
        else
        {
            m_BIC
        }
    }
}