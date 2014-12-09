stepwiseAIC <- function(X, y, family = "gaussian", outlier = FALSE)
{
    if (family == "gaussian")
    {
        data <- data.frame(X, y)
        ## Establish a model
        m0 <- lm(y ~ 1, data = data)
        m1 <- lm(y ~ ., data = data)
        m_AIC <- step(m1, scope = list(lower = m0, upper = m1),
                      direction = "both", trace = FALSE)
        ## Rremove Outliers
        if (outlier)
        {
            data <- data[-as.numeric(names(outlierTest(m_AIC)$bonf.p)), ]
            m0 <- lm(y ~ 1, data = data)
            m1 <- lm(y ~ ., data = data)
            m_AIC <- step(m1, scope = list(lower = m0, upper = m1),
                          direction = "both", trace = FALSE)
            m_AIC
        }
        else
        {
            m_AIC
        }
    }
    else 
    {
        data <- data.frame(X, y = as.factor(y))
        ## Establish a model
        m0 <- glm(y ~ 1, data = data, family = binomial)
        m1 <- glm(y ~ ., data = data, family = binomial)
        m_AIC <- step(m1, scope = list(lower = m0, upper = m1),
                      direction = "both", trace = FALSE)
        ## Rremove Outliers
        if (outlier)
        {
            data <- data[-as.numeric(names(outlierTest(m_AIC)$bonf.p)), ]
            m0 <- glm(y ~ 1, data = data, family = binomial)
            m1 <- glm(y ~ ., data = data, family = binomial)
            m_AIC <- step(m1, scope = list(lower = m0, upper = m1),
                          direction = "both", trace = FALSE)
            m_AIC
        }
        else
        {
            m_AIC
        }
    }
}