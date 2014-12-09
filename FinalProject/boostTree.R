boostTree <- function(X, y, family = "gaussian")
{   
    if (family == "gaussian")
    {
        data <- data.frame(X, y)
        m = gbm(y ~ ., data = data, distribution="gaussian",     # see the help for other choices
                n.trees=400,                # number of trees
                shrinkage=0.05,              # shrinkage or learning rate,
                # 0.001 to 0.1 usually work
                interaction.depth=2,         # 1: additive model, 2: two-way interactions, etc.
                bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
                train.fraction = 1,        # fraction of data for training,
                # first train.fraction*N used for training
                n.minobsinnode = 10,         # minimum total weight needed in each node
                cv.folds = 10,                # do 3-fold cross-validation
                keep.data=TRUE,              # keep a copy of the dataset with the object
                verbose=FALSE,               # don't print out progress
                n.cores=1)                   # use only a single core (detecting #cores is
        best.iter <- gbm.perf(m, method="cv")
        gbm(y ~ ., data = data, distribution="gaussian",     # see the help for other choices
            n.trees=best.iter,                # number of trees
            shrinkage=0.05,              # shrinkage or learning rate,
            # 0.001 to 0.1 usually work
            interaction.depth=2,         # 1: additive model, 2: two-way interactions, etc.
            bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
            train.fraction = 1,        # fraction of data for training,
            # first train.fraction*N used for training
            n.minobsinnode = 10,         # minimum total weight needed in each node
            cv.folds = 10,                # do 3-fold cross-validation
            keep.data=TRUE,              # keep a copy of the dataset with the object
            verbose=FALSE,               # don't print out progress
            n.cores=1)                  
    }
    else
    {
        data <- data.frame(X, y = as.factor(y))
        m = gbm(y ~ ., data = data, distribution="bernoulli",     # see the help for other choices
                n.trees=400,                # number of trees
                shrinkage=0.05,              # shrinkage or learning rate,
                # 0.001 to 0.1 usually work
                interaction.depth=1,         # 1: additive model, 2: two-way interactions, etc.
                bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
                train.fraction = 1,        # fraction of data for training,
                # first train.fraction*N used for training
                n.minobsinnode = 10,         # minimum total weight needed in each node
                cv.folds = 3,                # do 3-fold cross-validation
                keep.data=FALSE,              # keep a copy of the dataset with the object
                verbose=FALSE,               # don't print out progress
                n.cores=1)                   # use only a single core (detecting #cores is
        best.iter <- gbm.perf(m, method="cv")
        gbm(y ~ ., data = data, distribution="bernoulli",     # see the help for other choices
                n.trees=best.iter,                # number of trees
                shrinkage=0.05,              # shrinkage or learning rate,
                # 0.001 to 0.1 usually work
                interaction.depth=2,         # 1: additive model, 2: two-way interactions, etc.
                bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
                train.fraction = 1,        # fraction of data for training,
                # first train.fraction*N used for training
                n.minobsinnode = 5,         # minimum total weight needed in each node
                cv.folds = 10,                # do 3-fold cross-validation
                keep.data=TRUE,              # keep a copy of the dataset with the object
                verbose=FALSE,               # don't print out progress
                n.cores=1)      
    }
    
    
}