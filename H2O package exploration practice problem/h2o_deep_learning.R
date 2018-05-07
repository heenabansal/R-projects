library(h2o)
h2o.init(nthreads = -1)
h2o.no_progress()

# This step takes a few seconds bc we have to download the data from the internet...
train_file <- "/home/fractaluser/Backup/Courses/R301/H2O package exploration practice problem/train.csv.gz"
test_file <- "/home/fractaluser/Backup/Courses/R301/H2O package exploration practice problem/test.csv.gz"
train <- h2o.importFile(train_file)
test <- h2o.importFile(test_file)

y <- "C785"  #response column: digits 0-9
x <- setdiff(names(train), y)  #vector of predictor column names

# Since the response is encoded as integers, we need to tell H2O that
# the response is in fact a categorical/factor column.  Otherwise, it 
# will train a regression model instead of multiclass classification.
train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])

dl_fit1 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "dl_fit1",
                            hidden = c(20,20),
                            seed = 1)

dl_fit3 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "dl_fit3",
                            epochs = 50,
                            hidden = c(20,20),
                            nfolds = 3,                            #used for early stopping
                            score_interval = 1,                    #used for early stopping
                            stopping_rounds = 5,                   #used for early stopping
                            stopping_metric = "misclassification", #used for early stopping
                            stopping_tolerance = 1e-3,             #used for early stopping
                            seed = 1)


