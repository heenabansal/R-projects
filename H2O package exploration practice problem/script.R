path <- "F:\\Courses\\R301\\H2O package exploration practice problem"
setwd(path)

#install and load the package
library(data.table)
library(ggplot2)
library(gmodels)
library(dummies)
library(h2o)

#load data using fread
train <- fread("train.csv", stringsAsFactors = T)
test <- fread("test.csv", stringsAsFactors = T)

dim(train)
dim(test)
str(train)

sub_mean <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID, Purchase = mean(train$Purchase))
write.csv(sub_mean, file = "first_sub.csv", row.names = F)

test[,Purchase := mean(train$Purchase)]
c <- list(train, test)
combin <- rbindlist(c)

combin[,prop.table(table(Gender))] 
combin[,prop.table(table(Age))]
combin[,prop.table(table(City_Category))]
combin[,prop.table(table(Stay_In_Current_City_Years))]
length(unique(combin$Product_ID))
length(unique(combin$User_ID))
colSums(is.na(combin))

ggplot(combin, aes(Age, fill = Gender)) + geom_bar()
ggplot(combin, aes(Age, fill = City_Category)) + geom_bar()

CrossTable(combin$Occupation, combin$City_Category)
combin[,Product_Category_2_NA := ifelse(sapply(combin$Product_Category_2, is.na) ==    TRUE,1,0)]
combin[,Product_Category_3_NA := ifelse(sapply(combin$Product_Category_3, is.na) ==  TRUE,1,0)]
#impute missing values
combin[,Product_Category_2 := ifelse(is.na(Product_Category_2) == TRUE, "-999",  Product_Category_2)]
combin[,Product_Category_3 := ifelse(is.na(Product_Category_3) == TRUE, "-999",  Product_Category_3)]
levels(combin$Stay_In_Current_City_Years)[levels(combin$Stay_In_Current_City_Years) ==  "4+"] <- "4"
levels(combin$Age)[levels(combin$Age) == "0-17"] <- 0
levels(combin$Age)[levels(combin$Age) == "18-25"] <- 1
levels(combin$Age)[levels(combin$Age) == "26-35"] <- 2
levels(combin$Age)[levels(combin$Age) == "36-45"] <- 3
levels(combin$Age)[levels(combin$Age) == "46-50"] <- 4
levels(combin$Age)[levels(combin$Age) == "51-55"] <- 5
levels(combin$Age)[levels(combin$Age) == "55+"] <- 6

#convert age to numeric
combin$Age <- as.numeric(combin$Age)

#convert Gender into numeric
combin[, Gender := as.numeric(as.factor(Gender)) - 1]

combin[, User_Count := .N, by = User_ID]
combin[, Product_Count := .N, by = Product_ID]

combin[, Mean_Purchase_Product := mean(Purchase), by = Product_ID]
combin[, Mean_Purchase_User := mean(Purchase), by = User_ID]
combin <- dummy.data.frame(combin, names = c("City_Category"), sep = "_")
sapply(combin, class)
combin$Product_Category_2 <- as.integer(combin$Product_Category_2)
combin$Product_Category_3 <- as.integer(combin$Product_Category_3)
combin$Stay_In_Current_City_Years <- as.integer(combin$Stay_In_Current_City_Years)

c.train <- combin[1:nrow(train),]
c.test <- combin[-(1:nrow(train)),]
c.train <- c.train[c.train$Product_Category_1 <= 18,]

localH2O <- h2o.init(nthreads = -1)
h2o.init()
train.h2o <- as.h2o(c.train)
test.h2o <- as.h2o(c.test)
colnames(train.h2o)
y.dep <- 14
x.indep <- c(3:13,15:20)
regression.model <- h2o.glm( y = y.dep, 
                             x = x.indep, 
                             training_frame = train.h2o, family = "gaussian")
#for logistic regression
# regression.model <- h2o.glm( y = y.dep, 
#                              x = x.indep, 
#                              training_frame = train.h2o, family = "binomial")


h2o.performance(regression.model)

#make predictions
predict.reg <- as.data.frame(h2o.predict(regression.model, test.h2o))
sub_reg <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID, Purchase =  predict.reg$predict)

write.csv(sub_reg, file = "sub_reg.csv", row.names = F)


























