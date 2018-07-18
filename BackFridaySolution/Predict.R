# ---------------------------------------------------------------------------
# Black Friday Problem Prediction
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Load required libraries
library(data.table)
library(ggplot2)
library(gmodels)
library(dummies)   # used for one hot encoding
library(h2o)
options(stringsAsFactors = TRUE)

# Set Working Directory
setwd("C:/Kamal/Work/19_AV/BlackFriday_Prediction/")

# ---------------------------------------------------------------------------
# Step 1: Read train and test dataset and get mean of purchase variable
train <- fread("train.csv")
test <- fread("test.csv")

dim(test)
dim(train)
str(train)

sub_mean <- data.frame(User_ID = test$User_ID, Product_ID=test$Product_ID, Purchase=mean(train$Purchase))
write.csv(sub_mean,file = "submission_1.csv", row.names = FALSE)

summary(train)
summary(test)

# Combine dataset
test[,Purchase := mean(train$Purchase)]
c <- list(train,test)
combin <- rbindlist(c)

# ---------------------------------------------------------------------------
# Step 2: Further exploratory data analysis

# Univariate Analysis

# Analyzing Gender variable
combin[,prop.table(table(Gender))]

# Age variable
combin[,prop.table(table(Age))]

# City category variable
combin[,prop.table(table(combin$City_Category))]

# Stay in Current Years variable
combin[,prop.table((table(combin$Stay_In_Current_City_Years)))]

# Unique values in ID variables
length(unique(combin$Product_ID))

length(unique(combin$User_ID))

# missing values
colSums(is.na(combin))

# So analysis results of our univariate analysis::->
# We need to encode Gender variable into 0 and 1 (good practice).
# We’ll also need to re-code the Age bins.
# Since there are three levels in City_Category, we can do one-hot encoding.
# The “4+” level of Stay_in_Current_Years needs to be revalued.
# The data set does not contain all unique IDs. This gives us enough hint for feature engineering.
# Only 2 variables have missing values. In fact, a lot of missing values, which could be capturing a hidden trend. We’ll need to treat them differently.


# Bivariate Analysis

# Age vs Gender
ggplot(combin,aes(Age,fill=Gender)) + geom_bar()

# Age vs City Category
ggplot(combin,aes(Age,fill=City_Category)) + geom_bar()

CrossTable(combin$Occupation,combin$City_Category)

# ---------------------------------------------------------------------------
# Step 3: Data Manipulation

# In this part, we’ll create new variables, revalue existing variable and treat missing values. In simple words, we’ll get our data ready for modeling stage.
# Let’s start with missing values. We saw Product_Category_2 and Product_Category_3 had a lot of missing values. To me, this suggests a hidden trend which can be mapped by creating a new variable. So, we’ll create a new variable which will capture NAs as 1 and non-NAs as 0 in the variables Product_Category_2 and Product_Category_3.

# Create a new variable for missing values
combin[,Product_Category_2_NA := ifelse(sapply(combin$Product_Category_2, is.na) == TRUE, 1, 0)]
combin[,Product_Category_3_NA := ifelse(sapply(combin$Product_Category_3, is.na) == TRUE, 1, 0)]

# Impute missing values
combin[,Product_Category_2 := ifelse(is.na(combin$Product_Category_2) == TRUE,"-999", Product_Category_2)]
combin[,Product_Category_3 := ifelse(is.na(combin$Product_Category_3) == TRUE,"-999", Product_Category_3)]

# we’ll  revalue variable levels as inferred from our univariate analysis.
# Set Column level
levels(combin$Stay_In_Current_City_Years)[levels(combin$Stay_In_Current_City_Years) == "4+"] <- "4"

# Recode Age Groups
levels(combin$Age)[levels(combin$Age) == "0-17"] <- 0
levels(combin$Age)[levels(combin$Age) == "18-25"] <- 1
levels(combin$Age)[levels(combin$Age) == "26-35"] <- 2
levels(combin$Age)[levels(combin$Age) == "36-45"] <- 3
levels(combin$Age)[levels(combin$Age) == "46-50"] <- 4
levels(combin$Age)[levels(combin$Age) == "51-55"] <- 5
levels(combin$Age)[levels(combin$Age) == "55+"] <- 6

# Convert Age to Numeric
combin$Age <- as.numeric(combin$Age)

# Convert Gender to Numeric
combin[,Gender := as.numeric(as.factor(Gender)) - 1]

# Let’s create a new variable which captures the count of these ID variables. Higher user count suggests that a particular user has purchased products multiple times. High product count suggests that a product has been purchased many a times, which shows its popularity.
#User Count
combin[,User_Count := .N,by= User_ID]
#Product_Count
combin[,Product_Count := .N, by=Product_ID]

# Also, we can calculate the mean purchase price of a product. Because, lower the purchase price, higher will be the chances of that product being bought or vice versa. Similarly, we can create another variable which maps the average purchase price by user i.e. how much purchase (on an average) is made by a user.
#Mean Purchase of Product
combin[,Mean_Purchase_Product := mean(Purchase), by = Product_ID]
#Mean Purchase of User
combin[,Mean_Purchase_User := mean(Purchase), by = User_ID]

# we are only left with one hot encoding of City_Category variable. 
combin <- dummy.data.frame(combin,names = c("City_Category"),sep = "_")

# Before, proceeding to modeling stage, let’s check data types of variables once, and make the required changes, if necessary.

sapply(combin, class)

# Converting Product Category 2 and 3
combin$Product_Category_2 <- as.integer(combin$Product_Category_2)
combin$Product_Category_3 <- as.integer(combin$Product_Category_3)

# ---------------------------------------------------------------------------
# Step 4: Modeling

c.train <- combin[1:nrow(train),]
c.test <- combin[1:nrow(test),]

# Remove Noise - As discovered in beginning that the variable Product_Category_1 in train has some noise. Let’s remove it as well by selecting all rows in Product_Category_1 upto 18, thereby dropping rows which has category level 19 & 20.
c.train <- c.train[c.train$Product_Category_1 <= 18,]

# Launch H2O cluster
localH2O <- h2o.init(nthreads = -1)

# data to h2o cluster
train.h2o <- as.h2o(c.train)
test.h2o <- as.h2o(c.test)

# Check column index number
colnames(train.h2o)
# Dependent variable --> y is Purchase
y.dep <- 14
# Independent variables --> dropping id variables, no longer needed
x.indep <- c(3:13,15:20)

# ---------------------------------------------------------------------------
# Step 4a : Multiple Regression in H2O
regression.model <- h2o.glm(y=y.dep,x=x.indep,training_frame = train.h2o,family = "gaussian")

h2o.performance(regression.model)

# GLM algorithm in H2O can be used for all types of regression such as lasso, ridge, logistic, linear etc. A user only needs to modify the family parameter accordingly. For example: To do logistic regression, you can write family = “binomial”.
# So, after we print the model results, we see that regression gives a poor R² value i.e. 0.326. It means that only 32.6% of the variance in the dependent variable is explained by independent variable and rest is unexplained. This shows that regression model is unable to capture non linear relationships.

# Make predictions
predict.reg <- as.data.frame(h2o.predict(regression.model,test.h2o))

sub_reg <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID, Purchase = predict.reg$predict)

write.csv(sub_reg,file = "submission_2.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# Step 4b : Random Forest in H2O
system.time(rf.model <- h2o.randomForest(y=y.dep,x=x.indep,training_frame = train.h2o,ntrees = 1000,mtries = 3,max_depth = 4,seed=1234) )

h2o.performance(rf.model)
h2o.varimp(rf.model)
# making predictions
system.time(predict.rf <- as.data.frame(h2o.predict(rf.model,test.h2o)))
sub_rf <- data.frame(User_ID=test$User_ID,Product_ID=test$Product_ID,Purchase=predict.rf$predict)
write.csv(sub_rf,file = "submission_3.csv", row.names = FALSE)


# ---------------------------------------------------------------------------
# Step 4c : GBM in H2O

system.time(gbm.model <- h2o.gbm(y=y.dep,x=x.indep,training_frame = train.h2o,ntrees = 1000, max_depth = 4,learn_rate = 0.01,seed = 1234) )

h2o.performance(gbm.model)

# making predictions
system.time(predict.gbm <- as.data.frame(h2o.predict(gbm.model,test.h2o)))
sub_gbm <- data.frame(User_ID=test$User_ID,Product_ID=test$Product_ID,Purchase=predict.gbm$predict)
write.csv(sub_gbm,file = "submission_4.csv", row.names = FALSE)


# ---------------------------------------------------------------------------
# Step 4d : Deep Learning in H2O

system.time(
  dlearning.model <- h2o.deeplearning(y = y.dep,
                                      x = x.indep,
                                      training_frame = train.h2o,
                                      epoch = 60,
                                      hidden = c(100,100),
                                      activation = "Rectifier",
                                      seed = 1234
  )
)

h2o.performance(dlearning.model)
# making predictions
predict.dl <- as.data.frame(h2o.predict(dlearning.model, test.h2o))
sub_dl <- data.frame(User_ID=test$User_ID,Product_ID=test$Product_ID,Purchase=predict.dl$predict)
write.csv(sub_dl,file = "submission_5.csv", row.names = FALSE)

