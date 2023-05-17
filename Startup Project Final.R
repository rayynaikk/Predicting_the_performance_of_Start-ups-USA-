#-------------------------------BA with R Project-------------------------------
rm(list = ls())
cat("\014")

# Data Input

setwd("C:/Users/SHEKHAR/Desktop/UTD MSBA Spring/BA with R/Project 1")
df<- read.csv("startup data.csv" , stringsAsFactors = F)

# Importing required libraries:

library(adabag)
library(dplyr)
library(e1071)
library(rpart)
library(rpart.plot)
library(caret)
library(ggplot2)
library(ROCit)

#----------------Data Exploration---------------------
# Removing unwanted columns:


df <- subset(df , select = -c(Unnamed..0, Unnamed..6, id, object_id, zip_code, 
                              city, labels , state_code.1))

# Changing the date formats

df$founded_at <- as.Date(df$founded_at, '%m/%d/%Y')
df$closed_at <- as.Date(df$closed_at, '%m/%d/%Y')
df$first_funding_at <- as.Date(df$first_funding_at, '%m/%d/%Y')
df$last_funding_at <- as.Date(df$last_funding_at, '%m/%d/%Y')

# Finding and treating the missing values:

missing_count <- colSums(is.na(df))
df_missing <- as.data.frame(missing_count); df_missing

df$age_first_milestone_year <- ifelse(is.na(df$age_first_milestone_year),median(df$age_first_milestone_year,na.rm = T),df$age_first_milestone_year)
df$age_last_milestone_year <- ifelse(is.na(df$age_last_milestone_year),median(df$age_last_milestone_year,na.rm = T),df$age_last_milestone_year)


missing_count_2 <- colSums(is.na(df))
df_missing_2 <- as.data.frame(missing_count_2); df_missing_2

# Removing not required data sets
rm(df_missing, df_missing_2, missing_count, missing_count_2)


# Here we can see there are missing values in one date column i.e. closed_at and 2 age columns age_first_milestone_year , age_last_milestone_year
# Hence we have imputed the null values with the median values for these 2 columns


# Finding duplicates:

table(duplicated(df))

# Hence we can say that there are no duplicate rows in the data

# Finding outliers:

# Selecting numerical variable for checking outliers
df_outliers <- subset(df , select = c(relationships,
                                      funding_rounds,
                                      milestones,
                                      avg_participants))

# Boxplot for all these variables
boxplot(df_outliers, main = "Multiple Boxplots", xlab = "Variables", ylab = "Values")

# Using IQR approach to treat outliers for Avg_participants variable:

q1 <- quantile(df$avg_participants, probs = 0.25)
q3 <- quantile(df$avg_participants, probs = 0.75)

iqr <- q3-q1
LL_avg_participants <- q1 - 1.5 * iqr
UL_avg_participants <- q3 + 1.5 * iqr

df_clean <- df[df$avg_participants >= LL_avg_participants & df$avg_participants <= UL_avg_participants,]


# Using IQR approach to treat outliers for relationships variable:

q1_rel <- quantile(df_clean$relationships, probs = 0.25)
q3_rel <- quantile(df_clean$relationships, probs = 0.75)

iqr_rel <- q3-q1
LL_rel <- q1_rel - 1.5 * iqr_rel
UL_rel <- q3_rel + 1.5 * iqr_rel

df_clean <- df_clean[df_clean$relationships >= LL_rel & df_clean$relationships <= UL_rel,]


# Boxplot for cleaned variables 

df_outliers_clean <- subset(df_clean , select = c(relationships,
                                      funding_rounds,
                                      milestones,
                                      avg_participants))


boxplot(df_outliers_clean, main = "Multiple Boxplots", xlab = "Variables", ylab = "Values")


# Removing unwanted data frames:
rm(df_outliers, df_outliers_clean)


#------------------------------Data Exploration & Visualization-----------------

#str 
str(df)

#Scatter plot of latitude and longitude
ggplot(df, aes(x = longitude, y = latitude)) +
  geom_point() +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw()

#Trend in funding rounds
ggplot(df, aes(x = funding_rounds)) +
  geom_histogram(fill = "skyblue", color = "white", binwidth = 1) +
  ggtitle("Distribution of Funding Rounds") +
  xlab("Number of Funding Rounds") +
  ylab("Count")

#Regional trends in startup funding 
df %>% 
  group_by(state_code) %>% 
  summarize(avg_funding = mean(funding_total_usd))%>% 
  ggplot(aes(x=state_code, y=avg_funding)) +
  geom_bar(stat="identity", fill="skyblue", color="White") +
  ggtitle("Average Funding by State") +
  xlab("State") +
  ylab("Average Funding (in USD)")

#Bar plot of category code
ggplot(df, aes(y = category_code)) +
  geom_bar() +
  labs(y = "Category Code", x = "Count") +
  theme_bw()

#Box plot of funding total by category
ggplot(df, aes(y = category_code, x = funding_total_usd/1000000)) +
  geom_boxplot() +
  labs(y = "Category Code", x = "Funding Total (Millions of USD)") +
  theme_bw() +
  scale_x_continuous(limits = c(0, 300))


#Bar plot of status by category
ggplot(df, aes(y = category_code, fill = status)) +
  geom_bar() +
  labs(y = "Category Code", x = "Count") +
  theme_bw() +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"))

#Regional trends in startup funding
df %>%
  group_by(status) %>%
  summarize(avg_funding = mean(funding_total_usd)) %>%
  ggplot(aes(x = status, y = avg_funding)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "white") +
  ggtitle("Average Funding by Status") +
  xlab("Status") +
  ylab("Average Funding (in USD)")


#Bar chart of count of companies by state:
df_state <- df %>%
  group_by(state_code) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(df_state, aes(y = state_code, x = count, fill = state_code)) +
  geom_col() +
  labs(y = "State Code", x = "Count of Companies") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



# select relevant numeric variables
startup_data_num <- select_if(df, is.numeric)
# compute correlation matrix
corr_matrix <- cor(startup_data_num, use = "pairwise.complete.obs")


# Install ggcorrplot package
#install.packages("ggcorrplot")

# Load ggcorrplot package
library(ggcorrplot)

# plot correlation heat map
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", 
           lab_size = 3, ggtheme = ggplot2::theme_gray, 
           colors = c("#6D9EC1", "white", "#E46726"))

#-------------------------- Exploratory analysis-------------------------------

#Multiple regression

df_mr <- subset(df_clean,select =-c(name, latitude, longitude, closed_at))

df_mr$status <- as.factor(df_mr$status)
model.dummy1 <- lm(funding_total_usd ~ status + factor(category_code), data=df_mr);summary(model.dummy1) 
confint(model.dummy1, level = 0.95)


model.dummy3 <- lm(funding_total_usd ~ status + has_VC + is_top500 + has_VC*is_top500, data=df_mr);summary(model.dummy3) 
confint(model.dummy3, level = 0.95)



# ----------------------------Decision Tree Classifier Model:-------------------

# creating a subset after removing some unwanted variables
df_dt <- subset(df_clean,select =-c(name, latitude, longitude, closed_at, category_code))

# making factors for categorical variables
df_dt$state_code <- as.factor(df_dt$state_code)
df_dt$is_CA <- as.factor(df_dt$is_CA)
df_dt$is_NY <- as.factor(df_dt$is_NY)
df_dt$is_MA <- as.factor(df_dt$is_MA)
df_dt$is_TX <- as.factor(df_dt$is_TX)
df_dt$is_otherstate <- as.factor(df_dt$is_otherstate)
df_dt$is_software <- as.factor(df_dt$is_software)
df_dt$is_web <- as.factor(df_dt$is_web)
df_dt$is_mobile <- as.factor(df_dt$is_mobile)
df_dt$is_enterprise <- as.factor(df_dt$is_enterprise)
df_dt$is_advertising <- as.factor(df_dt$is_advertising)
df_dt$is_gamesvideo <- as.factor(df_dt$is_gamesvideo)
df_dt$is_ecommerce <- as.factor(df_dt$is_ecommerce)
df_dt$is_biotech <- as.factor(df_dt$is_biotech)
df_dt$is_consulting <- as.factor(df_dt$is_consulting)
df_dt$is_othercategory <- as.factor(df_dt$is_othercategory)
df_dt$has_VC <- as.factor(df_dt$has_VC)
df_dt$has_angel <- as.factor(df_dt$has_angel)
df_dt$has_roundA <- as.factor(df_dt$has_roundA)
df_dt$has_roundB <- as.factor(df_dt$has_roundB)
df_dt$has_roundC <- as.factor(df_dt$has_roundC)
df_dt$has_roundD <- as.factor(df_dt$has_roundD)
df_dt$is_top500 <- as.factor(df_dt$is_top500)
df_dt$status <- as.factor(df_dt$status)


# Split the data
set.seed(345)
train <- sample(1:nrow(df_dt), nrow(df_dt)*(2/3))
status.train <- df_dt[train,]
status.test <- df_dt[-train,]



fit.dt <- rpart(status ~ ., 
             data=status.train,
             method="class",  
             control=rpart.control(xval=0, minsplit=50), 
             parms=list(split="gini"))
fit.dt


rpart.plot(fit.dt, type = 3, extra = 5, main="Classification Tree for Startup Status Prediction")



status.pred <- predict(fit.dt, status.train, type="class")

status.actual <- status.train$status

confusionMatrix(table(status.pred,status.actual), positive='acquired')



status.pred_test <- predict(fit.dt, status.test, type="class")
status.actual_test <- status.test$status
confusionMatrix(table(status.pred_test,status.actual_test), positive='acquired')


# Decision Tree model with Post pruning:
fit.big <- rpart(status ~ ., 
                 data=status.train,
                 control=rpart.control(xval=10, minsplit=2, cp=0))
nrow(fit.big$frame)

rpart.plot(fit.big, type = 3, extra = 5, main="Classification Tree for Startup Status Prediction")

status.pred <- predict(fit.big, status.train, type="class")

status.actual <- status.train$status
# confusion matrix for training data status.train 
confusionMatrix(table(status.pred,status.actual), positive='acquired')

# confusion matrix for hold out data 
status.pred <- predict(fit.big, status.test, type="class")
status.actual <- status.test$status
confusionMatrix(table(status.pred,status.actual), positive='acquired')


bestcp <- fit.big$cptable[which.min(fit.big$cptable[,"xerror"]),"CP"]
bestcp   

# The lowest error occurs at CP = bestcp
# We can use this for post-pruning
fit.post <- prune.rpart(fit.big, cp=bestcp)
nrow(fit.post$frame)  

# plot the tree - NOTE. same as pre-pruned tree fit.small2 
prp(fit.post, type = 3, extra = 5, under = TRUE, split.font = 1, varlen = -10, 
    main="Post-prune Tree with best cp")  

# compute the confusion matrices and accuracy 
status.actual_PoP_train <- status.train$status
status.actual_PoP_test <- status.test$status



confusionMatrix(table(predict(fit.post, status.train, type="class"),
                      status.actual_PoP_train), positive='acquired')

confusionMatrix(table(predict(fit.post, status.test, type="class"),
                      status.actual_PoP_test), positive='acquired')

status.pred.PoP <- predict(fit.post, status.test, type="class")

# Applying PCA
# preparing test and train data set and removing status column


pca_df <- subset(df, select = -c(name, latitude, longitude, closed_at, category_code))

set.seed(345)
pca.train <- sample(1:nrow(pca_df), nrow(pca_df)*(2/3))
pca_train <- pca_df[pca.train,]
pca_test <- pca_df[-pca.train,]

zero_var_cols <- which(var(pca_train[, -c(1, 2, 3, 4, 36)]) == 0); zero_var_cols

# Subset dataset to exclude zero-variance columns
pca_train_nozero <- pca_train[, -c(1, 2, 3, 4, 36)][, -zero_var_cols]

# Perform PCA on the subsetted dataset
pca_result <- prcomp(pca_train_nozero, scale = TRUE)

summary(pca_result)

# Visualize the elbow plot

pca.var <- pca_result$sdev^2

# this is what we had in the summary: i.e., proportion of variance explained by each principal component
pca.pve <- data.frame( pve = pca.var/sum(pca.var), component = c(1:30))  
# plot
plot(pca.pve$pve)  # the "elbow" of the graph <=> the number of PCs to keep
g <- ggplot(pca.pve, aes(component, pve))  
g + geom_point() + labs(title="Scree Plot", x="Component Number", y="PVE") +
  scale_x_continuous(breaks=seq(1,30,by=1))


pca_train_data <- data.frame(status = pca_train$status ,pca_result$x)
pca_test_data <- data.frame(status = pca_test$status ,predict(pca_result, pca_test[ , -c(1,2,3,4,36)]))


# classification

# Here we will be using 19 PC's for our model

fit_pca <- rpart(status ~ ., 
             data=pca_train_data[,1:20],
             method="class",  
             control=rpart.control(xval=0, minsplit=50), 
             parms=list(split="gini"))
fit_pca


rpart.plot(fit_pca, type = 3, extra = 5, main="Classification Tree for Startup Status Prediction with PCA")



status.pred_pca <- predict(fit_pca, pca_train_data[,1:20], type="class")

status.actual_pca <- pca_train_data$status

confusionMatrix(table(status.pred_pca,status.actual_pca), positive='acquired')



status.pred_pca_test <- predict(fit_pca, pca_test_data[,1:20], type="class")
status.actual_pca_test <- pca_test_data$status
confusionMatrix(table(status.pred_pca_test,status.actual_pca_test), positive='acquired')

#-----------------------------Ensemble Methods----------------------------------

df_bagt=df_dt


# Split the data
set.seed(345)
train_bag <- sample(1:nrow(df_bagt), nrow(df_bagt)*(2/3))
train_bagt <- df_bagt[train_bag,]
test_bagt <- df_bagt[-train_bag,]

#single tree
fit.tree_bagt <- rpart(status ~ ., data = train_bagt)
pred_bagt <- predict(fit.tree_bagt, test_bagt, type = "class")
cm_bagt <- confusionMatrix(pred_bagt, test_bagt$status);cm_bagt


#bagging
fit.bagging <- bagging(status ~ ., data = train_bagt, mfinal = 20)
pred_bag_train <-predict(fit.bagging, train_bagt, type = "class")
cm_bag_train <- confusionMatrix(as.factor(pred_bag_train$class), train_bagt$status);cm_bag_train


pred_bag <- predict(fit.bagging, test_bagt, type = "class")
cm_bag <- confusionMatrix(as.factor(pred_bag$class), test_bagt$status);cm_bag

# boosting
# "mfinal" an integer, the number of iterations for which boosting is run 
fit.boosting <- boosting(status ~ ., data = train_bagt, mfinal = 20)

pred_boost_train <- predict(fit.boosting, train_bagt, type = "class")
cm_boost_train <- confusionMatrix(as.factor(pred_boost_train$class), train_bagt$status);cm_boost_train

pred_boost <- predict(fit.boosting, test_bagt, type = "class")
cm_boost <- confusionMatrix(as.factor(pred_boost$class), test_bagt$status);cm_boost

# compare across different methods
result_bagt <- rbind(cm_bagt$overall["Accuracy"], cm_bag$overall["Accuracy"], cm_boost$overall["Accuracy"])
row.names(result_bagt) <- c("single tree", "bagging", "boosting")
result_bagt

# TPR (sensitivity, recall)
result_bagt <- rbind(cm_bagt$byClass['Sensitivity'], cm_bag$byClass['Sensitivity'], cm_boost$byClass['Sensitivity'])
row.names(result_bagt) <- c("single tree", "bagging", "boosting")
result_bagt



# -------------------------Logistic Regression----------------------------------


df_logit <- df_clean
df_logit$status <- as.factor(df_logit$status)


df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], 
                                       as.factor)


set.seed(2)   # for reproducible results
train <- sample(1:nrow(df_logit), (0.6)*nrow(df_logit))
train.df <- df_logit[train,]
test.df <- df_logit[-train,]


logit.reg <- glm(status ~  age_first_funding_year + age_last_funding_year + age_first_milestone_year + 
                   age_last_milestone_year + relationships + funding_rounds + funding_total_usd + milestones + 
                   is_CA + is_NY + is_MA + is_TX + is_otherstate + category_code + is_software + is_web + is_mobile + 
                   is_enterprise + is_advertising + is_gamesvideo + is_ecommerce + is_biotech + is_consulting + 
                   is_othercategory + has_VC + has_angel + has_roundA + has_roundB + has_roundC + has_roundD + 
                   avg_participants + is_top500, data = df_logit, family = "binomial") 


summary(logit.reg)

logitPredict <- predict(logit.reg, train.df, type = "response")
# we choose 0.5 as the cutoff here for 1 vs. 0 classes
logitPredictClass <- ifelse(logitPredict > 0.5, 1, 0)

# For train data
actual <- train.df$status
predict <- logitPredictClass
cm <- table(predict, actual)
cm

tp <- cm[2,2]
tn <- cm[1,1]
fp <- cm[2,1]
fn <- cm[1,2]
# accuracy
(tp + tn)/(tp + tn + fp + fn)
# TPR = Recall = Sensitivity
tp/(fn+tp)
# TNR = Specificity
tn/(fp+tn)
# FPR
fp/(fp+tn)
# FNR
fn/(fn+tp)



# use predict() with type = "response" to compute predicted probabilities. 
logitPredict_test <- predict(logit.reg, test.df, type = "response")
# we choose 0.5 as the cutoff here for 1 vs. 0 classes
logitPredictClass <- ifelse(logitPredict_test > 0.5, 1, 0)

actual_test <- test.df$status
predict <- logitPredictClass
cm <- table(predict, actual_test)
cm

# consider class "1" as positive
tp <- cm[2,2]
tn <- cm[1,1]
fp <- cm[2,1]
fn <- cm[1,2]
# accuracy
(tp + tn)/(tp + tn + fp + fn)
# TPR = Recall = Sensitivity
tp/(fn+tp)
# TNR = Specificity
tn/(fp+tn)
# FPR
fp/(fp+tn)
# FNR
fn/(fn+tp)


# --------------------------------------KNN------------------------------------

df_knn <- subset(df_clean,select =-c(name, latitude, longitude, closed_at, category_code))

# convert output as factor
df_knn$state_code <- as.factor(df_knn$state_code)
df_knn$status <- as.factor(df_knn$status)
df_knn$funding_rounds <- as.factor(df_knn$funding_rounds)
df_knn$milestones <- as.factor(df_knn$milestones)
df_knn$is_CA <- as.factor(df_knn$is_CA)
df_knn$is_NY <- as.factor(df_knn$is_NY)
df_knn$is_MA <- as.factor(df_knn$is_MA)
df_knn$is_TX <- as.factor(df_knn$is_TX)
df_knn$is_otherstate <- as.factor(df_knn$is_otherstate)
df_knn$has_VC <- as.factor(df_knn$has_VC)
df_knn$has_angel <- as.factor(df_knn$has_angel)
df_knn$has_roundA <- as.factor(df_knn$has_roundA)
df_knn$has_roundB <- as.factor(df_knn$has_roundB)
df_knn$has_roundC <- as.factor(df_knn$has_roundC)
df_knn$has_roundD <- as.factor(df_knn$has_roundD)
df_knn$is_top500 <- as.factor(df_knn$is_top500)
df_knn$is_software <- as.factor(df_knn$is_software)
df_knn$is_web <- as.factor(df_knn$is_web)
df_knn$is_mobile <- as.factor(df_knn$is_mobile)
df_knn$is_enterprise <- as.factor(df_knn$is_enterprise)
df_knn$is_advertising <- as.factor(df_knn$is_advertising)
df_knn$is_gamesvideo <- as.factor(df_knn$is_gamesvideo)
df_knn$is_ecommerce <- as.factor(df_knn$is_ecommerce)
df_knn$is_biotech <- as.factor(df_knn$is_biotech)
df_knn$is_consulting <- as.factor(df_knn$is_consulting)
df_knn$is_othercategory <- as.factor(df_knn$is_othercategory)
df_knn$status <- as.factor(df_knn$status)



# split the data into training and test data sets
set.seed(2)   # for reproducible results
train_knn <- sample(1:nrow(df_knn), (0.6)*nrow(df_knn))
train.df_knn <- df_knn[train_knn,]
test.df_knn <- df_knn[-train_knn,]



#install.packages('caret')
library(caret)
# Checking distribution of outcome classes -> very few class = "1"
prop.table(table(train.df_knn$status)) * 100
prop.table(table(test.df_knn$status)) * 100
prop.table(table(df_knn$status)) * 100

# 10-fold cross-validation
ctrl <- trainControl(method="cv", number=10) 
# use preProcess to normalize the predictors
# "center" subtracts the mean; "scale" divides by the standard deviation
knnFit <- train(status ~ state_code + relationships + funding_rounds + funding_total_usd + 
                  milestones + is_CA + is_NY + is_MA + is_TX + is_otherstate + is_software + is_web + 
                  is_mobile + is_enterprise + is_advertising + is_gamesvideo + is_ecommerce + is_biotech + is_consulting + 
                  is_othercategory + has_VC + has_angel + has_roundA + has_roundB + has_roundC + has_roundD + 
                  avg_participants + is_top500, 
                data = train.df_knn, method = "knn", trControl = ctrl, preProcess = c("center","scale"),
                tuneGrid = expand.grid(k = 1:10))

# print the knn fit for different values of k
# Kappa is a more useful measure to use on problems that have imbalanced classes.
knnFit
# plot the # of neighbors vs. accuracy (based on repeated cross validation)
plot(knnFit)
ggplot(data=knnFit$results, aes(k, Accuracy)) + geom_line() + scale_x_continuous(breaks=1:10)

# Evaluate classifier performance on testing data
actual_knn <- test.df_knn$status
knnPredict <- predict(knnFit, test.df_knn)
cm_knn <- table(knnPredict, actual_knn)
cm_knn 
# alternative way to get a comprehensive set of statistics
confusionMatrix(knnPredict, actual_knn, positive="acquired")


#------------------------------Applying PCA + KNN-------------------------------
df_knn_pca <- subset(df, select = -c(name, latitude, longitude, closed_at, category_code))

set.seed(345)
knn_pca.train <- sample(1:nrow(df_knn_pca), nrow(df_knn_pca)*(2/3))
knn_pca_train <- df_knn_pca[knn_pca.train,]
knn_pca_test <- df_knn_pca[-knn_pca.train,]

zero_var_cols_knn <- which(var(knn_pca_train[, -c(1, 2, 3, 4, 36)]) == 0); zero_var_cols_knn

# Subset dataset to exclude zero-variance columns
pca_train_nozero_knn <- knn_pca_train[, -c(1, 2, 3, 4, 36)][, -zero_var_cols_knn]

# Perform PCA on the subsetted dataset
knn_pca_result <- prcomp(pca_train_nozero_knn, scale = TRUE)

summary(knn_pca_result)
knn_pca_result$rotation

# Visualize the elbow plot

knn_pca.var <- knn_pca_result$sdev^2

# this is what we had in the summary: i.e., proportion of variance explained by each principal component
knn_pca.pve <- data.frame( knn_pve = knn_pca.var/sum(knn_pca.var), component = c(1:30))  
# plot
plot(knn_pca.pve$knn_pve)  # the "elbow" of the graph <=> the number of PCs to keep
g_knn_pca <- ggplot(knn_pca.pve, aes(component, knn_pve))  
g_knn_pca + geom_point() + labs(title="Scree Plot", x="Component Number", y="PVE") +
  scale_x_continuous(breaks=seq(1,30,by=1))


knn_pca_train_data <- data.frame(status = knn_pca_train$status ,knn_pca_result$x)
knn_pca_test_data <- data.frame(status = knn_pca_test$status ,predict(knn_pca_result, knn_pca_test[ , -c(1,2,3,4,36)]))


# knn

# Here we will be using 25 PC's for our model

fit_knn_pca <- train(status ~ ., 
                     data=knn_pca_train_data[,1:26], method = "knn", trControl = ctrl, preProcess = c("center","scale"),
                     tuneGrid = expand.grid(k = 1:10))
fit_knn_pca

ggplot(data=fit_knn_pca$results, aes(k, Accuracy)) + geom_line() + scale_x_continuous(breaks=1:10)

# Evaluate classifier performance on testing data
actual_knn_pca <- knn_pca_test_data$status
Predict_knn_pca <- predict(fit_knn_pca, knn_pca_test_data)
cm_knn_pca <- table(Predict_knn_pca, actual_knn_pca)
cm_knn_pca

confusionMatrix(table(Predict_knn_pca,actual_knn_pca), positive='acquired')


# -------------------------Naive Bayes------------------------------------------


df_nb <- subset(df_clean,select =-c(name, latitude, longitude, closed_at, category_code))
df_nb[sapply(df_nb, is.character)] <- lapply(df_nb[sapply(df_nb, is.character)], 
                                             as.factor)

set.seed(2)   # for reproducible results
train_nb <- sample(1:nrow(df_nb), (2/3)*nrow(df_nb))
train.dfnb <- df_nb[train_nb,]
test.dfnb <- df_nb[-train_nb,]

fit.nb <- naiveBayes(status ~  state_code + relationships + funding_rounds + funding_total_usd + 
                       milestones + is_CA + is_NY + is_MA + is_TX + is_otherstate + is_software + is_web + 
                       is_mobile + is_enterprise + is_advertising + is_gamesvideo + is_ecommerce + is_biotech + is_consulting + 
                       is_othercategory + has_VC + has_angel + has_roundA + has_roundB + has_roundC + has_roundD + 
                       avg_participants + is_top500,
                     data = train.dfnb) 

fit.nb

# Evaluate classifier performance on train data
actual.nb_train <- train.dfnb$status
nbPredict_train <- predict(fit.nb, train.dfnb)
cm_nb_train <- table(nbPredict_train, actual.nb_train)
cm_nb_train
confusionMatrix(table(nbPredict_train,actual.nb_train), positive='acquired')


# Evaluate classifier performance on testing data
actual.nb <- test.dfnb$status
nbPredict <- predict(fit.nb, test.dfnb)
cm_nb <- table(nbPredict, actual.nb)
cm_nb
confusionMatrix(table(nbPredict,actual.nb), positive='acquired')

# Applying PCA + NB

df_nb_pca <- subset(df_clean,select =-c(name, latitude, longitude, closed_at, category_code))

set.seed(345)   
train_nb_pca <- sample(1:nrow(df_nb_pca), (2/3)*nrow(df_nb_pca))
train.dfnb_pca <- df_nb_pca[train_nb_pca,]
test.dfnb_pca <- df_nb_pca[-train_nb_pca,]

pca_out_nb = train.dfnb_pca[, -c(1, 2, 3, 4, 36)]
pca_out_nb

nb_pca_result <- prcomp(pca_out_nb, scale = TRUE)

summary(nb_pca_result)
nb_pca_result$rotation


nb_pca_train_data <- data.frame(status = train.dfnb_pca$status ,nb_pca_result$x)
nb_pca_test_data <- data.frame(status = test.dfnb_pca$status ,predict(nb_pca_result, test.dfnb_pca[ , -c(1,2,3,4,36)]))

fit.nb_pca <- naiveBayes(status ~ .,
                         data = nb_pca_train_data[,1:26]) 

fit.nb_pca

# Evaluate classifier performance on training data
actual.nb_pca_train <- nb_pca_train_data$status
nbPredict_pca_train <- predict(fit.nb_pca, nb_pca_train_data)
cm_nb_pca_train <- table(nbPredict_pca_train, actual.nb_pca_train)
cm_nb_pca_train
confusionMatrix(table(nbPredict_pca_train,actual.nb_pca_train), positive='acquired')




# Evaluate classifier performance on testing data
actual.nb_pca <- nb_pca_test_data$status
nbPredict_pca <- predict(fit.nb_pca, nb_pca_test_data)
cm_nb_pca <- table(nbPredict_pca, actual.nb_pca)
cm_nb_pca
confusionMatrix(table(nbPredict_pca,actual.nb_pca), positive='acquired')



# ---------------------------------ROC Curve------------------------------------

# create ROC curve

status.pred_test <- as.numeric(status.pred_test)
roc_dt <- rocit(score = status.pred_test, class = status.actual) 

status.pred.PoP <- as.numeric(status.pred.PoP)
roc_dt.PoP <- rocit(score = status.pred.PoP, class = status.actual_PoP_test) 

status.pred_pca_test <- as.numeric(status.pred_pca_test)
roc_dt_PCA <- rocit(score = status.pred_pca_test, class = status.actual_pca_test) 


roc_logit <- rocit(score = logitPredict_test, class = actual_test)

knnPredict <- as.numeric(knnPredict)
roc_knn <- rocit(score = knnPredict, class = actual_knn)

Predict_knn_pca <- as.numeric(Predict_knn_pca)
roc_knn_pca <- rocit(score = Predict_knn_pca, class = actual_knn_pca)

Predict_nb <- as.numeric(nbPredict)
roc_nb <- rocit(score = Predict_nb, class = actual.nb)


Predict_nb_pca <- as.numeric(nbPredict_pca)
roc_nb_pca <- rocit(score = Predict_nb_pca, class = actual.nb_pca)


# plot multiple ROC curves
plot(roc_logit, col = c("blue", "black"), legend = FALSE, YIndex = FALSE)
lines(roc_dt$TPR ~ roc_dt$FPR, col = "red")
lines(roc_dt.PoP$TPR ~ roc_dt.PoP$FPR, col = "yellow")
lines(roc_dt_PCA$TPR ~ roc_dt_PCA$FPR, col = "orange")
lines(roc_knn$TPR ~ roc_knn$FPR, col = "green")
lines(roc_knn_pca$TPR ~ roc_knn_pca$FPR, col = "green4")
lines(roc_nb$TPR ~ roc_nb$FPR, col = "brown3")
lines(roc_nb_pca$TPR ~ roc_nb_pca$FPR, col = "brown4")

legend("bottomright", col = c("blue","red","yellow","orange","green",
                              "green4","brown3","brown4"),c("ROC for logit", 
                                                                "ROC for DT",
                                                                "ROC for DT PP",
                                                                "ROC for DT PCA",
                                                                "ROC for KNN",
                                                                "ROC for KNN+PCA",
                                                                "ROC for NA",
                                                                "ROC for NB+PCA"), lwd = 2)
                                                                #lwd for line width

# Area Under the curve
roc_logit$AUC  
roc_dt$AUC
roc_dt.PoP$AUC
roc_dt_PCA$AUC
roc_knn$AUC
roc_knn_pca$AUC
roc_nb$AUC
roc_nb_pca$AUC







