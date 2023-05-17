# Clear the workspace
rm(list = ls())
cat("\014")

# Data Input

df<- read.csv("startup data.csv" , stringsAsFactors = F)

# Importing required libraries:

library(dplyr)
library(e1071)
library(rpart)
library(rpart.plot)
library(caret)
library(ggplot2)


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

#KNN________.........................................................................................

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



########### 2. K-Nearest Neighbors ########### 
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
ggplot(data=knnFit$results, aes(k, Kappa)) + geom_line() + scale_x_continuous(breaks=1:10)

# Evaluate classifier performance on testing data
actual_knn <- test.df_knn$status
knnPredict <- predict(knnFit, test.df_knn)
cm_knn <- table(knnPredict, actual_knn)
cm_knn 
# alternative way to get a comprehensive set of statistics
confusionMatrix(knnPredict, actual_knn, positive="acquired")


#Applying PCA + KNN
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

# Here we will be using 19 PC's for our model

fit_knn_pca <- train(status ~ ., 
                     data=knn_pca_train_data[,1:26], method = "knn", trControl = ctrl, preProcess = c("center","scale"),
                     tuneGrid = expand.grid(k = 1:10))
fit_knn_pca

ggplot(data=fit_knn_pca$results, aes(k, Kappa)) + geom_line() + scale_x_continuous(breaks=1:10)

# Evaluate classifier performance on testing data
actual_knn_pca <- knn_pca_test_data$status
Predict_knn_pca <- predict(fit_knn_pca, knn_pca_test_data)
cm_knn_pca <- table(Predict_knn_pca, actual_knn_pca)
cm_knn_pca

confusionMatrix(table(Predict_knn_pca,actual_knn_pca), positive='acquired')

