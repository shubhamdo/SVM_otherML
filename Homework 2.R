library(dplyr)
library(tidyverse)
library(e1071) # SVM Package
library(MASS)
library(rpart)
library(rpart.plot)
library(caret)
library(MLmetrics)

cell_churn <- read.csv("C:/Users/shubh/OneDrive - SRH IT/University/Semester 3/Analytics 3/Codes/cell2cellholdout.csv/cell2celltrain.csv",stringsAsFactors = TRUE)


str(cell_churn)

cell_churn_true <- cell_churn %>% 
  filter(cell_churn$Churn == "Yes")

cell_churn_false <- cell_churn %>% 
  filter(cell_churn$Churn == "No")

cell_churn_true <- head(cell_churn_true, 100)
cell_churn_false <- head(cell_churn_false, 100)
cell_churn <- rbind(cell_churn_true, cell_churn_false)

cell_churn$HandsetPrice <- as.numeric(cell_churn$HandsetPrice)
# ServiceArea_uniq <- unique(cell_churn$ServiceArea)
cell_churn$ServiceArea <- as.factor(cell_churn$ServiceArea)
cell_churn$ServiceArea <- gsub(" ","UNKNOWN",cell_churn$ServiceArea, fixed = TRUE)


write.csv(cell_churn, file = "C:/Users/shubh/OneDrive - SRH IT/University/Semester 3/Analytics 3/Codes/cell2cellholdout.csv/cell2celltrain_sample.csv")

cell_churn <- read.csv("C:/Users/shubh/OneDrive - SRH IT/University/Semester 3/Analytics 3/Codes/cell2cellholdout.csv/cell2celltrain_sample.csv",stringsAsFactors = TRUE)
str(cell_churn)



summary(cell_churn)
cell_churn <- na.omit(cell_churn)
# Non-numerical data such as categorical data are common in practice. Some classification methods are adaptive to categorical predictor variables in nature, but some methods can be only applied to continuous numerical data. Among the three classification methods, only Kernel Density Classification can handle the categorical variables in theory, while kNN and SVM are unable to be applied directly since they are based on the Euclidean distances. In order to define the distance metrics for categorical variables, the first step of preprocessing of the dataset is to use dummy variables to represent the categorical variables.
# Secondly, due to the distinct natures of categorical and numerical data, we usually need to standardize the numerical variables, such as the contributions to the euclidean distances from a numerical variable and a categorical variable are basically on the same level.
# Finally, the introduction of dummy variables usually increase the dimension significantly. By various experiments, we find that dimension reduction techniques such as PCA usually improve the performance of these three classifiers significantly.
# https://stats.libretexts.org/Bookshelves/Computing_and_Modeling/RTG%3A_Classification_Methods/4%3A_Numerical_Experiments_and_Real_Data_Analysis/Preprocessing_of_categorical_predictors_in_SVM%2C_KNN_and_KDC_(contributed_by_Xi_Cheng)
m <- svm(Churn~CustomerID+MonthlyRevenue+MonthlyMinutes+TotalRecurringCharge+DirectorAssistedCalls+OverageMinutes+
           RoamingCalls+PercChangeMinutes+PercChangeRevenues+DroppedCalls+BlockedCalls+UnansweredCalls+
           CustomerCareCalls+ThreewayCalls+ReceivedCalls+OutboundCalls+InboundCalls+PeakCallsInOut+
           OffPeakCallsInOut+DroppedBlockedCalls+CallForwardingCalls+CallWaitingCalls+MonthsInService+
           UniqueSubs+ActiveSubs+Handsets +HandsetModels +CurrentEquipmentDays +AgeHH1 +AgeHH2 +ChildrenInHH 
         +HandsetRefurbished +HandsetWebCapable +TruckOwner +RVOwner +Homeownership +BuysViaMailOrder 
         +RespondsToMailOffers +OptOutMailings +NonUSTravel +OwnsComputer +HasCreditCard +RetentionCalls 
         +RetentionOffersAccepted +NewCellphoneUser +NotNewCellphoneUser +ReferralsMadeBySubscriber 
         +IncomeGroup +OwnsMotorcycle +AdjustmentsToCreditRating +HandsetPrice +MadeCallToRetentionTeam 
         +CreditRating +PrizmCode +Occupation +MaritalStatus, data = cell_churn)

# m <- svm(Churn~., data = cell_churn)
# plot(m, cell_churn)



summary(m)


# new_data <- read.csv("C:/Users/shubh/OneDrive - SRH IT/University/Semester 3/Analytics 3/Codes/cell2cellholdout.csv/cell2cellholdout.csv",stringsAsFactors = TRUE)
# new_data$HandsetPrice <- as.numeric(new_data$HandsetPrice)
# new_data$ServiceArea <- gsub(" ","UNKNOWN",new_data$ServiceArea, fixed = TRUE)
# new_data <- new_data %>% dplyr::select(-Churn)
new_data1 <- cell_churn[1:50,]
# new_data1 = rbind(cell_churn[1:50,],new_data1)
#new_data1$Churn <- as.character(new_data1$Churn)
new_data = new_data1[1,]
# new_data1$ServiceArea <- as.factor(new_data1$ServiceArea)
# new_data1$ServiceArea <- gsub(" ","UNKNOWN",new_data1$ServiceArea, fixed = TRUE)
new_data$HandsetPrice <- as.numeric(new_data$HandsetPrice)
# new_data1 <- new_data1 %>% dplyr::select(-ServiceArea)
new_data <- na.omit(new_data)
p <- predict(m, newdata = new_data)
# View(new_data)
svm_cm <- confusionMatrix(predict(m, type = "class" ), reference = cell_churn$Churn)
table(true=cell_churn$Churn, predicted=predict(m))
# str(new_data1)
# str(cell_churn)

cell_churn <- na.omit(cell_churn)

dc_tree <- rpart(Churn~CustomerID+MonthlyRevenue+MonthlyMinutes+TotalRecurringCharge+DirectorAssistedCalls+OverageMinutes+
        RoamingCalls+PercChangeMinutes+PercChangeRevenues+DroppedCalls+BlockedCalls+UnansweredCalls+
        CustomerCareCalls+ThreewayCalls+ReceivedCalls+OutboundCalls+InboundCalls+PeakCallsInOut+
        OffPeakCallsInOut+DroppedBlockedCalls+CallForwardingCalls+CallWaitingCalls+MonthsInService+
        UniqueSubs+ActiveSubs+Handsets +HandsetModels +CurrentEquipmentDays +AgeHH1 +AgeHH2 +ChildrenInHH 
      +HandsetRefurbished +HandsetWebCapable +TruckOwner +RVOwner +Homeownership +BuysViaMailOrder 
      +RespondsToMailOffers +OptOutMailings +NonUSTravel +OwnsComputer +HasCreditCard +RetentionCalls 
      +RetentionOffersAccepted +NewCellphoneUser +NotNewCellphoneUser +ReferralsMadeBySubscriber 
      +IncomeGroup +OwnsMotorcycle +AdjustmentsToCreditRating +HandsetPrice +MadeCallToRetentionTeam 
      +CreditRating +PrizmCode +Occupation +MaritalStatus, data = cell_churn)
rpart.plot(dc_tree)

predict_unseen  <- predict(dc_tree, newdata = new_data, type = 'class')
new_data <- na.omit(new_data)

table(true=cell_churn$Churn, predicted=predict(dc_tree, type = "class" ))

dt_cm <- confusionMatrix(predict(dc_tree, type = "class" ), reference = cell_churn$Churn)
dt_cm
svm_cm



## Problem 2


insurance <- read.csv("C:/Users/shubh/OneDrive - SRH IT/University/Semester 3/Analytics 3/Codes/insurance.csv", stringsAsFactors = TRUE)
head(insurance)
# insurance$children <- as.factor(insurance$children)
str(insurance)

# unique(insurance$children)

insurance <- na.omit(insurance)

train_data <-  insurance[1:(nrow(insurance)*0.7),]
test_data <-  insurance[(nrow(insurance)*0.7):nrow(insurance),]


str(train_data)

## Linear Regression
lin.reg <- lm(charges ~ ., data = train_data)
summary(lin.reg)

ypred <- predict(lin.reg, newdata = test_data)

lm.rmse <- RMSE(y_pred = ypred, y_true = test_data$charges)

## Decision Tree


d.t <- rpart(charges ~ ., data = train_data)
summary(d.t)

ypred_dt <- predict(d.t, newdata = test_data)
rpart.plot(d.t)
dt.rmse <- RMSE(y_pred = ypred_dt, y_true = test_data$charges)
## SVM 

s.v.m <- svm(charges ~ ., data = train_data)
summary(s.v.m)

ypred_svm <- predict(s.v.m, newdata = test_data)

svm.rmse <- RMSE(y_pred = ypred_svm, y_true = test_data$charges)

lm.rmse
dt.rmse
svm.rmse
## Metrics
