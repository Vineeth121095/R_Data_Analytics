#Customer Churn Prediction
library(readxl)
Cellphone <- read_excel("D:/PGCBAA/7 Predictive Modeling using R/Project/Cellphone.xlsx")
View(Cellphone)
#Loading & Exploring Data
summary(Cellphone)
#Check for missing values
colSums(is.na(Cellphone))
#Insights on Churn Rate Analysis
#Count churned vs. non-churned
#Count churned vs. non-churned
table(Cellphone$Churn)
#Visualizing churn distribution
install.packages("ggplot2")
ggplot(Cellphone, aes(x = Churn, fill = Churn)) +  geom_bar() +  labs(title = "Churn Distribution", x = "Churn (Yes/No)", y = "Count") +  theme_minimal()
ggplot2(Cellphone, aes(x = Churn, fill = Churn)) +  geom_bar() +  labs(title = "Churn Distribution", x = "Churn (Yes/No)", y = "Count") +  theme_minimal()
library(ggplot2)
ggplot2(Cellphone, aes(x = Churn, fill = Churn)) +  geom_bar() +  labs(title = "Churn Distribution", x = "Churn (Yes/No)", y = "Count") +  theme_minimal()
#Insight 1: Relationship Between Monthly Charges & Churn
ggplot(Cellphone, aes(x = Churn, y = MonthlyCharge, fill = Churn)) + geom_boxplot() + labs(title = "Monthly Charge vs. Churn", x = "Churn", y = "Monthly Charge") + theme_minimal()
ggplot2(Cellphone, aes(x = Churn, y = MonthlyCharge, fill = Churn)) + geom_boxplot() + labs(title = "Monthly Charge vs. Churn", x = "Churn", y = "Monthly Charge") + theme_minimal()
install.packages("ggplot2")
library(ggplot2)
Cellphone$Churn <- as.factor(Cellphone$Churn)
#Boxplot to compare Monthly Charge between churners and non-churners
ggplot(Cellphone, aes(x = Churn, y = MonthlyCharge, fill = Churn)) + geom_boxplot() + labs(title = "Monthly Charge vs. Churn", x = "Churn", y = "Monthly Charge") + theme_minimal()
#Insight 2
#Boxplot to compare tenure between churners and non-churners
ggplot(Cellphone, aes(x = Churn, y = AccountWeeks, fill = Churn)) + geom_boxplot() + labs(title = " AccountWeeks vs. Churn", x = "Churn", y = " AccountWeeks (Weeks)") + theme_minimal()
# Convert to DataFrame
Cellphone <- as.data.frame(Cellphone)
#Splitting the data into test and train
set.seed(123)
splitIndex <- createDataPartition(Cellphone$Churn, p = 0.7, list = FALSE)
createDataPartition(Cellphone$Churn, p = 0.7, list = FALSE)
install.packages(caret)
library(caret)
splitIndex <- createDataPartition(Cellphone$Churn, p = 0.7, list = FALSE)
train_data <- Cellphone[splitIndex, ]
test_data <- Cellphone[-splitIndex, ]
table(train_data$Churn)
table(test_data$Churn)
#Fitting the data into a logistic regression model
model <- glm(Churn ~ AccountWeeks + ContractRenewal + DataPlan + DataUsage +
               CustServCalls + DayMins + DayCalls + MonthlyCharge,
             data = Cellphone, family = binomial)
summary(model)
#Predict Probabilities
Cellphone$predicted_probs <- predict(model, type = "response")
#Generate ROC Curve
roc_curve <- roc(Cellphone$Churn, Cellphone$predicted_probs)
install.packages("pROC")
library(pROC)
roc_curve <- roc(Cellphone$Churn, Cellphone$predicted_probs)
# Basic ROC Plot
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for Customer Churn Prediction")
# Printing AUC value
cat("AUC =", auc(roc_curve), "\n")
#Enhanced ggplot2 ROC Plot
roc_data <- data.frame(
  TPR = rev(roc_curve$sensitivities),
  FPR = rev(1 - roc_curve$specificities)
)
ggplot(roc_data, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(linetype = "dashed", color = "red") +  # 45-degree reference line
  labs(title = "ROC Curve for Customer Churn Prediction",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal()
ggplot2(roc_data, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(linetype = "dashed", color = "red") +  # 45-degree reference line
  labs(title = "ROC Curve for Customer Churn Prediction",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal()
library(ggplot2)
ggplot2(roc_data, aes(x = FPR, y = TPR)) +
  +     geom_line(color = "blue", size = 1) +
  +     geom_abline(linetype = "dashed", color = "red") +  # 45-degree reference line
  +     labs(title = "ROC Curve for Customer Churn Prediction",
             +          x = "False Positive Rate",
             ggplot(roc_data, aes(x = FPR, y = TPR)) +
               +     geom_line(color = "blue", size = 1) +
               +     geom_abline(linetype = "dashed", color = "red") +  # 45-degree reference line
               +     labs(title = "ROC Curve for Customer Churn Prediction",
                          +          x = "False Positive Rate",
                          ggplot(roc_data, aes(x = FPR, y = TPR)) + geom_line(color = "blue", size = 1) + geom_abline(linetype = "dashed", color = "red") + labs(title = "ROC Curve for Customer Churn Prediction",
                                                                                                                                                                 x = "False Positive Rate",
                                                                                                                                                                 y = "True Positive Rate") +
                            theme_minimal()