options(scipen=999) # force full notation not scientific
library(tidyverse)
library(here)
library(corrgram)
library(naniar)
library(treeheatr)
library(rpart.plot)
library(rpart)
library(olsrr)
library(MASS)
library(caret)
library(pheatmap)
library(ROCR)
library(dplyr)

#### Setup / read in data ####
data_path <- here("data", "airline_passenger_satisfaction.csv")
dict_path <- here("data", "data_dictionary.csv")
df <- read.csv(data_path)
dict <- read.csv(dict_path) 

# Set the seed for replicable results
set.seed(123)  # You can choose any integer as the seed

#### EDA ####
# very little missing data, only 393 rows missing arrival.delay data
skimr::skim(df)

# some strong correlations to take note of
#corrgram(sampled_data)
colnames(df)

# filter out small amount of missing data and convert all chr variables to factors
df <- df %>%
  filter(complete.cases(.)) %>% 
  mutate_if(is.character, as.factor)

# List of 1-5 satisfaction survey vars to be converted to factors
selected_vars <- c(
  "Departure.and.Arrival.Time.Convenience", 
  "Ease.of.Online.Booking",
  "Check.in.Service", 
  "Online.Boarding", 
  "Gate.Location",
  "On.board.Service", 
  "Seat.Comfort", 
  "Leg.Room.Service",
  "Cleanliness", 
  "Food.and.Drink", 
  "In.flight.Service",
  "In.flight.Wifi.Service", 
  "In.flight.Entertainment",
  "Baggage.Handling"
)

# Convert selected variables to factors
df <- df %>%
  mutate(across(all_of(selected_vars), as.factor))

# ensure that "Satisfied" is the reference level of the dep var, for ease of model interpretation
df$Satisfaction <- relevel(df$Satisfaction, ref = "Satisfied")

#### Create training / testing datasets ####
# Replace "your_data" with your dataset and "n" with the sample size
sampled_data <- df %>%
  sample_n(size = 1000)  # Change the sample size as needed

# Create training and testing datasets
all.idx <- 1:nrow(sampled_data)
assess.idx <- sample(all.idx,size=round(0.2*nrow(sampled_data),0))
train.idx <- all.idx[!(all.idx %in% assess.idx)]

samp_df.train <- sampled_data[train.idx,]
samp_df.test <- sampled_data[assess.idx,]

#### Tree Model using rpart ####
fit <- rpart(Satisfaction ~ ., method="class", data=samp_df.train)

# We will use this variable below in the ROC curve code.  Change "Admit" to the 
#name of your dependent variable.
depvariable <- "Satisfaction"
#dev.new(width = 10, height = 6)
rpart.plot(fit, extra = 101, type = 4, nn = TRUE)

#Display decision tree
plot(fit, uniform = TRUE, margin=.05)
text(fit, use.n=TRUE, all=TRUE, cex=0.6) 

# experimenting with a fancy visualization
# treeheatr demo: https://www.youtube.com/watch?v=aNAMcoZ5j1s
for_heattree <- partykit::as.party(fit)

# lighter = higher number
# So the lime green are the 1s, dark blue must be the 0s.
# wonder if it would be more useful with a classification tree rather than regression
heat_tree(x = samp_df.train,
          task = "classification",
          terminal_vars = NULL,
          target_lab = "Satisfaction")

# Tree Predictions
#predict the outcome using the test dataset
treepred1 <- predict(fit, newdata = samp_df.test, type="class")

#Place the prediction variable back in the test dataset
samp_df.test$treepred1 <- treepred1

# #### Regression Model via backwards AIC ####
# reg1 <- glm(Satisfaction ~ ., data=samp_df.train, family = binomial)
# #reg_ols <- ols_step_backward_aic(reg1)
# reg1_aic <- stepAIC(reg1, direction="backward")
# summary(reg1_aic)  # skipping the final AIC pass
# 
# #predict the outcome using the test dataset
# reg1_aic_pred <- predict(reg1_aic, samp_df.test, type="response")
# 
# 
# # Threshold predicted probabilities to classify as "Satisfied" or "Neutral or Dissatisfied"
# hist(reg1_aic_pred)
# # there is a clear gap between the two different sets of responses, with no predictions between .5 and .7. So using .6 as the cutoff for now. 
# # Come back later and adjust using Kappa stat. 
# # Convert predicted probabilities to a factor with specified levels
# reg1_aic_pred <- factor(ifelse(reg1_aic_pred >= 0.6, "Satisfied", "Neutral or Dissatisfied"), levels = c("Satisfied", "Neutral or Dissatisfied"))
# 
# #Place the prediction variable back in the test dataset
# samp_df.test$reg1_aic_pred <- reg1_aic_pred
# 
# # Check regression assumptions — does not look good lol. Come back to this. 
# par(mfrow = c(2, 2))
# plot(reg1_aic)

#### Calculate accuracy metrics ####
# for the tree-based model
tree_confusion <- confusionMatrix(samp_df.test$treepred1, samp_df.test$Satisfaction)
tree_accuracy <- tree_confusion$overall['Accuracy']

# for the logistic regression model
# reg_confusion <- confusionMatrix(samp_df.test$reg1_aic_pred, samp_df.test$Satisfaction)
# reg_accuracy <- reg_confusion$overall['Accuracy']

# Display accuracy metrics
cat("Tree Model Accuracy: ", tree_accuracy, "\n")
# cat("Logistic Regression Model Accuracy: ", reg_accuracy, "\n")


#### Tree model accuracy ####

varImp(fit)%>%
  arrange(desc(Overall))

# Assuming 'treepred1' and 'Satisfaction' are factors
confusion_matrix <- confusionMatrix(samp_df.test$treepred1, samp_df.test$Satisfaction)

# Get the confusion matrix as a table
confusion_matrix_table <- as.table(confusion_matrix$table)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix_table)) / sum(confusion_matrix_table)

# Create a heatmap of the confusion matrix colored by accuracy
pheatmap(confusion_matrix_table, 
         cluster_cols = FALSE,
         cluster_rows = FALSE,
         main = paste("Confusion Matrix (Accuracy = ", round(accuracy, 2), ")"),
         border_color = "white",
         fontsize = 10,
         show_rownames = TRUE,
         show_colnames = TRUE)

# Sensitivity: TruePositives / TotalActualPositives
# In this case the sensitivity is 71/79 or  0.90
# Specificity: TrueNegatives / TotalActualNegatives
# In this case the specificity is 105/121 or 0.87
confusionMatrix(samp_df.test$treepred1, samp_df.test$Satisfaction)
