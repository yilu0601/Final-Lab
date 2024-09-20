# Load data and packages
library(randomForest)
library(caret)

## Test data goes here
data <- 

# Remove ID and zip code
data <- subset(data, select = -c(HHKEY, ZIP_CODE))

# Change to factors
data$CC_CARD <- as.factor(data$CC_CARD)
data$WEB <- as.factor(data$WEB)
data$RESP <- as.factor(data$RESP)
data$CLUSTYPE <- as.factor(data$CLUSTYPE)

# Convert VALPHON to a single dummy variable
data$VALPHON <- ifelse(data$VALPHON == "Y", 1, 0)

# Dummy variables for CLUSTYPE
dummy_matrix <- model.matrix(~ CLUSTYPE - 1, data = data)

# Combine dummy variables with the original dataframe
data <- cbind(data, dummy_matrix)
data$CLUSTYPE <- NULL

### USE ORIGINAL TRAINING DATA FOR SCALING ###
og_data <- read.csv("Competition_data.csv")

# Remove ID and zip code
og_data <- subset(og_data, select = -c(HHKEY, ZIP_CODE))
# Change to factors
og_data$CC_CARD <- as.factor(og_data$CC_CARD)
og_data$WEB <- as.factor(og_data$WEB)
og_data$RESP <- as.factor(og_data$RESP)
og_data$CLUSTYPE <- as.factor(og_data$CLUSTYPE)

# Convert VALPHON to a single dummy variable
og_data$VALPHON <- ifelse(og_data$VALPHON == "Y", 1, 0)

# Dummy variables for CLUSTYPE
og_dummy_matrix <- model.matrix(~ CLUSTYPE - 1, data = og_data)

# Combine dummy variables with the original dataframe
og_data <- cbind(og_data, og_dummy_matrix)
og_data$CLUSTYPE <- NULL

# Partition
set.seed(1234)
train_indices <- createDataPartition(og_data$RESP, p = 0.8, list = FALSE)
train_data <- og_data[train_indices, ]

# Scale train data
preProc <- preProcess(subset(train_data, select = -c(RESP, CC_CARD, WEB, CLUSTYPE1, CLUSTYPE2, CLUSTYPE3, CLUSTYPE4, CLUSTYPE5, CLUSTYPE6, CLUSTYPE7, VALPHON)), 
                      method = c("center", "scale"))

# Transform/scale test data
test_scaled <- predict(preProc, data)
test_scaled$CLUSTYPE7 <- as.factor(test_scaled$CLUSTYPE7)

# Selected variables using lasso
select <- c("RESP", "REC", "FRE", "CC_CARD", "PSWEATERS", "PKNIT_TOPS", "PKNIT_DRES", "PJACKETS",
            "PCAS_PNTS", "PDRESSES", "PSUITS", "POUTERWEAR", "PJEWELRY", "AMSPEND", "CCSPEND", 
            "AXSPEND", "TMONSPEND", "OMONSPEND", "SMONSPEND", "PREVPD", "GMP", "PROMOS", 
            "DAYS", "CLASSES", "COUPONS", "NITEMS", "STORES", "WEB", "RESPONDED", "LTP12FRE", 
            "PERCRET", "CLUSTYPE7")
test_scaled <- subset(test_scaled, select = select)

# Load model - random forest model
load(file = "rf.Rdata")

# Make predictions
predictions <- predict(rf, newdata = test_scaled, type = "prob")

cutoff <- 0.21
pred <- ifelse(predictions[,2] > cutoff, 1, 0)
pred <- as.factor(pred)

# Add predicted RESP to original data
data$RESP_PRED <- pred

# Profit and accuracy
actual_RESP <- data$RESP
TP <- sum(pred == 1 & actual_RESP == 1)
FP <- sum(pred == 1 & actual_RESP == 0)
TN <- sum(pred == 0 & actual_RESP == 0)
FN <- sum(pred == 0 & actual_RESP == 1)
profit <- 170 * TP - 30 * (TP + FP)
print(paste("Profit:", profit))
accuracy <- (TP + TN) / (TP + TN + FP + FN)
print(paste("Accuracy:", accuracy))

write.csv(pred, "Predictions.csv")

