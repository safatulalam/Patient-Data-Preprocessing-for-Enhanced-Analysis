install.packages("dplyr")
library(dplyr)
install.packages("matrixStats")
library(matrixStats)
library(dplyr)

#install"caret"
install.packages("caret")

dataset<- read.csv("c:/users/USER/Downloads/Dataset.csv",header=TRUE,sep=",")
dataset
names(dataset)

attributes <- names(dataset)
dataTypes <- c(typeof(dataset$ID),typeof(dataset$Age),typeof(dataset$`weight(kg)`),typeof(dataset$Delivery_number),typeof(dataset$Delivery_time),
               typeof(dataset$Blood),typeof(dataset$Heart),typeof(dataset$Caesarian))
data.frame(attributes, dataTypes)
str(dataset)

#missing_values
dataset$Blood[dataset$Blood %in% c("", "NA", "N/A", "null")] <- NA
dataset$Blood <- as.factor(dataset$Blood)
summary(dataset$Blood)
print(dataset)

colSums(is.na(dataset))

#Visualized all missing values
missing_counts <- colSums(is.na(dataset))

barplot(missing_counts, 
        main = "Missing Values by Variable", 
        xlab = "Variables", 
        ylab = "Count of Missing Values", 
        col = "lightgreen", 
        las = 2)

#Use median for Delete missing value of Age
missingAge <- which(is.na(dataset$Age))
fillingAge <- median(dataset$Age, na.rm = TRUE)
dataset$Age[missingAge] <- fillingAge
colSums(is.na(dataset))

#most frequent
mode_delivery <- names(which.max(table(dataset$Delivery_number)))
dataset$Delivery_number[is.na(dataset$Delivery_number)] <- mode_delivery
colSums(is.na(dataset))

#Mean
missingDelivery_time <- which(is.na(dataset$Delivery_time))
fillingDelivery_time <- mean(dataset$Delivery_time, na.rm = TRUE)
dataset$Delivery_time[missingDelivery_time] <- fillingDelivery_time
colSums(is.na(dataset))

#most frequent
mode_blood <- names(which.max(table(dataset$Blood, useNA = "no")))
dataset$Blood[is.na(dataset$Blood)] <- mode_blood
colSums(is.na(dataset))

#normalize
dataset$Age_normalized <- (dataset$Age - min(dataset$Age)) / (max(dataset$Age) - min(dataset$Age))
print(dataset)


#outlier_detect
dataset <- read.csv("c:/users/USER/Downloads/Dataset.csv",header=TRUE,sep=",")
detect_outliers_iqr <- function(x) {
  if (!is.numeric(x)) {
    stop("Input to detect_outliers_iqr must be numeric.")
  }
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  return(x < lower | x > upper)
}

# Numeric columns to check
num_cols <- c("ID", "Age", "Delivery_number", "Delivery_time", "Heart", "Caesarian")

# Loop through each column and report number of outliers
for (col in num_cols) {
  if (col %in% names(dataset)) {
    if (is.numeric(dataset[[col]])) {
      outliers <- detect_outliers_iqr(dataset[[col]])
      count <- sum(outliers, na.rm = TRUE)
      cat("Outliers in", col, ":", count, "\n")
    } else {
      cat("Skipping non-numeric column:", col, "\n")
    }
  } else {
    cat("Column not found in dataset:", col, "\n")
  }
}

#outlier_Show
detect_outliers_iqr <- function(x) {
  if (!is.numeric(x)) return(rep(FALSE, length(x))) # Return FALSE for non-numeric columns
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(x < lower_bound | x > upper_bound)
}

# Numeric columns to check
num_cols <- c("ID", "Age", "Delivery_number", "Delivery_time","Heart","Caesarian")

# Create a data frame of TRUE/FALSE for outliers in each column
outlier_flags <- sapply(dataset[num_cols], detect_outliers_iqr)

# Convert to data frame (if it's a matrix)
outlier_flags_df <- as.data.frame(outlier_flags)

# Count how many outliers per row
dataset$Outlier_Count <- rowSums(outlier_flags_df, na.rm = TRUE)

# Show rows with multiple (e.g. ≥2) outliers
multi_outlier_rows <- dataset[dataset$Outlier_Count >= 1, ]

# View those rows
print(multi_outlier_rows)

#Outlier_Handling
AgeOutlierHandling <- dataset
AgeOutlierHandling <- subset(AgeOutlierHandling, Age <= 50)
print(AgeOutlierHandling)

#convertion
dataset$Blood <- factor(dataset$Blood,levels = c("low","normal","high"),labels = c(0,1,2))
dataset$Blood
print(dataset)

#convertion
dataset$Caesarian <- factor(dataset$Caesarian,levels = c(0,1),labels = c("no","yes"))
dataset$Caesarian
print(dataset)

#convertion
dataset$Heart <- factor(dataset$Heart,levels = c(0,1),labels = c("apt","inept"))
dataset$Heart
print(dataset)

#invalid data
for (col in num_cols) {
  dataset[[col]][dataset[[col]] < 0 | dataset[[col]] > 50] <- NA
  dataset[[col]][is.na(dataset[[col]])] <- mean(dataset[[col]], na.rm = TRUE)
}
print(dataset)

#Check Class Imbalance
table(dataset$Caesarian)



#Oversampling
library(caret)

# Set seed for reproducibility
set.seed(123)

# Upsample the minority class
balanced_data <- upSample(
  x = dataset[, -which(names(dataset) == "Caesarian")],  # Features
  y = as.factor(dataset$Caesarian),                      # Target variable
  yname = "Caesarian"                                    # Name of the target column
)

# Check new distribution
table(balanced_data$Caesarian)

#Undersampling
library(caret)

set.seed(123)

# Downsample the majority class
balanced_data <- downSample(
  x = dataset[, -which(names(dataset) == "Caesarian")],
  y = as.factor(dataset$Caesarian),
  yname = "Caesarian"
)

table(balanced_data$Caesarian)


#SMOTE

# Function to calculate mode (since R doesn't have a built-in mode function)
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x))]
}

# Calculate central tendencies by Blood group
central_tendencies <- dataset %>%
  group_by(Blood) %>%
  summarise(
    count = n(),
    mean_age = mean(Age, na.rm = TRUE),
    median_age = median(Age, na.rm = TRUE),
    mode_age = get_mode(na.omit(Age))
  ) %>%
  arrange(Blood)

# Print results
print(central_tendencies)

#Split the dataset for Training and Testing
# Load required library
library(caret)

# Set seed for reproducibility
set.seed(123)

# Split data into 70% training and 30% testing
train_index <- createDataPartition(dataset$Caesarian, p = 0.7, list = FALSE)
train_data <- dataset[train_index, ]
test_data  <- dataset[-train_index, ]

# Check dimensions
cat("Training set:", nrow(train_data), "rows\n")
cat("Testing set:", nrow(test_data), "rows\n")

