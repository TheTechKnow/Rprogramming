###############################################
# PART 1 - DATA Prep and Analysis
# Group 13
# Project: Wine Quality Analysis
###############################################

# Load library for data manipulation.
# dplyr gives cleaner syntax for selecting variables, binding rows,
# summarizing numbers, etc., making data preparation easier and more readable.
library(dplyr)

# Load the raw datasets.
# The original wine quality files use semicolons instead of commas,
# so sep = ";" is required for R to read the data correctly.
red <- read.csv("winequality-red.csv", sep = ";")
white <- read.csv("winequality-white.csv", sep = ";")

# Add a 'type' variable so we can identify red vs white wines after merging. This variable becomes important for group comparisons (t-tests, boxplots, etc.).
red$type <- "red"
white$type <- "white"

# Combine both datasets into one.
# rbind() stacks the two datasets vertically because they have identical columns.
wine <- rbind(red, white)

# Convert appropriate variables into factors.
# 'type' and 'quality' are categorical. Treating them as numeric would mislead
# statistical tests and produce incorrect models, so we explicitly convert them.
wine$type <- as.factor(wine$type)
wine$quality <- as.factor(wine$quality)

# Check for missing values in each column.
# If missing values existed, we would need to decide whether to remove or impute.
# This output shows the dataset is complete, which means no cleaning is needed.
colSums(is.na(wine))

# Save cleaned dataset for future use.
# Exporting this ensures every group member works with the exact same prepared data.
write.csv(wine, "wine_cleaned.csv", row.names = FALSE)


###############################################
# DESCRIPTIVE STATISTICS after prepping and cleaning
###############################################

# Define the continuous variables from the dataset. We will compute summary statistics for each one.
continuous_vars <- c("fixed.acidity", "volatile.acidity", "citric.acid",
                     "residual.sugar", "chlorides", "free.sulfur.dioxide",
                     "total.sulfur.dioxide", "density", "pH",
                     "sulphates", "alcohol")

# Mean of each variable.
# sapply() applies the mean function to every selected variable. The mean tells us the central tendency of each chemical property.
mean_values <- sapply(wine[, continuous_vars], mean)
mean_values

# Median of each variable. The median is a more robust measure when data are skewed (e.g., residual sugar).
median_values <- sapply(wine[, continuous_vars], median)
median_values

# Standard deviation.
# This measures how spread out each chemical variable is.  Higher SD = more variability in that chemical component.
sd_values <- sapply(wine[, continuous_vars], sd)
sd_values


###############################################
# MEANINGFUL Plots
###############################################

# Histogram of alcohol content.
# This helps us visualize the distribution (shape, skewness) of alcohol levels across all wine samples. Useful for understanding average strength.
hist(wine$alcohol,
     main = "Distribution of Alcohol Content",
     xlab = "Alcohol (%)",
     col = "lightblue",
     border = "black")

# Boxplot of alcohol by wine quality.
# This shows how alcohol varies with perceived quality. Boxplots allow comparison of medians and spread across multiple categories.
boxplot(alcohol ~ quality, data = wine,
        main = "Alcohol Content by Quality Rating",
        xlab = "Wine Quality",
        ylab = "Alcohol (%)",
        col = "lightgreen")

# Scatterplot: residual sugar vs alcohol.
# Purpose: Explore whether sweetness is related to alcohol content. This can reveal negative or positive trends.
plot(wine$residual.sugar, wine$alcohol,
     main = "Alcohol vs Residual Sugar",
     xlab = "Residual Sugar (g/L)",
     ylab = "Alcohol (%)",
     col = "purple",
     pch = 16)

# Barplot showing distribution of quality ratings. This visually confirms which quality levels are most common.
quality_counts <- table(wine$quality)
barplot(quality_counts,
        main = "Frequency of Wine Quality Ratings",
        xlab = "Quality Rating",
        ylab = "Count",
        col = "orange")

# Correlation heatmap of all continuous variables. This is one of the fastest ways to see which chemical components move together.
# Useful for understanding relationships before modeling.
library(corrplot)
cor_matrix <- cor(wine[, continuous_vars])
corrplot(cor_matrix, method = "color")

