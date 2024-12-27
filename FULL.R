############################################
# EDA Like a Pro: Airbnb NYC 2019 Dataset
# File: AB_NYC_2019.csv
############################################

# 1. Load Required Libraries ----
# Install packages if you don't have them:
# install.packages(c("dplyr", "ggplot2", "data.table", "skimr", "DT", "stringr"))

library(dplyr)
library(ggplot2)
library(data.table)  # for fread (faster reading)
library(skimr)       # for skim() EDA
library(DT)          # for datatable() display
library(stringr)

# 2. Import Data ----
# Adjust the file path to where your file is located.
# If you encounter encoding issues, specify 'encoding = "latin1"' or 'encoding = "ISO-8859-1"'.
#data_path <- "AB_NYC_2019.csv"

# Using fread (from data.table) for faster import, but read.csv is fine as well.
airlist <- fread(data_path, encoding = "latin1", na.strings = c("", "NA"))

# 3. Preliminary Overview ----

# Check dimensions
cat("Dataset dimensions:", dim(airlist), "\n")

# Display column names
cat("Column names:\n")
print(names(airlist))

# Show the first few rows in a scrollable table (for interactive R sessions or R Markdown)
datatable(head(airlist, 10), caption = "Preview of First 10 Rows")

# Check the structure (data types, number of observations, etc.)
str(airlist)

# Optional: Quick advanced summary using skim()
cat("\n--- Skim Summary ---\n")
skim(airlist)

# 4. Initial Data Cleaning ----

# 4.1 Identify columns with minimal or no data
# Sometimes the dataset has unnamed columns at the end with no data
zero_var_cols <- names(airlist)[apply(airlist, 2, function(x) all(is.na(x)))]
if (length(zero_var_cols) > 0) {
    cat("Dropping empty columns:", zero_var_cols, "\n")
    airlist <- airlist[, setdiff(names(airlist), zero_var_cols), with = FALSE]
}

# 4.2 Inspect Missing Values
cat("\n--- Missing Values per Column ---\n")
missing_counts <- sapply(airlist, function(x) sum(is.na(x)))
print(missing_counts)

# 5. Descriptive Statistics ----
# For numeric columns only:
numeric_cols <- names(airlist)[sapply(airlist, is.numeric)]
cat("\n--- Basic Descriptive Stats (numeric cols) ---\n")
print(summary(airlist[, ..numeric_cols]))

# 6. Handling Price Anomalies (if needed) ----
# Sometimes prices can be 0 or extremely high
# We'll remove entries where price <= 0 to avoid errors in log-scale plots
if ("price" %in% names(airlist)) {
    original_count <- nrow(airlist)
    airlist <- airlist %>%
        filter(price > 0)
    removed_count <- original_count - nrow(airlist)
    cat("Removed", removed_count, "rows with zero or negative price.\n")
}

# 7. Exploratory Visualizations ----

# 7.1 Distribution of Price
# Using a log scale can help visualize skewed data
if ("price" %in% names(airlist)) {
    ggplot(airlist, aes(x = price)) +
        geom_histogram(bins = 50, fill = "blue", color = "white") +
        scale_x_log10() +
        labs(
            title = "Distribution of Price (Log Scale)",
            x = "Price (log scale)",
            y = "Count"
        ) +
        theme_minimal()
}

# 7.2 Boxplot of Price by Room Type
# Check how prices vary by different room types
if (all(c("price", "room_type") %in% names(airlist))) {
    ggplot(airlist, aes(x = room_type, y = price)) +
        geom_boxplot(outlier.alpha = 0.4, fill = "orange") +
        coord_cartesian(ylim = c(0, 1000)) +  # focus on up to $1000
        labs(
            title = "Price Distribution by Room Type",
            x = "Room Type",
            y = "Price"
        ) +
        theme_minimal()
}

# 7.3 Price vs. Minimum Nights
if (all(c("price", "minimum_nights") %in% names(airlist))) {
    ggplot(airlist, aes(x = minimum_nights, y = price)) +
        geom_point(alpha = 0.3, color = "purple") +
        coord_cartesian(xlim = c(0, 100), ylim = c(0, 1000)) +
        labs(
            title = "Price vs. Minimum Nights (Clamped Scales)",
            x = "Minimum Nights",
            y = "Price"
        ) +
        theme_minimal()
}

# 7.4 Geographical Distribution of Listings
# We'll see if we have 'longitude', 'latitude', and 'neighbourhood_group'
if (all(c("longitude", "latitude", "neighbourhood_group") %in% names(airlist))) {
    ggplot(airlist, aes(x = longitude, y = latitude, color = neighbourhood_group)) +
        geom_point(alpha = 0.3) +
        labs(
            title = "Geographical Distribution of Listings",
            x = "Longitude",
            y = "Latitude",
            color = "Neighborhood Group"
        ) +
        theme_minimal()
}

# 8. Correlation Analysis (Optional) ----
# Quickly check correlation among numeric columns if relevant
if (length(numeric_cols) > 1) {
    corr_data <- airlist[, ..numeric_cols] %>% na.omit()
    corr_matrix <- cor(corr_data)
    
    # Heatmap of correlation
    library(reshape2)
    melted_corr <- melt(corr_matrix)
    ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
        labs(
            title = "Correlation Heatmap (Numeric Columns)",
            x = "",
            y = "",
            fill = "Correlation"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# 9. Key Observations / Findings (printed to console) ----

cat("\n--- Key Observations from EDA ---\n")
cat("* Missing Data:\n")
cat("  - Check if missing values in 'last_review' or 'reviews_per_month' are legitimate (hosts with no reviews)\n")
cat("* Price Distribution:\n")
cat("  - Highly skewed; majority of listings under $500, but extreme outliers exist.\n")
cat("* Room Type:\n")
cat("  - Entire home/apt typically has higher median prices than private/shared rooms.\n")
cat("* Minimum Nights:\n")
cat("  - Extreme values (some listings > 365). Could be special cases.\n")
cat("* Geographic Plots:\n")
cat("  - Manhattan and Brooklyn have the densest clusters. Higher prices often in Manhattan.\n")

cat("\n--- EDA Complete ---\n")

