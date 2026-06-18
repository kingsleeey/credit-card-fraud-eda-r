#Class Project - EDA
#by Kingsley Egei


#--------------- Step 1
# Project proposal attached to Report



#--------------- Step 2
# Loading data and idientifying data issues 
# Was going to go the API route but downloading to my computer was just faster
# To Download yourself, data set can be found at https://www.kaggle.com/datasets/neharoychoudhury/credit-card-fraud-data/code

TempFraud_data <- read.csv("/Users/kings/Documents/Data Science 2026/EDA and Visualization/fraud_data.csv", header = TRUE, stringsAsFactors = FALSE)
# examine variable types
head(TempFraud_data) #1st 6 rows and columns check
str(TempFraud_data) #structure and data types check
dim(TempFraud_data) #dimensions (rows x columns) check 


#---------------------------------------
#install possible packages I might need
#_______________________________________

install.packages(c("skimr", "naniar",
                  "visdat", "dplyr",
                  "ggplot2", "scales",
                  "lubridate"))
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales) #last Tidyverse library needed
library(janitor)
library(visdat)
library(naniar)
library(skimr)
vis_miss(TempFraud_data) #This is the most common use for this library. it creates 
#a vertical "map" of your data where the black lines represent missing (NA) values.
vis_dat(TempFraud_data)

cat("Visually there is no missing data. \n\n")
cat("========================================\n")
cat("STEP 2.1: THE RAW DATA OVERVIEW\n")
cat("========================================\n\n")
cat("Rows   :", nrow(TempFraud_data), "\n")
cat("Columns:", ncol(TempFraud_data), "\n\n")
cat("Column names:\n")
print(names(TempFraud_data))
cat("\nData types (raw):\n")
print(sapply(TempFraud_data, class))
print(table(sapply(TempFraud_data, class)))

#--------------- STEP 2.2: Summary 
cat("\n========================================\n")
cat("Using summary() in Base R\n")
cat("========================================\n\n")
print(summary(TempFraud_data))


#--------------- STEP 2.3: skimr::skim() —  For a more detailed summary
#========================================
cat("\n========================================\n")
cat("STEP 2.3: Call the skimr::skim() Function\n")
cat("========================================\n\n")
print(skim(TempFraud_data)) #I like this function 


#--------------- STEP 2.4: Check Missingness

# Use the naniar and visdat packages
#========================================
# The simple code would to just use this one below, but we will further dive in to outher wasys 
# for thesake of education 
colSums(is.na(TempFraud_data))

cat("\n========================================\n")
cat("STEP 2.4: Whats Missing? - THE MISSINGNESS CHECK\n")
cat("========================================\n\n")

# Count missing per column
cat("--- Missing values per column ---\n")
print(miss_var_summary(TempFraud_data))

# 2.4b: Overall missingness
cat(sprintf("\nOverall missing rate: %.2f%%\n",
            pct_miss(TempFraud_data)))
# great, I think in class you said to add dirty data into the dataset then try to clean it with imputation?
# I cannot recall so I just left the data source as is.


# 2.4c: Plots to visualize Missingness
# Not super necessary buta good practice to keep in mind for datasets with actual
# missing data.

# Plot 1: vis_miss — heatmap of missing values
vis_miss(TempFraud_data,
         warn_large_data = FALSE) +
  labs(title = "Missing Value Heatmap — fraud_data")

# Plot 2: gg_miss_var — bar chart of missingness per variable
gg_miss_var(TempFraud_data) +
  labs(title = "Missing Values by Variable")

# Plot 3: vis_dat — overview of data types
vis_dat(TempFraud_data, #Visdat we used early on as a clear indicator that there were no missing values
        warn_large_data = FALSE) +
  labs(title = "Data Type Overview — fraud_data")

cat("No missing values detected in this dataset. Yay!\n")
cat("All 14,446 rows are complete.\n\n")




#--------------- STEP 2.5: Identify ALL the data quality issues

#the issues identified that need fixing
# 1. Data Type Problems
# 2. High Cardinality Categorical Variables
# 3. Class Imbalance (Very Important)
# 4. Scale Differences in Numeric Variables
# 5. Geographic Data Complexity
# 6. Data Quality Issues
# 7. Redundant or Low-Value Columns
# 8. Time-Based Patterns Not Ready

cat("========================================\n")
cat("STEP 2.5: TIME TO AUDIT DATA QUALITY\n")
cat("========================================\n\n")

# --- Issue 1: Corrupted is_fraud values ---
cat("--- ISSUE 1: Corrupted is_fraud values ---\n")
cat("Expected values: '0' or '1' only\n")
print(table(TempFraud_data$is_fraud, useNA = "always"))
corrupted_rows <- TempFraud_data[
  !TempFraud_data$is_fraud %in% c("0", "1"), ]
cat(sprintf("\nCorrupted rows found: %d\n",
            nrow(corrupted_rows)))
print(corrupted_rows[, c("trans_date_trans_time",
                         "merchant",
                         "is_fraud")])

# Corrupted rows found: 2
# > print(corrupted_rows[, c("trans_date_trans_time",
#                            +                          "merchant",
#                            +                          "is_fraud")])
# trans_date_trans_time         merchant               is_fraud
# 1782      11-12-2020 23:19 Thompson-Gleason 1"2020-12-24 16:56:24"
# 7781      31-12-2020 23:59  Breitenberg LLC 0"2019-01-01 00:00:44"




# --- Issue 2: Date columns stored as strings ---
cat("\n--- ISSUE 2: Date columns stored as character ---\n")
cat("trans_date_trans_time class:",
    class(TempFraud_data$trans_date_trans_time), "\n")
cat("dob class               :",
    class(TempFraud_data$dob), "\n")
cat("Sample trans_date_trans_time:\n")
print(head(TempFraud_data$trans_date_trans_time, 5))
cat("Sample dob:\n")
print(head(TempFraud_data$dob, 5))
cat("Date format detected: DD-MM-YYYY HH:MM\n")

# Sample dob:
#   > print(head(TempFraud_data$dob, 5))
# [1] "09-11-1939" "09-11-1939" "09-11-1939" "09-11-1939" "09-11-1939"
# > cat("Date format detected: DD-MM-YYYY HH:MM\n")
# Date format detected: DD-MM-YYYY HH:MM


# --- Issue 3: is_fraud stored as character ---
cat("\n--- ISSUE 3: is_fraud stored as character ---\n")
cat("Current class:", class(TempFraud_data$is_fraud), "\n")
cat("Should be   : factor or integer (0/1)\n")

# Current class: character 
# > cat("Should be   : factor or integer (0/1)\n")
# Should be   : factor or integer (0/1)

# --- Issue 4: Extra quotes in merchant and job ---
cat("\n--- ISSUE 4: Extra quotes in merchant/job ---\n")
merchant_quoted <- sum(startsWith(TempFraud_data$merchant,
                                  '"'))
job_quoted      <- sum(grepl('"',
                             TempFraud_data$job))
cat(sprintf("Merchants with quotes: %d of %d (%.1f%%)\n",
            merchant_quoted,
            nrow(TempFraud_data),
            merchant_quoted/nrow(TempFraud_data)*100))
cat(sprintf("Jobs with quotes     : %d of %d (%.1f%%)\n",
            job_quoted,
            nrow(TempFraud_data),
            job_quoted/nrow(TempFraud_data)*100))
cat("Sample merchants with quotes:\n")
print(head(TempFraud_data$merchant[
  startsWith(TempFraud_data$merchant, '"')], 5))
cat("Sample jobs with quotes:\n")
print(head(TempFraud_data$job[
  grepl('"', TempFraud_data$job)], 5))

# Merchants with quotes: 4438 of 14446 (30.7%)
# > cat(sprintf("Jobs with quotes     : %d of %d (%.1f%%)\n",
#               +             job_quoted,
#               +             nrow(TempFraud_data),
#               +             job_quoted/nrow(TempFraud_data)*100))
# Jobs with quotes     : 3924 of 14446 (27.2%)
# > cat("Sample merchants with quotes:\n")
# Sample merchants with quotes:
#   > print(head(TempFraud_data$merchant[
#     +   startsWith(TempFraud_data$merchant, '"')], 5))
# [1] "\"Stokes, Christiansen and Sipes\""  "\"Raynor, Reinger and Hagenes\""    
# [3] "\"Gottlieb, Considine and Schultz\"" "\"Moen, Reinger and Murphy\""       
# [5] "\"Hauck, Dietrich and Funk\""       
# > cat("Sample jobs with quotes:\n")
# Sample jobs with quotes:
#   > print(head(TempFraud_data$job[
#     +   grepl('"', TempFraud_data$job)], 5))
# [1] "\"Administrator, education\"" "\"Administrator, education\"" "\"Administrator, education\""
# [4] "\"Administrator, education\"" "\"Administrator, education\""





# --- Issue 5: Duplicate rows ---
cat("\n--- ISSUE 5: Duplicate rows ---\n")
dup_count <- sum(duplicated(TempFraud_data))
cat(sprintf("Fully duplicate rows: %d\n", dup_count))
dup_trans  <- sum(duplicated(TempFraud_data$trans_num))
cat(sprintf("Duplicate trans_num : %d\n", dup_trans))
if (dup_count > 0) {
  cat("Sample duplicate rows:\n")
  dupes <- TempFraud_data[duplicated(TempFraud_data) |
                       duplicated(TempFraud_data,
                                  fromLast = TRUE), ]
  print(head(dupes[, c("trans_date_trans_time",
                       "merchant", "amt",
                       "trans_num",
                       "is_fraud")], 6))
}
# IDK about this one this one, the only duplicate I see off break is the IS fruad column. Have
# to dive deeper

#========================================
# Investigate Further Which Columns Are Duplicates
#========================================

cat("========================================\n")
cat("DUPLICATE FURTHER INVESTIGATION\n")
cat("========================================\n\n")

# Issue 5.1: Pull out ALL duplicate rows
# (both the original AND its copy)
dupes <- TempFraud_data[
  duplicated(TempFraud_data) |
    duplicated(TempFraud_data, fromLast = TRUE), ]

cat(sprintf("Total rows involved in duplicates: %d\n\n",
            nrow(dupes)))

# Issue 5.2: Print them side by side — ALL columns
cat("--- FULL DUPLICATE ROWS (all columns) ---\n")
print(dupes)

# Issue 5.3: Sort by trans_num so pairs sit
# next to each other for easy comparison
dupes_sorted <- dupes[order(dupes$trans_num), ]
cat("\n--- SORTED BY trans_num ---\n")
print(dupes_sorted)
#This code helps me clearly see that there are duplicates. Should We deal with these?


# Issue 5.4: Compare column by column
# to find WHERE they differ

cat("\n--- COLUMN-BY-COLUMN COMPARISON ---\n")
cat("Comparing each pair of duplicate rows\n\n")

# Get unique trans_nums that are duplicated
dup_trans_nums <- unique(
  TempFraud_data$trans_num[
    duplicated(TempFraud_data$trans_num)])

cat(sprintf("Duplicate trans_num values: %d\n\n",
            length(dup_trans_nums)))

# for each duplicate trans_num compare the two rows column by column
for (tn in head(dup_trans_nums, 5)) {
  
  pair <- TempFraud_data[
    TempFraud_data$trans_num == tn, ]
  
  cat(sprintf("trans_num: %s\n", tn))
  cat(strrep("-", 50), "\n")
  
  if (nrow(pair) >= 2) {
    for (col in names(TempFraud_data)) {
      val1 <- as.character(pair[[col]][1])
      val2 <- as.character(pair[[col]][2])
      
      # Flag columns where values DIFFER
      if (val1 != val2) {
        cat(sprintf("  DIFFERS  %-25s row1='%s'  row2='%s'\n",
                    col, val1, val2))
      } else {
        cat(sprintf("  same     %-25s '%s'\n",
                    col, val1))
      }
    }
  }
  cat("\n")
}

# Issue 5.5: Summary of which columns differ across ALL duplicate pairs

cat("--- WHICH COLUMNS DIFFER MOST OFTEN ---\n\n")

col_diff_count <- setNames(
  rep(0, ncol(TempFraud_data)),
  names(TempFraud_data))

for (tn in dup_trans_nums) {
  pair <- TempFraud_data[
    TempFraud_data$trans_num == tn, ]
  
  if (nrow(pair) >= 2) {
    for (col in names(TempFraud_data)) {
      val1 <- as.character(pair[[col]][1])
      val2 <- as.character(pair[[col]][2])
      if (!is.na(val1) && !is.na(val2) &&
          val1 != val2) {
        col_diff_count[col] <- col_diff_count[col] + 1
      }
    }
  }
}

# Print only columns that differ at least once
differing <- col_diff_count[col_diff_count > 0]
if (length(differing) == 0) {
  cat("All duplicate pairs are IDENTICAL across\n")
  cat("every column — true fully duplicate rows.\n")
} else {
  cat("Columns that differ between duplicate pairs:\n\n")
  cat(sprintf("%-25s  %s\n",
              "Column", "Times it differs"))
  cat(strrep("-", 42), "\n")
  for (col in names(sort(differing, decreasing=TRUE))) {
    cat(sprintf("%-25s  %d\n",
                col, differing[col]))
  }
  cat("\nThese columns make the rows look different\n")
  cat("but trans_num is the same — meaning the\n")
  cat("same transaction was recorded twice with\n")
  cat("slightly different values in those columns.\n")
}



# --- Issue 6: Class imbalance ---
cat("\n--- ISSUE 6: Class imbalance (is_fraud) ---\n")
valid_fraud <- TempFraud_data[
  TempFraud_data$is_fraud %in% c("0","1"), ]
fraud_table <- table(valid_fraud$is_fraud)
print(fraud_table)
cat(sprintf("\nFraud rate : %.2f%%\n",
            fraud_table["1"] /
              sum(fraud_table) * 100))
cat(sprintf("Ratio      : %.0f non-fraud per fraud case\n",
            fraud_table["0"] / fraud_table["1"]))
cat("WARNING: Severe class imbalance.\n")
cat("Models will be biased toward predicting non-fraud.\n")

# --- Issue 7: High cardinality ---
# Remember,  High cardinality means a categorical variable has a large number of unique values.
# AKA this is bad
cat("\n--- ISSUE 7: High cardinality variables ---\n")
for (col in c("merchant","job","trans_num",
              "city","state","category")) {
  n_unique <- length(unique(TempFraud_data[[col]]))
  cat(sprintf("  %-22s: %d unique values\n",
              col, n_unique))
}
#trans_num jumps out immediately
cat("trans_num is a unique ID — not useful for modelling.\n")
cat("NOTE: Merchant and Job are also high but may or may not effect modeling. TBD.\n") 

# --- Issue 8: Scale differences ---
cat("\n--- ISSUE 8: Scale differences ---\n")
for (col in c("amt","city_pop","lat","long",
              "merch_lat","merch_long")) {
  cat(sprintf("  %-12s: min=%-12.2f max=%-12.2f range=%.2f\n",
              col,
              min(TempFraud_data[[col]], na.rm=TRUE),
              max(TempFraud_data[[col]], na.rm=TRUE),
              max(TempFraud_data[[col]], na.rm=TRUE) -
                min(TempFraud_data[[col]], na.rm=TRUE)))
}
cat("Normalisation/scaling needed before modelling.\n")

# amt         : min=1.00         max=3261.47      range=3260.47
# city_pop    : min=46.00        max=2383912.00   range=2383866.00
# lat         : min=20.03        max=66.69        range=46.67
# long        : min=-165.67      max=-89.63       range=76.04
# merch_lat   : min=19.03        max=67.51        range=48.48
# merch_long  : min=-166.67      max=-88.65       range=78.02



# --- Issue 9: amt right skew ---
cat("\n--- ISSUE 9: amt is heavily right-skewed ---\n")
cat(sprintf("  Mean  : $%.2f\n",
            mean(TempFraud_data$amt)))
cat(sprintf("  Median: $%.2f\n",
            median(TempFraud_data$amt)))
cat(sprintf("  Max   : $%.2f\n",
            max(TempFraud_data$amt)))
cat(sprintf("  Skew proxy (mean-median): $%.2f\n",
            mean(TempFraud_data$amt) -
              median(TempFraud_data$amt)))
cat("Log transformation recommended for amt.\n")




#--------------- STEP 2.7: Create a DATA DICTIONARY 
#========================================
# Data dictionary can be seen in the Data Audit Report 



#--------------- STEP 3: CLEAN THE DATA

cat("\n========================================\n")
cat("STEP 3: DATA CLEANING STEP\n")
cat("========================================\n\n")

# Work on a copy, never modify raw data
fraud <- TempFraud_data

# --- Clean 1: Fix corrupted is_fraud rows ---
cat("--- Clean 1: Fix corrupted is_fraud ---\n")
cat("Extracting leading 0 or 1 from corrupted values:\n")

fraud$is_fraud <- substr(
  trimws(fraud$is_fraud), 1, 1)

cat("After fix — is_fraud unique values:\n")
print(table(fraud$is_fraud))

# Verification step -  only 0 and 1  should remain
stopifnot(all(fraud$is_fraud %in% c("0","1")))
cat("Verification passed: only '0' and '1' present.\n\n")

# --- Clean 3.2: Convert is_fraud to factor ---
cat("--- Clean 2: Convert is_fraud to factor ---\n")
fraud$is_fraud <- factor(fraud$is_fraud,
                         levels = c("0","1"),
                         labels = c("not_fraud",  #simple
                                    "fraud"))
cat("is_fraud class:", class(fraud$is_fraud), "\n")
cat("Levels:", levels(fraud$is_fraud), "\n")
print(table(fraud$is_fraud))
cat("\n")



# --- Clean 3.3:Fix the DATES: Parse trans_date_trans_time ---
cat("--- Clean 3: Parse trans_date_trans_time ---\n")
cat("Format detected: DD-MM-YYYY HH:MM\n")
fraud$trans_date_trans_time <- as.POSIXct(
  fraud$trans_date_trans_time,
  format = "%d-%m-%Y %H:%M",
  tz     = "UTC")
cat("Class after conversion:",
    class(fraud$trans_date_trans_time), "\n")
cat("Sample values:\n")
print(head(fraud$trans_date_trans_time, 5))
cat(sprintf("NAs introduced: %d\n\n",
            sum(is.na(fraud$trans_date_trans_time))))



# --- Clean 3.4: Parse dob ---
cat("--- Clean 4: Parse dob ---\n")
cat("Format detected: DD-MM-YYYY\n")

fraud$dob <- as.Date(
  fraud$dob,
  format = "%d-%m-%Y")

cat("Class after conversion:",
    class(fraud$dob), "\n")
cat("Sample values:\n")
print(head(fraud$dob, 5))
cat(sprintf("NAs introduced: %d\n\n",
            sum(is.na(fraud$dob))))


# --- Clean 3.5: Remove extra quotes from merchant ---
cat("--- Clean 5: Remove quotes from merchant ---\n")
#can do this with gsub() - gsub(pattern, replacement, x)
fraud$merchant <- gsub('"', '', fraud$merchant)
fraud$merchant <- trimws(fraud$merchant)
# trimws() removes invisible blank spaces, tabs, and newlines from 
# the beginning and/or end of a string — essential after string cleaning
# operations like gsub() which can leave behind unwanted whitespace at the edges.

quoted_after <- sum(grepl('"', fraud$merchant))
cat(sprintf("Merchants still with quotes: %d\n",
            quoted_after))
cat("Sample cleaned merchants:\n")
print(head(unique(fraud$merchant), 5))
cat("\n")

# --- Clean 3.6: Remove extra quotes from job ---
cat("--- Clean 6: Remove quotes from job ---\n")
fraud$job <- gsub('"', '', fraud$job)
fraud$job <- trimws(fraud$job)

quoted_job_after <- sum(grepl('"', fraud$job))
cat(sprintf("Jobs still with quotes: %d\n",
            quoted_job_after))
cat("Sample cleaned jobs:\n")
print(head(unique(fraud$job), 5))
cat("\n")

# --- Clean 3.7: Remove duplicate rows ---
cat("--- Clean 7: Remove duplicate rows ---\n")
cat(sprintf("Rows before dedup: %d\n", nrow(fraud)))
fraud <- fraud[!duplicated(fraud), ]
cat(sprintf("Rows after  dedup: %d\n", nrow(fraud)))
cat(sprintf("Duplicate rows removed: %d\n\n",
            14446 - nrow(fraud)))

# > cat(sprintf("Rows after  dedup: %d\n", nrow(fraud)))
# Rows after  dedup: 14383
# > cat(sprintf("Duplicate rows removed: %d\n\n",
#               +             14446 - nrow(fraud)))
# Duplicate rows removed: 63


# --- Clean 3.8: Derive time features ---
cat("--- Clean 8: Derive time features ---\n")
fraud$trans_hour  <- hour(fraud$trans_date_trans_time)
fraud$trans_day   <- wday(fraud$trans_date_trans_time,
                          label = TRUE)
fraud$trans_month <- month(fraud$trans_date_trans_time,
                           label = TRUE)
fraud$trans_year  <- year(fraud$trans_date_trans_time)

cat("New time columns added:\n")
cat("  trans_hour  : hour of transaction (0-23)\n")
cat("  trans_day   : day of week\n")
cat("  trans_month : month of year\n")
cat("  trans_year  : year\n\n")

cat("Sample:\n")
print(head(fraud[, c("trans_date_trans_time",
                     "trans_hour",
                     "trans_day",
                     "trans_month",
                     "trans_year")], 5))

# --- Clean 3.9: Derive age from dob ---
cat("\n--- Clean 9: Derive age from dob ---\n")
fraud$age <- as.integer(
  floor(as.numeric(
    difftime(as.Date("2020-12-31"),
             fraud$dob,
             units = "days")) / 365.25))

cat("Age summary:\n")
print(summary(fraud$age))
cat("\n")

# > print(summary(fraud$age))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 19.00   35.00   46.00   48.59   59.00   93.00 
# > cat("\n")

# --- Clean 3.10: Drop trans_num (unique ID) ---
cat("--- Clean 10: Drop trans_num (unique ID) ---\n")
fraud$trans_num <- NULL
cat("trans_num column removed.\n\n")

# --- Clean 3.11: Log transform amt ---
cat("--- Clean 11: Log transform amt ---\n")
fraud$log_amt <- log(fraud$amt)
cat("log_amt column added.\n")
cat(sprintf("log_amt range: %.4f to %.4f\n",
            min(fraud$log_amt),
            max(fraud$log_amt)))

# --- Clean 3.12: Compute distance between
#               customer and merchant ---
cat("\n--- Clean 12: Customer-merchant distance ---\n")

# Haversine formula for geographic distance
#had to look this one up
haversine <- function(lat1, lon1, lat2, lon2) {
  R     <- 6371                    # Earth radius km
  phi1  <- lat1 * pi / 180
  phi2  <- lat2 * pi / 180
  dphi  <- (lat2 - lat1) * pi / 180
  dlam  <- (lon2 - lon1) * pi / 180
  a     <- sin(dphi/2)^2 +
    cos(phi1) * cos(phi2) *
    sin(dlam/2)^2
  2 * R * asin(sqrt(a))
}

fraud$dist_km <- haversine(
  fraud$lat,      fraud$long,
  fraud$merch_lat, fraud$merch_long)

cat("dist_km column added (haversine distance).\n")
cat("Distance summary (km):\n")
print(summary(fraud$dist_km))
cat("\n")

# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.6393  55.4358  77.9497  75.8367  97.7347 143.5600 
# > cat("\n")


# STEP 3.12: Post-cleaning summary
#========================================
cat("========================================\n")
cat("STEP 3.12: POST-CLEANING SUMMARY\n")
cat("========================================\n\n")

cat("Final dataset dimensions:\n")
cat(sprintf("  Rows   : %d\n", nrow(fraud)))
cat(sprintf("  Columns: %d\n\n", ncol(fraud)))

cat("Updated data types:\n")
print(sapply(fraud, class))

cat("\nFinal column list:\n")
print(names(fraud))

cat("\nFinal skim:\n")
print(skim(fraud))

#========================================
# Visualize where we are now
#========================================
cat("\n========================================\n")
cat("EDA VISUALIZATIONS\n")
cat("========================================\n\n")

par(mfrow = c(2, 3), mar = c(5, 5, 4, 2))

# Plot 1: Class imbalance bar
fraud_counts <- table(fraud$is_fraud)
barplot(fraud_counts,
        main  = "Class Distribution\n(is_fraud)",
        col   = c("steelblue", "coral"),
        ylab  = "Count",
        names.arg = c("Not Fraud", "Fraud"))
text(x      = c(0.7, 1.9),
     y      = fraud_counts + 200,
     labels = paste0(fraud_counts, "\n(",
                     round(fraud_counts /
                             sum(fraud_counts)*100,1),
                     "%)"),
     cex    = 0.8, font = 2)

# Plot 2: amt distribution (raw)
hist(fraud$amt,
     breaks = 50,
     main   = "Transaction Amount\n(Raw)",
     xlab   = "Amount ($)",
     col    = rgb(0.2, 0.4, 0.8, 0.6),
     border = "white")

# Plot 3: log_amt distribution
hist(fraud$log_amt,
     breaks = 30,
     main   = "Transaction Amount\n(log transformed)",
     xlab   = "log(Amount)",
     col    = rgb(0.9, 0.3, 0.2, 0.6),
     border = "white")

# Plot 4: Transaction hour
fraud_by_hour <- table(fraud$trans_hour)
barplot(fraud_by_hour,
        main  = "Transactions by Hour",
        xlab  = "Hour of Day",
        ylab  = "Count",
        col   = "mediumpurple",
        border= "white")

# Plot 5: age distribution
hist(fraud$age,
     breaks = 20,
     main   = "Customer Age Distribution",
     xlab   = "Age (years)",
     col    = rgb(0.2, 0.7, 0.3, 0.6),
     border = "white")

# Plot 6: dist_km distribution
hist(fraud$dist_km,
     breaks = 30,
     main   = "Customer-Merchant Distance\n(km)",
     xlab   = "Distance (km)",
     col    = rgb(0.8, 0.5, 0.1, 0.6),
     border = "white")

par(mfrow = c(1, 1))

# ggplot2: amt by fraud status
ggplot(fraud, aes(x    = is_fraud,
                  y    = log_amt,
                  fill = is_fraud)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.1,
               fill  = "white",
               alpha = 0.6) +
  scale_fill_manual(values = c("steelblue",
                               "coral")) +
  labs(title    = "Log Transaction Amount by Fraud Status",
       x        = "Fraud Status",
       y        = "log(Amount)",
       fill     = "Status") +
  theme_minimal()

# ggplot2: fraud by category
fraud_cat <- fraud %>%
  group_by(category) %>%
  summarise(
    total      = n(),
    fraud_n    = sum(is_fraud == "fraud"),
    fraud_rate = round(fraud_n/total*100, 2)) %>%
  arrange(desc(fraud_rate))

ggplot(fraud_cat,
       aes(x    = reorder(category, fraud_rate),
           y    = fraud_rate,
           fill = fraud_rate)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Fraud Rate by Transaction Category (%)",
       x     = NULL,
       y     = "Fraud Rate (%)",
       fill  = "Rate") +
  theme_minimal()

# ggplot2: fraud by hour
fraud_hour <- fraud %>%
  group_by(trans_hour, is_fraud) %>%
  summarise(n = n(), .groups = "drop")

ggplot(fraud_hour,
       aes(x    = trans_hour,
           y    = n,
           fill = is_fraud)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("steelblue",
                               "coral"),
                    labels  = c("Not Fraud",
                                "Fraud")) +
  labs(title = "Transactions by Hour of Day",
       x     = "Hour",
       y     = "Count",
       fill  = "Status") +
  theme_minimal()

#  FINAL DATA QUALITY REPORT
#========================================
cat("========================================\n")
cat("FINAL DATA QUALITY REPORT\n")
cat("========================================\n\n")

issues <- data.frame(
  Issue = c(
    "Corrupted is_fraud values",
    "is_fraud wrong data type",
    "trans_date_trans_time wrong type",
    "dob wrong data type",
    "Extra quotes in merchant",
    "Extra quotes in job",
    "Duplicate rows",
    "Class imbalance",
    "amt right skew",
    "Scale differences",
    "High cardinality (merchant/job)",
    "trans_num (useless ID column)"),
  Status = c(
    "FIXED — extracted leading digit",
    "FIXED — converted to factor",
    "FIXED — converted to POSIXct",
    "FIXED — converted to Date",
    "FIXED — quotes removed with gsub()",
    "FIXED — quotes removed with gsub()",
    "FIXED — 63 duplicates removed",
    "NOTED — 12.8% fraud rate",
    "ADDRESSED — log_amt column added",
    "ADDRESSED — dist_km + age derived",
    "NOTED — use with caution in models",
    "REMOVED — dropped from dataset"),
  stringsAsFactors = FALSE)

cat(sprintf("%-40s  %s\n", "Issue", "Status"))
cat(strrep("-", 75), "\n")
for (i in 1:nrow(issues)) {
  cat(sprintf("%-40s  %s\n",
              issues$Issue[i],
              issues$Status[i]))
}

cat(sprintf("\n\nFinal clean dataset: %d rows × %d cols\n",
            nrow(fraud), ncol(fraud)))
cat("Ready for analysis and modelling.\n\n")

cat("New columns derived during cleaning:\n")
new_cols <- c("trans_hour", "trans_day",
              "trans_month", "trans_year",
              "age", "log_amt", "dist_km")
for (col in new_cols) {
  cat(sprintf("  %-15s: %s\n", col,
              class(fraud[[col]])))
}




#outliers may flag for fraud so I won't be removing them for this data set
#but below I will Identify them

#========================================
# OUTLIER IDENTIFICATION ONLY
# No data is modified or removed!
#========================================
#create a character vector that contains the names of the numeric columns in dataset.
numeric_vars <- c("amt", "city_pop", "lat", "long","merch_lat", "merch_long", "dist_km", "age")

#IQR

cat("========================================\n")
cat("OUTLIER IDENTIFICATION REPORT\n")
cat("IQR Method: outside Q1-1.5*IQR or Q3+1.5*IQR\n")
cat("========================================\n\n")

cat(sprintf("%-14s  %8s  %8s  %8s  %10s  %10s  %10s\n",
            "Variable", "Q1", "Q3","IQR", "Lower fence", "Upper fence", "N outliers"))
cat(strrep("-", 78), "\n") #line space

#For statement to generate values
for (v in numeric_vars) {
  vals  <- fraud[[v]]
  vals  <- vals[!is.na(vals)]
  Q1    <- quantile(vals, 0.25)
  Q3    <- quantile(vals, 0.75)
  IQR_v <- IQR(vals)
  lower <- Q1 - 1.5 * IQR_v
  upper <- Q3 + 1.5 * IQR_v
  n_out <- sum(vals < lower | vals > upper)
  pct   <- round(n_out / length(vals) * 100, 2)
  
  cat(sprintf("%-14s  %8.2f  %8.2f  %8.2f  %10.2f  %10.2f  %6d (%.1f%%)\n",
              v, Q1, Q3, IQR_v, lower, upper, n_out, pct))
}


#--------------------------------------------------
# Z score comments for myself and can be disregarded 
# A Z-score tells you how far a value is from the mean, measured in standard deviations.

# Z-scores work best when the variable is roughly normally distributed.
# 
# Transaction amounts in fraud datasets are often right-skewed 
# (many small transactions, a few very large ones), so a boxplot or 
# IQR method may sometimes be better for outlier detection than Z-scores alone.
# 
# For your EDA, Z-scores are most useful for:
# -Standardizing variables before modeling.
# -Flagging potentially suspicious transactions.
# -Comparing values across variables with different units.
#--------------------------------------------------

cat("\n========================================\n")
cat("Z-SCORE METHOD: |Z| > 3\n")
cat("========================================\n\n")
#Z score forumula : z = (x(observation) - mean)/ SD

cat(sprintf("%-14s  %10s  %10s  %10s\n",
            "Variable", "Mean", "SD", "N outliers"))
cat(strrep("-", 60), "\n") # 60 lines sems about right 

#for statement to assign values to vector
for (v in numeric_vars) {
  vals  <- fraud[[v]]
  vals  <- vals[!is.na(vals)]
  z     <- abs((vals - mean(vals)) / sd(vals))
  n_out <- sum(z > 3)
  pct   <- round(n_out / length(vals) * 100, 2)
  
  cat(sprintf("%-14s  %10.2f  %10.2f  %6d (%.1f%%)\n",
              v,
              mean(vals),
              sd(vals),
              n_out, pct))
}


# #after doing some research credti card transaction data, I learned about this need for the 
# Impossible Value check. This comment can be ignored and is for personal Reference.
#
# The impossible values check is needed because some errors are logically 
# impossible rather than just statistically unusual 
# — a negative age, a $0 transaction, or a latitude of 500 will pass 
# through outlier detection undetected but will silently corrupt your means, 
# break your transformations, and bias your models if not caught.

cat("\n========================================\n")
cat("IMPOSSIBLE VALUE CHECK\n")
cat("========================================\n\n")

cat(sprintf("amt <= 0         : %d\n",   #cant be neagtive 0
            sum(fraud$amt <= 0,
                na.rm = TRUE)))
cat(sprintf("age < 16         : %d\n",   #under 16 shouldn't be using CC
            sum(fraud$age < 16,
                na.rm = TRUE)))
cat(sprintf("age > 100        : %d\n",   #not too many people live over 100
            sum(fraud$age > 100,
                na.rm = TRUE)))
cat(sprintf("lat out of range : %d\n",   # I think -90 and 90 should be sufficient lats
            sum(fraud$lat < -90 |
                  fraud$lat > 90,
                na.rm = TRUE)))
cat(sprintf("long out of range: %d\n",    # betwen -180 and 180 long should be fine 
            sum(fraud$long < -180 |
                  fraud$long > 180,
                na.rm = TRUE)))
cat("\n Dataset looks good with no impossible values detected. Moving on \n")#


#boxplot check

cat("\n========================================\n")
cat("BOXPLOTS — Visual identification only\n")
cat("========================================\n\n")

par(mfrow = c(2, 4), mar = c(5, 5, 4, 1))

for (v in numeric_vars) {
  vals  <- fraud[[v]]
  Q1    <- quantile(vals, 0.25, na.rm=TRUE)
  Q3    <- quantile(vals, 0.75, na.rm=TRUE)
  IQR_v <- IQR(vals, na.rm=TRUE)
  n_out <- sum(vals < Q1 - 1.5*IQR_v | vals > Q3 + 1.5*IQR_v,  #for outliers
               na.rm = TRUE)
  
  # for cleaner plots, I can change the names from variables to the full name for the report
  # also maybe export as 2 rows to 2 columns so that the plots can be more easily read
  boxplot(vals,
          main    = v,
          ylab    = v,
          col     = rgb(0.2, 0.4, 0.8, 0.5),
          border  = "black",
          outline = TRUE)          # dots = outliers
  
  #cool little code 
  colors()
  mtext(paste0(n_out, " outliers"),
        side = 3, line = 0.2,
        cex  = 0.75,
        col  = ifelse(n_out > 0,
                      "red3",
                      "darkgreen"))
}

par(mfrow = c(1, 1)) #reset plot 

cat("Dataset rows unchanged:", nrow(fraud), "\n")
cat("No data was modified or removed.\n")



#--------------- Step 4          
#— Univariate Exploration Section

# Remember, Univariate Exploration is analyzing one variable at a time to 
# understand its characteristics before looking at relationships between variables.
# 
# In EDA, it's usually the first step because it helps you identify data quality issues,
# distributions, outliers, and unusual values.

#========================================
# STEP 4: UNIVARIATE EXPLORATION
# fraud_data.csv — The cleaned dataset (Fraud)
#========================================
# Make sure these libraries are ldoed, if not. remove comment tag and load 
# library(ggplot2)
# library(dplyr)
# library(scales)
# library(lubridate)

#========================================
# Step 4.1 - Ensure the dataset is clean before even starting
#========================================
cat("========================================\n")
cat("DATASET STATE CHECK\n")
cat("========================================\n\n")
#cleaned data set is "fraud" data set
cat(sprintf("Rows   : %d\n", nrow(fraud)))
cat(sprintf("Columns: %d\n\n", ncol(fraud)))

# Quality-control check that ensures all engineered features exist before I start
# analysis, visualization, or modeling.
# Confirm key derived columns exist
required_cols <- c("log_amt", "age", "dist_km", "trans_hour", "trans_day", "trans_month")
missing_cols  <- required_cols[
  !required_cols %in% names(fraud)]

if (length(missing_cols) > 0) {
  cat("Re-deriving missing columns:\n")
  
  if (!"log_amt" %in% names(fraud)) {
    fraud$log_amt <- log(fraud$amt)
    cat("  log_amt derived\n")
  }
  if (!"age" %in% names(fraud)) {
    fraud$age <- as.integer(floor(
      as.numeric(difftime(
        as.Date("2020-12-31"),
        fraud$dob,
        units = "days")) / 365.25))
    cat("  age derived\n")
  }
  if (!"dist_km" %in% names(fraud)) {
    haversine <- function(lat1,lon1,lat2,lon2) {
      R    <- 6371
      phi1 <- lat1 * pi/180
      phi2 <- lat2 * pi/180
      dp   <- (lat2-lat1) * pi/180
      dl   <- (lon2-lon1) * pi/180
      a    <- sin(dp/2)^2 +
        cos(phi1)*cos(phi2)*sin(dl/2)^2
      2 * R * asin(sqrt(a))
    }
    fraud$dist_km <- haversine(
      fraud$lat, fraud$long,
      fraud$merch_lat, fraud$merch_long)
    cat("  dist_km derived\n")
  }
  if (!"trans_hour" %in% names(fraud)) {
    fraud$trans_hour <- hour(
      fraud$trans_date_trans_time)
    fraud$trans_day  <- wday(
      fraud$trans_date_trans_time, label=TRUE)
    fraud$trans_month<- month(
      fraud$trans_date_trans_time, label=TRUE)
    cat("  time features derived\n")
  }
}


# Sumary Statistics 
#========================================
cat("\n========================================\n")
cat("SUMMARY STATISTICS — KEY VARIABLES\n")
cat("========================================\n\n")

#creating numeric_vars once agian just in case
numeric_vars <- c("amt", "log_amt", "city_pop", "age","dist_km", "trans_hour")

cat(sprintf("%-12s  %8s  %8s  %8s  %8s  %8s  %8s  %8s\n",
            "Variable", "n", "Mean", "Median", "SD", "Min", "Max", "Skew"))
cat(strrep("-", 80), "\n") # 10 lines per variable 

for (v in numeric_vars) {
  if (v %in% names(fraud)) {
    vals  <- fraud[[v]][!is.na(fraud[[v]])]
    skew  <- round(mean(vals) - median(vals), 3)
    cat(sprintf("%-12s  %8d  %8.2f  %8.2f  %8.2f  %8.2f  %8.2f  %8.3f\n",
                v, length(vals), mean(vals), median(vals), sd(vals), min(vals), max(vals), skew))
  }
}


#ok lets begin, time for visuals 
#be sure to use GGplot

#---- Visual 1: is_fraud — Class Distribution?
#========================================
cat("\n========================================\n")
cat("VISUAL 1: is_fraud — Class Distribution\n")
cat("========================================\n\n")

fraud_counts <- fraud %>%
  count(is_fraud) %>%
  mutate(
    pct   = round(n / sum(n) * 100, 1),
    label = paste0(n, "\n(", pct, "%)"))

# colors()
p1 <- ggplot(fraud_counts,
             aes(x    = is_fraud,
                 y    = n,
                 fill = is_fraud)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = label),
            vjust = -0.4,
            size  = 4,
            fontface = "bold") +
  scale_fill_manual(
    values = c("not_fraud" = "cadetblue1",
               "fraud"     = "salmon1")) +
  scale_y_continuous(
    labels   = label_comma(),
    expand   = expansion(mult = c(0, 0.15))) +
  labs(title    = "Class Distribution — is_fraud",
       subtitle = "Fraud represents 12.4% of all transactions",
       x        = "Fraud Status",
       y        = "Number of Transactions",
       fill     = "Status") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title      = element_text(
          face  = "bold",
          size  = 14))
print(p1)

#how do we intepret this info
cat("INTERPRETATION:\n")
cat(sprintf("Not fraud: %d (%.1f%%)\n",
            fraud_counts$n[
              fraud_counts$is_fraud=="not_fraud"],
            fraud_counts$pct[
              fraud_counts$is_fraud=="not_fraud"]))
cat(sprintf("Fraud    : %d (%.1f%%)\n",
            fraud_counts$n[
              fraud_counts$is_fraud=="fraud"],
            fraud_counts$pct[
              fraud_counts$is_fraud=="fraud"]))
cat("The dataset is SEVERELY IMBALANCED — only 12.4%\n")
cat("of transactions are fraudulent. This means:\n")
cat("  - A naive model predicting 'not fraud' always\n")
cat("    would achieve 87.6% accuracy — which is misleading.\n")
cat("  - Precision and recall are better metrics.\n")
cat("  - Class balancing (SMOTE/undersampling) may\n")
cat("    be needed before modelling.\n\n")

cat(" For some more context, SMOTE and undersampling are \n")
cat(" techniques used to deal with class imbalance, \n")
cat(" which is very common in fraud detection datasets. \n\n")

# Apply SMOTE only to the training data, not the test data. 
# Otherwise you'll get overly optimistic results because synthetic fraud
# examples leak information into evaluation. If fraud cases are less than 
# about 5–10% of all transactions, SMOTE is often worth considering. SMOTE + Undersampling 
# is a Common industry approach. Undersampling reduces the size of the majority class.
# It provides a balanced data set and faster model training. BUT it throws away 
# lots of legitiate transaction data and you may lose important patterns. 



#---- VISUAL 2: amt — Raw Distribution
#========================================
cat("========================================\n")
cat("VISUAL 2: amt — Transaction Amount\n")
cat("========================================\n\n")

# colors()
p2 <- ggplot(fraud,
             aes(x = amt)) +
  geom_histogram(bins   = 60,
                 fill   = "darkblue",
                 color  = "white",
                 alpha  = 0.8) +
  geom_vline(xintercept = mean(fraud$amt),
             color      = "red",
             lwd        = 1,
             lty        = 2) +
  geom_vline(xintercept = median(fraud$amt),
             color      = "darkorange",
             lwd        = 1,
             lty        = 3) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_comma()) +
  annotate("text",
           x     = mean(fraud$amt) + 250,
           y     = 3000,
           label = paste0("Mean $",
                          round(mean(fraud$amt), 0)),
           color = "red",
           size  = 3.5) +
  annotate("text",
           x     = median(fraud$amt) - 80,
           y     = 3000,
           label = paste0("Median $",
                          round(median(fraud$amt), 0)),
           color = "darkorange",
           size  = 3.5,
           hjust = 1) +
  labs(title    = "Distribution of Transaction Amount (Raw)",
       subtitle = "Heavily right-skewed — log transformation recommended",
       x        = "Transaction Amount ($)",
       y        = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold",
                                  size=14))
print(p2)

#Intpretation section
cat("INTERPRETATION:\n")
cat(sprintf("Mean  : $%.2f\n", mean(fraud$amt)))
cat(sprintf("Median: $%.2f\n", median(fraud$amt)))
cat(sprintf("Max   : $%.2f\n", max(fraud$amt)))
cat(sprintf("SD    : $%.2f\n", sd(fraud$amt)))
cat("Looking at the plot, the distribution is STRONGLY RIGHT SKEWED.\n")
cat("The mean ($122.72) is more than double the\n")
cat("median ($51.29) — indicating a long right tail\n")
cat("driven by a small number of very large transactions.\n")
cat("75% of transactions are under $100.14.\n")
cat("The max of $3,261.47 is far from the bulk of data.\n\n")


#---- Visual 3: log_amt — Log-Transformed Amount
#========================================
cat("========================================\n")
cat("VISUAL 3: log_amt — Log-Transformed Amount\n")
cat("========================================\n\n")
# Self notes:
# A variable like log_amt is often created because the original variable amt is highly right-skewed. 
# When you plot the raw amounts, those very large values will stretch the x-axis.
# Most observations get compressed into the left side of the histogram,
# making it hard to see the true distribution. 
# Heavily right-skewed — > log transformation is recommended
# The goal isn't to change the data arbitrarily but to make the distribution easier 
# to visualize and analyze while reducing the dominance of a few very large transactions.


p3 <- ggplot(fraud,
             aes(x = log_amt)) +
  geom_histogram(bins  = 40,
                 fill  = "plum3",
                 color = "white",
                 alpha = 0.8) +
  geom_density(aes(y = after_stat(count) *
                     (max(fraud$log_amt) -
                        min(fraud$log_amt)) / 40),
               color = "black",
               lwd   = 1) +
  geom_vline(xintercept = mean(fraud$log_amt),
             color = "red",   lwd=1, lty=2) +
  geom_vline(xintercept = median(fraud$log_amt),
             color = "darkgreen", lwd=1, lty=3) +
  scale_x_continuous(
    breaks = log(c(1, 5, 10, 50,
                   100, 500, 1000, 3000)),
    labels = paste0("$", c(1,5,10,50,
                           100,500,1000,3000))) +
  labs(title    = "Distribution of Transaction Amount (Log Scale)",
       subtitle = "Log transformation produces a near-normal distribution",
       x        = "Transaction Amount (log scale)",
       y        = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold",
                                  size=14))
print(p3)

# comments
cat("INTERPRETATION:\n")
cat("After log transformation, the distribution becomes\n")
cat("APPROXIMATELY NORMAL and bimodal — two peaks suggest\n")
cat("two distinct clusters of transaction sizes:\n")
cat("  Peak 1: small everyday purchases ($5-$20)\n")
cat("  Peak 2: larger purchases ($50-$200)\n")
cat("Log transformation reduces skewness from strongly\n")
cat("right-skewed to near-symmetric, making the data\n")
cat("suitable for regression and distance-based models.\n\n")


#---- Visual 4: category — Transaction Types
#========================================
cat("========================================\n")
cat("VISUAL 4: category — Transaction Categories\n")
cat("========================================\n\n")

cat_counts <- fraud %>%
  count(category) %>%
  arrange(desc(n)) %>%
  mutate(
    pct      = round(n/sum(n)*100, 1),
    category = factor(category, levels = category))

p4 <- ggplot(cat_counts,
             aes(x    = reorder(category, n),
                 y    = n,
                 fill = n)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(n,
                               " (", pct, "%)")),
            hjust = -0.1,
            size  = 3) +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma",
                       direction = -1) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.18)),
    labels = label_comma()) +
  labs(title    = "Transaction Count by Category",
       subtitle = "14 spending categories — grocery and gas dominate",
       x        = NULL,
       y        = "Number of Transactions") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title      = element_text(
          face="bold", size=14))
print(p4)

cat("INTERPRETATION:\n")
cat("The top 5 categories by volume:\n")
#use a for statement for top 5
for (i in 1:5) {
  cat(sprintf("  %d. %-20s %d (%.1f%%)\n",
              i, as.character(cat_counts$category[i]), cat_counts$n[i], cat_counts$pct[i]))
}
cat("It is clear that grocery_pos and gas_transport are the most common\n")
cat("transaction types — representing everyday spending.\n")
cat("travel has the fewest transactions but may have\n")
cat("higher average amounts and fraud rates.\n\n")


#---- Visual 5: age — Customer Age Distribution
#========================================
cat("========================================\n")
cat("VISUAL 5: age — Customer Age\n")
cat("========================================\n\n")

p5 <- ggplot(fraud,
             aes(x = age)) +
  geom_histogram(bins  = 35,
                 fill  = "darkorange",
                 color = "white",
                 alpha = 0.8) +
  geom_vline(xintercept = mean(fraud$age,
                               na.rm=TRUE),
             color = "red", lwd=1, lty=2) +
  geom_vline(xintercept = median(fraud$age,
                                 na.rm=TRUE),
             color = "darkblue", lwd=1, lty=3) +
  annotate("text",
           x     = mean(fraud$age,
                        na.rm=TRUE) + 2,
           y     = 1200,
           label = paste0("Mean ", round(mean(fraud$age,
                                     na.rm=TRUE),
                                1)),
           color = "red", size = 3.5, hjust=0) +
  annotate("text",
           x     = median(fraud$age,
                          na.rm=TRUE) - 2,
           y     = 1200,
           label = paste0("Median ", median(fraud$age,
                                 na.rm=TRUE)),
           color = "darkblue",
           size  = 3.5, hjust=1) +
  labs(title    = "Distribution of Customer Age",
       subtitle = "Age derived from date of birth (dob)",
       x        = "Age (years)",
       y        = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold",
                                  size=14))
print(p5)

#intepretation comments
cat("INTERPRETATION:\n")
cat(sprintf("Mean age  : %.1f years\n",
            mean(fraud$age, na.rm=TRUE)))
cat(sprintf("Median age: %.0f years\n",
            median(fraud$age, na.rm=TRUE)))
cat(sprintf("Range     : %d to %d years\n",
            min(fraud$age, na.rm=TRUE),
            max(fraud$age, na.rm=TRUE)))
cat("The age distribution is approximately UNIFORM\n")
cat("to slightly right-skewed — customers span a wide\n")
cat("range of ages without a dominant age group.\n")
cat("Historical data accross industires has shown us that \n")
cat("older customers are more susceptible to Fraud and \n")
cat("may represent different fraud risk.\n\n")


#---- Visual 6: trans_hour — "Hour" of Day
#========================================
cat("========================================\n")
cat("VISUAL 6: trans_hour — Hour of Day\n")
cat("========================================\n\n")

hour_counts <- fraud %>%
  count(trans_hour) %>%
  mutate(period = case_when(
    trans_hour >= 6  & trans_hour < 12 ~ "Morning",
    trans_hour >= 12 & trans_hour < 18 ~ "Afternoon",
    trans_hour >= 18 & trans_hour < 22 ~ "Evening",
    TRUE ~ "Night"))

p6 <- ggplot(hour_counts,
             aes(x    = trans_hour,
                 y    = n,
                 fill = period)) +
  geom_col(alpha = 0.85) +
  scale_fill_manual(
    values = c("Morning"   = "gold",
               "Afternoon" = "darkorange",
               "Evening"   = "cornflowerblue",
               "Night"     = "darkblue")) +
  scale_x_continuous(
    breaks = seq(0, 23, by=2),
    labels = paste0(seq(0,23,by=2), ":00")) +
  scale_y_continuous(labels = label_comma()) +
  labs(title    = "Transaction Volume by Hour of Day",
       subtitle = "Color coded by time period",
       x        = "Hour of Day",
       y        = "Number of Transactions",
       fill     = "Period") +
  theme_minimal() +
  theme(plot.title    = element_text(
    face="bold", size=14),
    axis.text.x   = element_text(
      angle=45, hjust=1))
print(p6)

peak_hour <- hour_counts$trans_hour[
  which.max(hour_counts$n)]
low_hour  <- hour_counts$trans_hour[
  which.min(hour_counts$n)]

#interpetation comments 
cat("INTERPRETATION:\n")
cat(sprintf("Peak transaction hour: %d:00\n",
            peak_hour))
cat(sprintf("Lowest transaction hour: %d:00\n",
            low_hour))
cat("Transactions peak during daytime hours and drop\n")
cat("sharply overnight. This is expected for legitimate\n")
cat("transactions. However, fraud transactions may show\n")
cat("a different hourly pattern (As you can see, there is more activity at night).\n")
cat("This will be explored in the bivariate analysis section.\n\n")


#---- Visual 7: city_pop — City Population
#========================================
cat("========================================\n")
cat("VISUAL 7: city_pop — City Population\n")
cat("========================================\n\n")

colors()
p7 <- ggplot(fraud,
             aes(x = log10(city_pop))) +
  geom_histogram(bins  = 35,
                 fill  = "turquoise2",
                 color = "white",
                 alpha = 0.85) +
  geom_vline(xintercept = log10(
    mean(fraud$city_pop)),
    color = "red", lwd=1, lty=2) +
  geom_vline(xintercept = log10(
    median(fraud$city_pop)),
    color = "darkgreen", lwd=1, lty=3) +
  annotate("text",
           x = log10(mean(fraud$city_pop)),
           y = Inf,             # top of plot
           label    = paste0( "Mean\n", format(round(mean(fraud$city_pop)), big.mark=",")),
           color    = "red",
           size     = 3.5,
           fontface = "bold",
           hjust    = -0.1,            # nudge right
           vjust    = 1.5) +           # nudge down from top
  
  annotate("text",
           x = log10(median(fraud$city_pop)),
           y = Inf,
           label    = paste0("Median\n", format(round(median(fraud$city_pop)), big.mark=",")),
           color    = "darkgreen",
           size     = 3.5,
           fontface = "bold",
           hjust    = -0.1,             # nudge left
           vjust    = 1.5) +           # nudge down from top
  scale_x_continuous(
    breaks = c(2, 3, 4, 5, 6),
    labels = c("100", "1K", "10K",
               "100K", "1M")) +
  labs(title    = "Distribution of City Population (Log Scale)",
       subtitle = "Log scale needed — cities range from 46 to 2.4 million",
       x        = "City Population (log scale)",
       y        = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold",
                                  size=14))


print(p7)

#interpret the plot
cat("INTERPRETATION:\n")
cat(sprintf("Median city population: %s\n",
            format(median(fraud$city_pop), big.mark=",")))
cat(sprintf("Mean   city population: %s\n",
            format(round(mean(fraud$city_pop)), big.mark=",")))
cat(sprintf("Range: %s to %s\n",
            format(min(fraud$city_pop), big.mark=","),
            format(max(fraud$city_pop), big.mark=",")))
cat("City population spans 5 orders of magnitude\n")
cat("requiring log scale for meaningful visualisation.\n")
cat("Most transactions occur in small-to-medium towns\n")
cat("(median ~1,645 residents) rather than major cities.\n")
cat("The log-scale distribution shows a roughly normal\n")
cat("shape which would suggest a balanced urban-rural mix.\n\n")
#had to go back and add annotate 




#---- Visual 8: state — Geographic Distribution
#========================================
cat("========================================\n")
cat("VISUAL 8: state — State Distribution\n")
cat("========================================\n\n")

state_counts <- fraud %>%
  count(state) %>%
  arrange(desc(n)) %>%
  mutate(pct = round(n/sum(n)*100, 1))

p8 <- ggplot(state_counts,
             aes(x    = reorder(state, n),
                 y    = n,
                 fill = n)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(n,
                               "\n(", pct, "%)")),
            hjust = -0.1,
            size  = 2.8) +
  coord_flip() +
  scale_fill_viridis_c(option    = "cividis",
                       direction = -1) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.20)),
    labels = label_comma()) +
  labs(title    = "Transaction Count by State",
       subtitle = "13 states represented — California dominates",
       x        = NULL,
       y        = "Number of Transactions") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title      = element_text(
          face="bold", size=14))
print(p8)

#interpreation comments. Continue to keep it short and to the point.
cat("INTERPRETATION:\n")
cat("Top 5 states by transaction volume:\n")
for (i in 1:5) {
  cat(sprintf("  %d. %-5s %d (%.1f%%)\n",
              i,
              state_counts$state[i],
              state_counts$n[i],
              state_counts$pct[i]))
}
cat("California has more than double any other state.\n")
cat("Only 13 states are represented meaning this dataset is\n")
cat("NOT a national sample but focused on the western\n")
cat("United States. Findings may not generalise broadly\n")
cat("and would be biased if assumed as National findngs.\n\n")


#---- Visual 9: dist_km — Customer-Merchant Distance
#========================================
cat("========================================\n")
cat("VISUAL 9: dist_km — Transaction Distance\n")
cat("========================================\n\n")

colors()
p9 <- ggplot(fraud,
             aes(x = dist_km)) +
  geom_histogram(bins  = 50,
                 fill  = "wheat2",
                 color = "white",
                 alpha = 0.8) +
  geom_vline(xintercept = mean(fraud$dist_km),
             color = "red", lwd=1, lty=2) +
  geom_vline(xintercept = median(fraud$dist_km),
             color = "darkgreen", lwd=1, lty=3) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  annotate("text",
           x     = mean(fraud$dist_km) - 30,
           y     = 3500,
           label = paste0("Mean ", round(mean(fraud$dist_km), 1), " km"),
           color = "red", size=3.5, hjust=0) +
  annotate("text",
           x     = median(fraud$dist_km) + 30,
           y     = 3500,
           label = paste0("Median ", round(median(fraud$dist_km), 1), " km"),
           color = "darkgreen",
           size=3.5, hjust=1) +
  labs(title    = "Distribution of Customer-Merchant Distance",
       subtitle = "Haversine distance in km between customer and merchant",
       x        = "Distance (km)",
       y        = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold",
                                  size=14))
print(p9)

#comments
cat("INTERPRETATION:\n")
cat(sprintf("Mean distance  : %.2f km\n",
            mean(fraud$dist_km)))
cat(sprintf("Median distance: %.2f km\n",
            median(fraud$dist_km)))
cat(sprintf("Max distance   : %.2f km\n",
            max(fraud$dist_km)))
cat("Observing the shape, Customer–merchant distances \n")
cat("are approximately symmetric with a SLIGHT NEGATIVE LEFT SKEW\n")
cat("Most transactions occur very close to home,\n")
cat("as indicated by the mean distance (75.8 km) being slightly \n")
cat("lower than the median distance (77.9 km). No substantial skewness is evident.\n")
cat("However transactions hundreds of km away may signal card use \n")
cat("while travelling or fraudulent remote charges.\n\n")


#---- Visual 10: trans_month — Seasonal Pattern
#========================================
cat("========================================\n")
cat("VISUAL 10: trans_month — Monthly Volume\n")
cat("========================================\n\n")

month_counts <- fraud %>%
  count(trans_month) %>%
  mutate(
    trans_month = factor(trans_month, levels = month.abb),
    pct         = round(n/sum(n)*100, 1))

p10 <- ggplot(month_counts,
              aes(x    = trans_month,
                  y    = n,
                  fill = n)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(pct, "%")),
            vjust = -0.4,
            size  = 3) +
  scale_fill_viridis_c(option = "magma",
                       direction = -1) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0, 0.12))) +
  labs(title    = "Transaction Volume by Month",
       subtitle = "Seasonal patterns — Jan 2019 to Dec 2020",
       x        = "Month",
       y        = "Number of Transactions") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title      = element_text(
          face="bold", size=14))
print(p10)

peak_month <- as.character( #peak months
  month_counts$trans_month[
    which.max(month_counts$n)])
low_month  <- as.character( # low season / low months
  month_counts$trans_month[
    which.min(month_counts$n)])

#interpretation comments
cat("INTERPRETATION:\n")
cat(sprintf("Highest volume month: %s (%d txns)\n",
            peak_month,
            max(month_counts$n)))
cat(sprintf("Lowest  volume month: %s (%d txns)\n",
            low_month,
            min(month_counts$n)))
cat("Transaction volume shows a mild seasonal pattern.\n")
cat("Higher volumes in mid-year months may reflect\n")
cat("summer spending increases.\n")
cat("Note: dataset spans Jan 2019 to Dec 2020 so\n")
cat("some months have 2 years of data aggregated.\n\n")

#saw online that its a good practice to add overall summary tables
#---- OVERALL / FINAL UNIVARIATE SUMMARY TABLE
#========================================
cat("========================================\n")
cat("UNIVARIATE EDA SUMMARY\n")
cat("========================================\n\n")

summary_tbl <- data.frame(
  Visual   = paste0("V", 1:10),
  Variable = c("is_fraud", "amt (raw)",
               "amt (log)", "category",
               "age", "trans_hour",
               "city_pop", "state",
               "dist_km", "trans_month"),
  Type     = c("Categorical","Numeric",
               "Numeric","Categorical",
               "Numeric","Numeric",
               "Numeric","Categorical",
               "Numeric","Categorical"),
  Key_Finding = c(
    "12.4% fraud — severe imbalance",
    "Right skewed — mean $122 median $51",
    "Log transform gives near-normal shape",
    "Grocery/gas dominate (11% each)",
    "Uniform spread — no dominant age group",
    "Peak daytime — low overnight",
    "Log-normal — mostly small towns",
    "13 western US states — CA dominates",
    "Slightly left skewed — most transactions under 5km",
    "Mild seasonality evident — peaks mid-year"),
  stringsAsFactors = FALSE)

cat(sprintf("%-6s  %-16s  %-12s  %s\n",
            "Visual", "Variable",
            "Type", "Key Finding"))
cat(strrep("-", 72), "\n")
for (i in 1:nrow(summary_tbl)) {
  cat(sprintf("%-6s  %-16s  %-12s  %s\n",
              summary_tbl$Visual[i],
              summary_tbl$Variable[i],
              summary_tbl$Type[i],
              summary_tbl$Key_Finding[i]))
}

cat("\n--- NEXT STEP is the Bivariate analysis---\n")
cat("Step 5: Bivariate analysis — explore how\n")
cat("each variable relates to is_fraud.\n")
cat("Key questions to answer:\n")
cat("  - Do fraudulent transactions have higher amt?\n")
cat("  - Are certain categories more fraud-prone?\n")
cat("  - Do fraud transactions happen at night?\n")
cat("  - Are fraudulent transactions further away?\n")


#--------------- Step 5
# Bivariate & Multivariate 
# Bivariate Exploration?
#   Bivariate exploration is the process of examining the relationship between two variables at a time.
# After you've understood each variable individually (univariate analysis), the next question is:
# "How does one variable relate to another?"


#========================================
# STEP 5: BIVARIATE & MULTIVARIATE ANALYSIS
# fraud — cleaned dataset
#========================================

#these should already be loaded, if not load libraries 
# library(ggplot2)
# library(dplyr)
# library(scales)
library(reshape2)   #for correlation heatmap, ppreattu sure I havent loaded yet.

#---- Visual 1: amt by is_fraud
# Violin + Boxplot
#========================================
cat("========================================\n")
cat("VISUAL 1: Transaction Amount by Fraud Status\n")
cat("========================================\n\n")

# Summary stats by group
amt_by_fraud <- fraud %>%
  group_by(is_fraud) %>%
  summarise(
    n      = n(),
    mean   = round(mean(amt), 2),
    median = round(median(amt), 2),
    sd     = round(sd(amt), 2),
    q75    = round(quantile(amt, 0.75), 2),
    max    = round(max(amt), 2),
    .groups = "drop")

cat("Summary of amt by fraud status:\n")
print(amt_by_fraud)

p1 <- ggplot(fraud,
             aes(x    = is_fraud,
                 y    = log_amt,
                 fill = is_fraud)) +
  geom_violin(trim  = FALSE,
              alpha = 0.7) +
  geom_boxplot(width = 0.12,
               fill  = "white",
               alpha = 0.8,
               outlier.size = 0.5) +
  stat_summary(fun  = mean,
               geom = "point",
               pch  = 18,
               size = 3,
               col  = "black") +
  scale_fill_manual(
    values = c("not_fraud" = "steelblue",
               "fraud"     = "coral")) +
  scale_y_continuous(
    breaks = log(c(1,5,10,50,
                   100,500,1000)),
    labels = paste0("$",
                    c(1,5,10,50,
                      100,500,1000))) +
  annotate("text",
           x     = 1, y = 7.0,
           label = paste0("Mean: $",
                          amt_by_fraud$mean[
                            amt_by_fraud$is_fraud ==
                              "not_fraud"]),
           size  = 3.5, color = "steelblue",
           fontface = "bold") +
  annotate("text",
           x     = 2, y = 8,
           label = paste0("Mean: $",
                          amt_by_fraud$mean[
                            amt_by_fraud$is_fraud ==
                              "fraud"]),
           size  = 3.5, color = "coral",
           fontface = "bold") +
  labs(title    = "Transaction Amount by Fraud Status",
       subtitle = "Log scale — fraud transactions are dramatically larger",
       x        = "Fraud Status",
       y        = "Transaction Amount (log scale)",
       fill     = "Status") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title      = element_text(
          face="bold", size=14))
print(p1)

cat("\nINTERPRETATION:\n")
cat(sprintf("Not fraud mean: $%.2f | Median: $%.2f\n",
            amt_by_fraud$mean[
              amt_by_fraud$is_fraud=="not_fraud"],
            amt_by_fraud$median[
              amt_by_fraud$is_fraud=="not_fraud"]))
cat(sprintf("Fraud     mean: $%.2f | Median: $%.2f\n",
            amt_by_fraud$mean[
              amt_by_fraud$is_fraud=="fraud"],
            amt_by_fraud$median[
              amt_by_fraud$is_fraud=="fraud"]))
cat(sprintf("Fraud transactions have %.1fx higher mean amount.\n",
            amt_by_fraud$mean[
              amt_by_fraud$is_fraud=="fraud"] /
              amt_by_fraud$mean[
                amt_by_fraud$is_fraud=="not_fraud"]))
cat("The violin plots show a dramatic difference:\n")
cat("  - Non-fraud: concentrated at low amounts ($10-$100)\n")
cat("  - Fraud    : shifted heavily toward $100-$1000+\n")
cat("Transaction amount is a STRONG predictor of fraud.\n")
cat("This is the single most powerful signal in the data.\n\n")


#---- Visual 2: Fraud Rate by Category
# Bar chart ranked by fraud rate
#========================================
cat("========================================\n")
cat("VISUAL 2: Fraud Rate by Transaction Category\n")
cat("========================================\n\n")

cat_fraud <- fraud %>%
  group_by(category) %>%
  summarise(
    n          = n(),
    n_fraud    = sum(is_fraud == "fraud"),
    fraud_rate = round(n_fraud / n * 100, 2),
    .groups    = "drop") %>%
  arrange(desc(fraud_rate))

cat("Fraud rate by category:\n")
print(cat_fraud)

# Add overall average line
avg_fraud_rate <- round(
  mean(fraud$is_fraud == "fraud") * 100, 2)

p2 <- ggplot(cat_fraud,
             aes(x    = reorder(category,
                                fraud_rate),
                 y    = fraud_rate,
                 fill = fraud_rate)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = avg_fraud_rate,
             lty   = 2,
             color = "red",
             lwd   = 0.8) +
  geom_text(aes(label = paste0(fraud_rate, "%")),
            hjust = -0.15,
            size  = 3.2) +
  annotate("text",
           x     = 1,
           y     = avg_fraud_rate + 0.8,
           label = paste0("Avg: ",
                          avg_fraud_rate, "%"),
           color = "red",
           size  = 3.2,
           hjust = 0) +
  coord_flip() +
  scale_fill_viridis_c(option    = "plasma",
                       direction = -1) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    labels = function(x) paste0(x, "%")) +
  labs(title    = "Fraud Rate by Transaction Category",
       subtitle = paste0("Red dashed line = overall average (",
                         avg_fraud_rate, "%)"),
       x        = NULL,
       y        = "Fraud Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title      = element_text(
          face="bold", size=14))
print(p2)

cat("\nINTERPRETATION:\n")
cat("HIGH RISK categories (above average):\n")
high_risk <- cat_fraud[
  cat_fraud$fraud_rate > avg_fraud_rate, ]
for (i in 1:nrow(high_risk)) {
  cat(sprintf("  %-20s %.1f%% (%.1fx average)\n",
              high_risk$category[i],
              high_risk$fraud_rate[i],
              high_risk$fraud_rate[i] /
                avg_fraud_rate))
}
cat("\nLOW RISK categories (below average):\n")
low_risk <- cat_fraud[
  cat_fraud$fraud_rate <= avg_fraud_rate, ]
for (i in 1:nrow(low_risk)) {
  cat(sprintf("  %-20s %.1f%%\n",
              low_risk$category[i],
              low_risk$fraud_rate[i]))
}
cat("\nshopping_net and grocery_pos have fraud rates\n")
cat("more than DOUBLE the dataset average, suggesting\n")
cat("these categories are systematically targeted.\n")
cat("home and health_fitness have the lowest rates.\n\n")


#---- Visual 3: Fraud Rate by Hour of Day
#========================================
cat("========================================\n")
cat("VISUAL 3: Fraud Rate by Hour of Day\n")
cat("========================================\n\n")

hour_fraud <- fraud %>%
  group_by(trans_hour) %>%
  summarise(
    n          = n(),
    n_fraud    = sum(is_fraud == "fraud"),
    fraud_rate = round(n_fraud / n * 100, 2),
    .groups    = "drop") %>%
  mutate(period = case_when(
    trans_hour >= 22 | trans_hour <= 3 ~ "High risk (night)",
    trans_hour >= 4  & trans_hour < 22 ~ "Lower risk (day)"))

cat("Fraud rate by hour:\n")
print(hour_fraud[, c("trans_hour",
                     "n", "n_fraud",
                     "fraud_rate")])

p3 <- ggplot(hour_fraud,
             aes(x    = trans_hour,
                 y    = fraud_rate,
                 fill = period)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = avg_fraud_rate,
             lty = 2, color = "black",
             lwd = 0.8) +
  scale_fill_manual(
    values = c("High risk (night)" = "coral",
               "Lower risk (day)"  = "steelblue")) +
  scale_x_continuous(
    breaks = 0:23,
    labels = paste0(0:23, ":00")) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%")) +
  annotate("text",
           x = 12, y = avg_fraud_rate + 1.5,
           label = paste0("Overall avg: ",
                          avg_fraud_rate, "%"),
           size = 3.2, color = "black") +
  labs(title    = "Fraud Rate by Hour of Day",
       subtitle = "Fraud spikes dramatically between 22:00 and 03:00",
       x        = "Hour of Day",
       y        = "Fraud Rate (%)",
       fill     = "Risk Period") +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle=45, hjust=1,
    size=8),
    plot.title  = element_text(
      face="bold", size=14))
print(p3)

cat("\nINTERPRETATION:\n")
night_avg <- mean(
  hour_fraud$fraud_rate[
    hour_fraud$period == "High risk (night)"])
day_avg   <- mean(
  hour_fraud$fraud_rate[
    hour_fraud$period == "Lower risk (day)"])

cat(sprintf("Night hours (22:00-03:00) avg fraud rate: %.1f%%\n",
            night_avg))
cat(sprintf("Day hours   (04:00-21:00) avg fraud rate: %.1f%%\n",
            day_avg))
cat(sprintf("Night fraud rate is %.1fx higher than day.\n",
            night_avg / day_avg))
cat("This is the STRONGEST temporal signal in the data.\n")
cat("Hours 22 and 23 have ~41% fraud rates — meaning\n")
cat("4 in 10 late-night transactions are fraudulent.\n")
cat("This suggests fraudsters deliberately act at night\n")
cat("when cardholders are less likely to notice activity.\n\n")


#---- Visual 4: Correlation Heatmap
#========================================
cat("========================================\n")
cat("VISUAL 4: Correlation Heatmap\n")
cat("========================================\n\n")

# Create binary fraud column for correlation
fraud$fraud_binary <- as.numeric(
  fraud$is_fraud == "fraud")

num_vars <- c("fraud_binary", "amt",
              "log_amt", "age",
              "city_pop", "dist_km",
              "trans_hour")

cor_mat  <- round(
  cor(fraud[, num_vars],
      use = "complete.obs"), 3)

# Melt for ggplot
cor_melt <- melt(cor_mat)
names(cor_melt) <- c("Var1", "Var2",
                     "Correlation")

p4 <- ggplot(cor_melt,
             aes(x    = Var1,
                 y    = Var2,
                 fill = Correlation)) +
  geom_tile(color = "white",
            lwd   = 0.5) +
  geom_text(aes(label = sprintf("%.2f",
                                Correlation)),
            size     = 3.2,
            fontface = "bold") +
  scale_fill_gradient2(
    low      = "#185FA5",
    mid      = "white",
    high     = "#993C1D",
    midpoint = 0,
    limits   = c(-1, 1),
    name     = "r") +
  scale_x_discrete(
    labels = c("fraud_binary" = "Fraud",
               "amt"          = "Amount",
               "log_amt"      = "log(Amount)",
               "age"          = "Age",
               "city_pop"     = "City Pop",
               "dist_km"      = "Distance",
               "trans_hour"   = "Hour")) +
  scale_y_discrete(
    labels = c("fraud_binary" = "Fraud",
               "amt"          = "Amount",
               "log_amt"      = "log(Amount)",
               "age"          = "Age",
               "city_pop"     = "City Pop",
               "dist_km"      = "Distance",
               "trans_hour"   = "Hour")) +
  labs(title    = "Correlation Heatmap — Key Variables",
       subtitle = "Red = positive, Blue = negative correlation",
       x        = NULL,
       y        = NULL) +
  theme_minimal() +
  theme(axis.text.x  = element_text(
    angle=30, hjust=1),
    plot.title   = element_text(
      face="bold", size=14),
    panel.grid   = element_blank())
print(p4)

cat("INTERPRETATION:\n")
cat("Correlations with fraud_binary:\n")
fraud_cors <- cor_mat["fraud_binary", ]
fraud_cors <- sort(fraud_cors[
  names(fraud_cors) != "fraud_binary"],
  decreasing = TRUE)
for (v in names(fraud_cors)) {
  r   <- fraud_cors[v]
  str <- ifelse(abs(r) >= 0.3, "STRONG",
                ifelse(abs(r) >= 0.1, "MODERATE",
                       "WEAK"))
  cat(sprintf("  %-15s r = %6.3f  %s\n",
              v, r, str))
}
cat("\namt and log_amt are the only meaningful\n")
cat("correlates of fraud — confirming transaction\n")
cat("amount is the primary fraud signal.\n")
cat("dist_km shows virtually zero correlation —\n")
cat("distance alone does not predict fraud.\n\n")

# clean up helper column..
fraud$fraud_binary <- NULL


#---- Visual 5: Age vs Amount Colored by Fraud
# Scatterplot
#========================================
cat("========================================\n")
cat("VISUAL 5: Age vs log(Amount) by Fraud\n")
cat("========================================\n\n")

# Sample for readability
set.seed(42)
fraud_sample <- fraud %>%
  group_by(is_fraud) %>%
  slice_sample(n = 500) %>%
  ungroup()

p5 <- ggplot(fraud_sample,
             aes(x     = age,
                 y     = log_amt,
                 color = is_fraud,
                 alpha = is_fraud)) +
  geom_point(size = 1.2) +
  scale_color_manual(
    values = c("not_fraud" = "steelblue",
               "fraud"     = "coral"),
    labels = c("Not fraud", "Fraud")) +
  scale_alpha_manual(
    values = c("not_fraud" = 0.3,
               "fraud"     = 0.8)) +
  scale_y_continuous(
    breaks = log(c(1,10,50,
                   100,500,1000)),
    labels = paste0("$",
                    c(1,10,50,
                      100,500,1000))) +
  geom_smooth(aes(group = is_fraud),
              method = "lm",
              se     = FALSE,
              lwd    = 1.2) +
  guides(alpha = "none") +
  labs(title    = "Age vs Transaction Amount by Fraud Status",
       subtitle = "Sample of 500 per group — fraud clustered at high amounts",
       x        = "Customer Age (years)",
       y        = "Transaction Amount (log scale)",
       color    = "Status") +
  theme_minimal() +
  theme(plot.title = element_text(
    face="bold", size=14))
print(p5)

cat("INTERPRETATION:\n")
cat("The scatterplot reveals two clear patterns:\n\n")
cat("1. VERTICAL SEPARATION: Fraud (coral) points\n")
cat("   cluster at the top of the y-axis — confirming\n")
cat("   high amounts are strongly associated with fraud.\n\n")
cat("2. AGE EFFECT: Both trend lines show a slight\n")
cat("   positive slope — older customers tend toward\n")
cat("   slightly higher transaction amounts.\n")
cat("   The fraud trend line sits consistently above\n")
cat("   the non-fraud line across all age groups.\n\n")
cat("3. NO AGE THRESHOLD: Fraud is spread across all\n")
cat("   ages — no specific age group is uniquely\n")
cat("   targeted. Amount is the key discriminator.\n\n")

#---- Visual6: Fraud Rate by State
# Horizontal bar chart
#========================================
cat("========================================\n")
cat("VISUAL 6: Fraud Rate by State\n")
cat("========================================\n\n")

state_fraud <- fraud %>%
  group_by(state) %>%
  summarise(
    n          = n(),
    n_fraud    = sum(is_fraud == "fraud"),
    fraud_rate = round(n_fraud / n * 100, 2),
    .groups    = "drop") %>%
  arrange(desc(fraud_rate))

cat("Fraud rate by state:\n")
print(state_fraud)

p6 <- ggplot(state_fraud,
             aes(x    = reorder(state,
                                fraud_rate),
                 y    = fraud_rate,
                 fill = fraud_rate)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(fraud_rate, "%\n(n=", n,")")),
            hjust = -0.1,
            size  = 3) +
  geom_hline(yintercept = avg_fraud_rate,
             lty = 2, color = "red",
             lwd = 0.8) +
  coord_flip() +
  scale_fill_gradient(
    low  = "steelblue",
    high = "coral") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.25)),
    labels = function(x) paste0(x, "%")) +
  annotate("text",
           x     = 1,
           y     = avg_fraud_rate + 0.5,
           label = paste0("Avg: ",
                          avg_fraud_rate, "%"),
           color = "red",
           size  = 3.2, hjust=0) +
  labs(title    = "Fraud Rate by State",
       subtitle = "Alaska (AK) has the highest fraud rate at 31.7%",
       x        = NULL,
       y        = "Fraud Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title      = element_text(
          face="bold", size=14))
print(p6)

cat("\nINTERPRETATION:\n")
cat(sprintf("Highest fraud state: %s (%.1f%%)\n",
            state_fraud$state[1],
            state_fraud$fraud_rate[1]))
cat(sprintf("Lowest  fraud state: %s (%.1f%%)\n",
            state_fraud$state[nrow(state_fraud)],
            state_fraud$fraud_rate[nrow(state_fraud)]))
cat("\nAlaska stands out with 31.7% — more than double\n")
cat("the dataset average of 12.4%. This could reflect:\n")
cat("  - Geographic isolation making fraud harder to detect\n")
cat("  - Smaller sample size (n=158) inflating the rate\n")
cat("  - Genuine regional fraud patterns\n")
cat("Oregon (16.3%) and Nebraska (15.0%) are also\n")
cat("notably above average.\n\n")


#---- Visual 7: Amount Distribution by Category
# Faceted violin plots
#========================================
cat("========================================\n")
cat("VISUAL 7: Amount by Category — Fraud vs Not\n")
cat("========================================\n\n")

# Order categories by fraud rate
cat_order <- cat_fraud$category

p7 <- ggplot(fraud,
             aes(x    = is_fraud,
                 y    = log_amt,
                 fill = is_fraud)) +
  geom_violin(trim  = TRUE,
              alpha = 0.7,
              scale = "width") +
  geom_boxplot(width   = 0.15,
               fill    = "white",
               alpha   = 0.7,
               outlier.size = 0.3) +
  scale_fill_manual(
    values = c("not_fraud" = "steelblue",
               "fraud"     = "coral")) +
  scale_y_continuous(
    breaks = log(c(1,10,100,1000)),
    labels = paste0("$",
                    c(1,10,100,1000))) +
  facet_wrap(~ factor(category,
                      levels=cat_order),
             ncol = 4) +
  labs(title    = "Transaction Amount by Category and Fraud Status",
       subtitle = "Ordered by fraud rate (highest top-left)",
       x        = NULL,
       y        = "Amount (log scale)",
       fill     = "Status") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(
          size=7, angle=30,
          hjust=1),
        strip.text      = element_text(
          size=8, face="bold"),
        plot.title      = element_text(
          face="bold", size=14))
print(p7)

cat("INTERPRETATION:\n")
cat("The faceted violins reveal a consistent pattern:\n")
cat("In EVERY category, fraud transactions (coral)\n")
cat("show a higher and wider distribution than\n")
cat("non-fraud (blue) — confirming that amount\n")
cat("is a universal fraud signal across all categories.\n\n")
cat("Categories where the gap is largest:\n")
cat("  - shopping_net: huge separation\n")
cat("  - grocery_pos : clear upward shift in fraud\n")
cat("  - misc_net    : fraud values tightly clustered high\n\n")


#---- Visual 8: Fraud Rate by Hour AND Category
# Heatmap
#========================================
cat("========================================\n")
cat("VISUAL 8: Fraud Rate Heatmap — Hour x Category\n")
cat("========================================\n\n")

hour_cat <- fraud %>%
  group_by(trans_hour, category) %>%
  summarise(
    n          = n(),
    fraud_rate = round(
      sum(is_fraud=="fraud") / n() * 100, 1),
    .groups    = "drop")

p8 <- ggplot(hour_cat,
             aes(x    = trans_hour,
                 y    = factor(
                   category,
                   levels = rev(cat_order)),
                 fill = fraud_rate)) +
  geom_tile(color = "white",
            lwd   = 0.3) +
  scale_fill_gradient2(
    low      = "steelblue",
    mid      = "lightyellow",
    high     = "coral",
    midpoint = avg_fraud_rate,
    name     = "Fraud\nrate (%)") +
  scale_x_continuous(
    breaks = seq(0, 23, by=2),
    labels = paste0(seq(0,23,by=2),
                    ":00")) +
  labs(title    = "Fraud Rate Heatmap — Hour of Day vs Category",
       subtitle = "Red = high fraud rate | Blue = low fraud rate",
       x        = "Hour of Day",
       y        = NULL) +
  theme_minimal() +
  theme(axis.text.x  = element_text(
    angle=45, hjust=1,
    size=8),
    axis.text.y  = element_text(size=9),
    plot.title   = element_text(
      face="bold", size=14),
    panel.grid   = element_blank())
print(p8)

cat("INTERPRETATION:\n")
cat("The heatmap reveals a powerful INTERACTION:\n\n")
cat("1. NIGHT EFFECT is universal — hours 22-03\n")
cat("   show red (high fraud) across most categories.\n\n")
cat("2. CATEGORY EFFECT stacks on top — shopping_net\n")
cat("   and grocery_pos are red even during daytime.\n\n")
cat("3. WORST COMBINATIONS: shopping_net at night\n")
cat("   and grocery_pos at night represent the highest\n")
cat("   risk transactions in the entire dataset.\n\n")
cat("4. SAFE ZONES: home, health_fitness, and\n")
cat("   food_dining during daytime show consistently\n")
cat("   low fraud rates (blue cells).\n\n")


#---- Visual 9: Age Distribution by Fraud
# Overlapping density + rug plot
#========================================
cat("========================================\n")
cat("VISUAL 9: Age Distribution by Fraud Status\n")
cat("========================================\n\n")

age_stats <- fraud %>%
  group_by(is_fraud) %>%
  summarise(
    mean   = round(mean(age,   na.rm=TRUE), 1),
    median = round(median(age, na.rm=TRUE), 1),
    sd     = round(sd(age,     na.rm=TRUE), 1),
    .groups= "drop")
print(age_stats)

p9 <- ggplot(fraud,
             aes(x    = age,
                 fill = is_fraud,
                 color= is_fraud)) +
  geom_density(alpha = 0.4,
               lwd   = 1) +
  geom_rug(alpha = 0.05,
           lwd   = 0.3) +
  geom_vline(
    data = age_stats,
    aes(xintercept = mean,
        color      = is_fraud),
    lty = 2, lwd = 1) +
  scale_fill_manual(
    values = c("not_fraud" = "steelblue",
               "fraud"     = "coral"),
    labels = c("Not fraud", "Fraud")) +
  scale_color_manual(
    values = c("not_fraud" = "steelblue",
               "fraud"     = "coral"),
    labels = c("Not fraud", "Fraud")) +
  annotate("text",
           x     = age_stats$mean[
             age_stats$is_fraud=="not_fraud"]-2,
           y     = 0.022,
           label = paste0("Mean ",
                          age_stats$mean[
                            age_stats$is_fraud==
                              "not_fraud"]),
           color = "steelblue",
           size  = 3.5, hjust=1) +
  annotate("text",
           x     = age_stats$mean[
             age_stats$is_fraud=="fraud"]+2,
           y     = 0.022,
           label = paste0("Mean ",
                          age_stats$mean[
                            age_stats$is_fraud==
                              "fraud"]),
           color = "coral",
           size  = 3.5, hjust=0) +
  labs(title    = "Age Distribution by Fraud Status",
       subtitle = "Older customers slightly more likely to be defrauded",
       x        = "Customer Age (years)",
       y        = "Density",
       fill     = "Status",
       color    = "Status") +
  theme_minimal() +
  theme(plot.title = element_text(
    face="bold", size=14))
print(p9)

cat("\nINTERPRETATION:\n")
cat(sprintf("Not fraud mean age: %.1f years\n",
            age_stats$mean[
              age_stats$is_fraud=="not_fraud"]))
cat(sprintf("Fraud     mean age: %.1f years\n",
            age_stats$mean[
              age_stats$is_fraud=="fraud"]))
cat(sprintf("Difference        : %.1f years\n",
            age_stats$mean[
              age_stats$is_fraud=="fraud"] -
              age_stats$mean[
                age_stats$is_fraud=="not_fraud"]))
cat("While both distributions overlap substantially,\n")
cat("fraud victims are on average 3 years older.\n")
cat("The fraud density curve is shifted slightly right\n")
cat("suggesting older customers are marginally more\n")
cat("vulnerable. The effect is real but modest —\n")
cat("age alone is not a strong fraud predictor.\n\n")


#---- Visual 10: Multivariate — amt, hour,
# category colored by fraud
#========================================
cat("========================================\n")
cat("VISUAL 10: Amount vs Hour — Top Risk Categories\n")
cat("========================================\n\n")

# Focus on top 4 fraud categories
top4_cats <- as.character(
  cat_fraud$category[1:4])

fraud_top4 <- fraud %>%
  filter(category %in% top4_cats)

p10 <- ggplot(fraud_top4,
              aes(x     = trans_hour,
                  y     = log_amt,
                  color = is_fraud)) +
  geom_point(alpha = 0.3,
             size  = 0.8) +
  geom_smooth(method = "loess",
              se     = FALSE,
              lwd    = 1.5) +
  scale_color_manual(
    values = c("not_fraud" = "steelblue",
               "fraud"     = "coral"),
    labels = c("Not fraud", "Fraud")) +
  scale_y_continuous(
    breaks = log(c(1,10,100,1000)),
    labels = paste0("$",
                    c(1,10,100,1000))) +
  scale_x_continuous(
    breaks = seq(0,23, by=6),
    labels = paste0(seq(0,23,by=6), ":00")) +
  facet_wrap(~ category, ncol=2) +
  labs(title    = "Amount vs Hour — Top 4 Fraud Categories",
       subtitle = "Fraud (coral) stays high-value across all hours",
       x        = "Hour of Day",
       y        = "Amount (log scale)",
       color    = "Status") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text      = element_text(
          face="bold"),
        plot.title      = element_text(
          face="bold", size=14))
print(p10)

cat("INTERPRETATION:\n")
cat("Across all four high-risk categories:\n\n")
cat("1. FRAUD (coral) stays consistently HIGH on the\n")
cat("   y-axis regardless of the time of day —\n")
cat("   confirming amount is the dominant signal.\n\n")
cat("2. NON-FRAUD (blue) shows a slight dip in amount\n")
cat("   during overnight hours — people make smaller\n")
cat("   genuine purchases late at night.\n\n")
cat("3. SMOOTH LINES diverge most at hours 22-03\n")
cat("   where the gap between fraud and non-fraud\n")
cat("   amount is largest — the combined effect\n")
cat("   of time AND amount is most pronounced.\n\n")
cat("4. shopping_net shows the sharpest overnight\n")
cat("   spike — online shopping fraud peaks hardest\n")
cat("   in the early morning hours.\n\n")


#keeping the trend going with the overall summarization
#========================================
# BIVARIATE SUMMARY TABLE
#========================================
cat("========================================\n")
cat("BIVARIATE / MULTIVARIATE EDA SUMMARY\n")
cat("========================================\n\n")

cat(sprintf("%-6s  %-30s  %s\n",
            "Visual", "Variables","Key Finding"))
cat(strrep("-", 78), "\n")

findings <- data.frame(
  v  = paste0("V", 1:10),
  vars = c(
    "amt vs is_fraud",
    "category vs fraud rate",
    "trans_hour vs fraud rate",
    "Correlation heatmap",
    "age vs log_amt vs fraud",
    "state vs fraud rate",
    "amt × category × fraud",
    "hour × category heatmap",
    "age vs fraud density",
    "amt × hour × top categories"),
  finding = c(
    "Fraud $518 mean vs $67 — A 7.7x difference",
    "shopping_net/grocery_pos 27% fraud rate",
    "Hours 22-23: 41% fraud — 10x the daytime rate",
    "amt/log_amt only meaningful correlates",
    "Fraud clusters at high amounts all ages",
    "Alaska 31.7% — double the average",
    "Amount gap consistent across categories",
    "Night + high-risk category = worst combo",
    "Fraud victims 3 years older on average",
    "shopping_net night fraud sharpest spike"),
  stringsAsFactors = FALSE)

for (i in 1:nrow(findings)) {
  cat(sprintf("%-6s  %-30s  %s\n",
              findings$v[i],
              findings$vars[i],
              findings$finding[i]))
}

cat("\n--- OVERALL FINDINGS ---\n\n")
cat("TOP 3 FRAUD PREDICTORS identified:\n\n")
cat("1. TRANSACTION AMOUNT (strongest signal)\n")
cat("   Fraud mean $518 vs non-fraud $67.\n")
cat("   Single most powerful predictor.\n\n")
cat("2. HOUR OF DAY (time signal)\n")
cat("   Hours 22-03 have 10x higher fraud rate.\n")
cat("   Night transactions should trigger alerts.\n\n")
cat("3. CATEGORY (category signal)\n")
cat("   shopping_net and grocery_pos at 27%.\n")
cat("   Category × hour interaction is powerful.\n\n")
cat("NOT useful: dist_km (r ≈ 0), age (weak)\n\n")

#Next steps
cat("--- NEXT STEPs ---\n")
cat("Step 6: Feature engineering and model building\n")
cat("Key features to include:\n")
cat("  - amt / log_amt\n")
cat("  - trans_hour (especially flag night hours)\n")
cat("  - category (as dummy variables)\n")
cat("  - age\n")
cat("  - state\n")
cat("  - is_night flag (hour >= 22 or hour <= 3)\n")



#--------------- Step 6 
# Feature Engineering 

#========================================
# STEP 6: FEATURE ENGINEERING
# fraud data — cleaned dataset
#========================================
# Call libraries if havent done so. Ctrl+f is an easy way to search
# if you have called the librarieis previoisly or not
# library(dplyr)
# library(lubridate)
# library(ggplot2)
#library(scales)

cat("========================================\n")
cat("STEP 6: FEATURE ENGINEERING\n")
cat("========================================\n\n")
cat(sprintf("Starting columns: %d\n", ncol(fraud)))
cat(sprintf("Starting rows   : %d\n\n", nrow(fraud)))

# Work on a copy
fraud_feat <- fraud

#-----------------------------------------------

# GROUP 1: DATE & TIME FEATURES
# need to extract granular time components
#========================================
cat("========================================\n")
cat("GROUP 1: DATE & TIME FEATURES\n")
cat("========================================\n\n")

# F01: is_night
# Night = hours 22,23,0,1,2,3
# Based on bivariate analysis: 34% fraud rate
# vs 2.5% daytime — 13x difference

fraud_feat$is_night <- as.integer(
  fraud_feat$trans_hour %in% c(22,23,0,1,2,3))

cat("F01: is_night\n")
cat(sprintf("  Night transactions: %d (%.1f%%)\n",
            sum(fraud_feat$is_night),
            mean(fraud_feat$is_night)*100))
cat(sprintf("  Night fraud rate  : %.1f%%\n",
            mean(fraud_feat$is_fraud[
              fraud_feat$is_night==1]=="fraud")*100))
cat(sprintf("  Day   fraud rate  : %.1f%%\n\n",
            mean(fraud_feat$is_fraud[
              fraud_feat$is_night==0]=="fraud")*100))

# F02: hour_sin and hour_cos
# Cyclical encoding of hour — treats 23 and 0
# as adjacent (they are!) rather than far apart
fraud_feat$hour_sin <- sin(
  2 * pi * fraud_feat$trans_hour / 24)
fraud_feat$hour_cos <- cos(
  2 * pi * fraud_feat$trans_hour / 24)

cat("F02: hour_sin / hour_cos (cyclical encoding)\n")
cat("  Encodes 23:00 and 00:00 as adjacent\n")
cat("  Prevents models treating hour as linear\n\n")

# F03: is_weekend
fraud_feat$is_weekend <- as.integer(
  fraud_feat$trans_day %in% c("Sat","Sun"))

cat("F03: is_weekend\n")
cat(sprintf("  Weekend fraud rate: %.1f%%\n",
            mean(fraud_feat$is_fraud[
              fraud_feat$is_weekend==1]=="fraud")*100))
cat(sprintf("  Weekday fraud rate: %.1f%%\n\n",
            mean(fraud_feat$is_fraud[
              fraud_feat$is_weekend==0]=="fraud")*100))

# F04: day_of_week numeric (Mon=1, Sun=7)
fraud_feat$day_num <- as.integer(
  factor(fraud_feat$trans_day,
         levels = c("Mon","Tue","Wed",
                    "Thu","Fri","Sat","Sun")))

cat("F04: day_num (Mon=1 ... Sun=7)\n")
cat("  Numeric encoding of day of week\n\n")


# F05: is_month_end (last 3 days of month)
fraud_feat$trans_mday <- mday(
  fraud_feat$trans_date_trans_time)
fraud_feat$is_month_end <- as.integer(
  fraud_feat$trans_mday >= 29)

cat("F05: is_month_end (day >= 29)\n")
cat(sprintf("  Month-end fraud rate: %.1f%%\n",
            mean(fraud_feat$is_fraud[
              fraud_feat$is_month_end==1]=="fraud")*100))
cat(sprintf("  Other days   rate   : %.1f%%\n\n",
            mean(fraud_feat$is_fraud[
              fraud_feat$is_month_end==0]=="fraud")*100))

# F06: quarter
fraud_feat$quarter <- quarter(
  fraud_feat$trans_date_trans_time)

cat("F06: quarter (1-4)\n")
quarter_fraud <- tapply(
  fraud_feat$is_fraud == "fraud",
  fraud_feat$quarter, mean) * 100
for (q in 1:4) {
  cat(sprintf("  Q%d fraud rate: %.1f%%\n",
              q, quarter_fraud[q]))
}
cat("\n")

#-----------------------------------------------

# GROUP 2: AMOUNT FEATURES
# Transform and bin transaction amounts
#========================================
cat("========================================\n")
cat("GROUP 2: AMOUNT FEATURES\n")
cat("========================================\n\n")

# F07: amt_bin — categorical amount buckets
# Based on percentile analysis from Step 4
fraud_feat$amt_bin <- cut(
  fraud_feat$amt,
  breaks = c(0, 10, 50, 100,
             500, 1000, Inf),
  labels = c("micro",      # $0-10
             "small",      # $10-50
             "medium",     # $50-100
             "large",      # $100-500
             "very_large", # $500-1000
             "extreme"),   # $1000+
  right  = FALSE)

cat("F07: amt_bin — amount buckets\n")
amt_bin_fraud <- fraud_feat %>%
  group_by(amt_bin) %>%
  summarise(
    n          = n(),
    pct        = round(n()/nrow(fraud_feat)*100,1),
    fraud_rate = round(
      mean(is_fraud=="fraud")*100, 1),
    .groups    = "drop")
cat(sprintf("  %-12s  %6s  %6s  %s\n",
            "Bin", "n", "Pct%", "Fraud%"))
cat(strrep("-", 38), "\n")
for (i in 1:nrow(amt_bin_fraud)) {
  cat(sprintf("  %-12s  %6d  %5.1f%%  %5.1f%%\n",
              as.character(amt_bin_fraud$amt_bin[i]),
              amt_bin_fraud$n[i],
              amt_bin_fraud$pct[i],
              amt_bin_fraud$fraud_rate[i]))
}
cat("\n")

# F08: is_high_value
# Transactions above 95th percentile ($772)
p95_amt <- quantile(fraud_feat$amt, 0.95)
fraud_feat$is_high_value <- as.integer(
  fraud_feat$amt > p95_amt)

cat(sprintf("F08: is_high_value (amt > $%.2f — 95th pct)\n",
            p95_amt))
cat(sprintf("  High value fraud rate: %.1f%%\n",
            mean(fraud_feat$is_fraud[
              fraud_feat$is_high_value==1]=="fraud")*100))
cat(sprintf("  Normal    fraud rate : %.1f%%\n\n",
            mean(fraud_feat$is_fraud[
              fraud_feat$is_high_value==0]=="fraud")*100))

# F09: amt_zscore
# How many SDs from the mean
fraud_feat$amt_zscore <- (fraud_feat$amt -
                          mean(fraud_feat$amt)) /
  sd(fraud_feat$amt)

cat("F09: amt_zscore — standardised amount\n")
cat(sprintf("  Mean zscore (fraud)    : %.4f\n",
            mean(fraud_feat$amt_zscore[
              fraud_feat$is_fraud=="fraud"])))
cat(sprintf("  Mean zscore (not_fraud): %.4f\n\n",
            mean(fraud_feat$amt_zscore[
              fraud_feat$is_fraud=="not_fraud"])))

# F10: log_amt already exists from Step 3
cat("F10: log_amt — already created in Step 3\n\n")


#-----------------------------------------------

# GROUP 3: RISK SCORING FEATURES
# Encode historical fraud rates per group
#========================================
cat("========================================\n")
cat("GROUP 3: RISK SCORING FEATURES\n")
cat("========================================\n\n")

# F11: category_fraud_rate
# Encode the observed fraud rate per category
# as a continuous numeric feature
cat_rates <- fraud_feat %>%
  group_by(category) %>%
  summarise(
    category_fraud_rate = mean(
      is_fraud == "fraud"),
    .groups = "drop")

fraud_feat <- fraud_feat %>%
  left_join(cat_rates,
            by = "category")

cat("F11: category_fraud_rate\n")
cat("  Encodes category-level fraud rate\n")
cat("  as continuous numeric feature\n")
cat(sprintf("  Range: %.4f to %.4f\n",
            min(fraud_feat$category_fraud_rate),
            max(fraud_feat$category_fraud_rate)))
cat(sprintf("  Correlation with fraud: %.4f\n\n",
            cor(fraud_feat$category_fraud_rate,
                as.numeric(
                  fraud_feat$is_fraud=="fraud"))))

# F12: state_fraud_rate
state_rates <- fraud_feat %>%
  group_by(state) %>%
  summarise(
    state_fraud_rate = mean(
      is_fraud == "fraud"),
    .groups = "drop")

fraud_feat <- fraud_feat %>% left_join(state_rates, by = "state")

cat("F12: state_fraud_rate\n")
cat(sprintf("  Range: %.4f to %.4f\n",
            min(fraud_feat$state_fraud_rate),
            max(fraud_feat$state_fraud_rate)))
cat(sprintf("  Correlation with fraud: %.4f\n\n",
            cor(fraud_feat$state_fraud_rate,
                as.numeric(
                  fraud_feat$is_fraud=="fraud"))))

# F13: is_high_risk_category
# Top 3 categories with fraud rate > 20%
high_risk_cats <- cat_rates$category[
  cat_rates$category_fraud_rate > 0.20]
fraud_feat$is_high_risk_category <- as.integer(
  fraud_feat$category %in% high_risk_cats)

cat("F13: is_high_risk_category\n")
cat("  Categories with fraud rate > 20%:\n")
for (c in high_risk_cats) {
  cat(sprintf("    %s\n", c))
}
cat(sprintf("  High risk cat fraud rate: %.1f%%\n",
            mean(fraud_feat$is_fraud[
              fraud_feat$is_high_risk_category==1]
              =="fraud")*100))
cat(sprintf("  Low  risk cat fraud rate: %.1f%%\n\n",
            mean(fraud_feat$is_fraud[
              fraud_feat$is_high_risk_category==0]
              =="fraud")*100))

# F14: risk_score
# Composite score combining night + high value
# + high risk category
# Each flag contributes equally (0-3 scale)
fraud_feat$risk_score <- fraud_feat$is_night +
  fraud_feat$is_high_value +
  fraud_feat$is_high_risk_category

cat("F14: risk_score (0-3 composite)\n")
cat("  is_night + is_high_value + is_high_risk_cat\n")
risk_fraud <- fraud_feat %>%
  group_by(risk_score) %>%
  summarise(
    n          = n(),
    fraud_rate = round(
      mean(is_fraud=="fraud")*100, 1),
    .groups    = "drop")
cat(sprintf("  %-12s  %8s  %s\n",
            "Score", "n", "Fraud rate"))
cat(strrep("-", 35), "\n")
for (i in 1:nrow(risk_fraud)) {
  cat(sprintf("  %-12d  %8d  %.1f%%\n",
              risk_fraud$risk_score[i],
              risk_fraud$n[i],
              risk_fraud$fraud_rate[i]))
}
cat("\n")

#-----------------------------------------------

# GROUP 4: CUSTOMER FEATURES
# Age groups and customer risk segments
#========================================
cat("========================================\n")
cat("GROUP 4: CUSTOMER FEATURES\n")
cat("========================================\n\n")

# F15: age_group
fraud_feat$age_group <- cut(
  fraud_feat$age,
  breaks = c(0, 25, 40, 55, 70, 100),
  labels = c("18-25", "26-40",
             "41-55", "56-70", "71+"),
  right  = FALSE)

cat("F15: age_group — age buckets\n")
age_fraud <- fraud_feat %>%
  group_by(age_group) %>%
  summarise(
    n          = n(),
    fraud_rate = round(
      mean(is_fraud=="fraud")*100, 1),
    .groups    = "drop")
cat(sprintf("  %-10s  %8s  %s\n",
            "Age group", "n", "Fraud rate"))
cat(strrep("-", 32), "\n")
for (i in 1:nrow(age_fraud)) {
  cat(sprintf("  %-10s  %8d  %.1f%%\n",
              as.character(age_fraud$age_group[i]),
              age_fraud$n[i],
              age_fraud$fraud_rate[i]))
}
cat("\n")

# F16: is_senior (age >= 60)
fraud_feat$is_senior <- as.integer(fraud_feat$age >= 60)

cat("F16: is_senior (age >= 60)\n")
cat(sprintf("  Senior fraud rate    : %.1f%%\n",
            mean(fraud_feat$is_fraud[
              fraud_feat$is_senior==1]=="fraud")*100))
cat(sprintf("  Non-senior fraud rate: %.1f%%\n\n",
            mean(fraud_feat$is_fraud[
              fraud_feat$is_senior==0]=="fraud")*100))

#-----------------------------------------------

# GROUP 5: GEOGRAPHIC FEATURES
# City size and location-based features
#========================================
cat("========================================\n")
cat("GROUP 5: GEOGRAPHIC FEATURES\n")
cat("========================================\n\n")

# F17: city_size
# Bin city population into urban tiers
fraud_feat$city_size <- cut(
  fraud_feat$city_pop,
  breaks = c(0, 500, 2000,
             50000, 500000, Inf),
  labels = c("rural",       # < 500
             "small_town",  # 500-2k
             "town",        # 2k-50k
             "city",        # 50k-500k
             "metro"),      # 500k+
  right  = FALSE)

cat("F17: city_size — urban tier\n")
city_fraud <- fraud_feat %>%
  group_by(city_size) %>%
  summarise(
    n          = n(),
    fraud_rate = round(
      mean(is_fraud=="fraud")*100, 1),
    .groups    = "drop")
cat(sprintf("  %-12s  %8s  %s\n",
            "City tier", "n", "Fraud rate"))
cat(strrep("-", 35), "\n")
for (i in 1:nrow(city_fraud)) {
  cat(sprintf("  %-12s  %8d  %.1f%%\n",
              as.character(city_fraud$city_size[i]),
              city_fraud$n[i],
              city_fraud$fraud_rate[i]))
}
cat("\n")

# F18: log_city_pop
fraud_feat$log_city_pop <- log(
  fraud_feat$city_pop + 1)

cat("F18: log_city_pop (log scale population)\n")
cat(sprintf("  Correlation with fraud: %.4f\n\n",
            cor(fraud_feat$log_city_pop,
                as.numeric(
                  fraud_feat$is_fraud=="fraud"))))

#-----------------------------------------------

# GROUP 6: INTERACTION FEATURES
# Combine strongest predictors
#========================================
cat("========================================\n")
cat("GROUP 6: INTERACTION FEATURES\n")
cat("========================================\n\n")

# F19: night_x_high_value
# Transactions that are BOTH night AND high value
fraud_feat$night_x_high_value <-
  fraud_feat$is_night *
  fraud_feat$is_high_value

cat("F19: night_x_high_value (interaction)\n")
cat(sprintf("  Both night AND high value: %d txns\n",
            sum(fraud_feat$night_x_high_value)))
cat(sprintf("  Fraud rate: %.1f%%\n\n",
            mean(fraud_feat$is_fraud[
              fraud_feat$night_x_high_value==1]
              =="fraud")*100))

# F20: night_x_high_risk_cat
fraud_feat$night_x_high_risk_cat <-
  fraud_feat$is_night *
  fraud_feat$is_high_risk_category

cat("F20: night_x_high_risk_cat (interaction)\n")
cat(sprintf("  Night AND high-risk category: %d txns\n",
            sum(fraud_feat$night_x_high_risk_cat)))
cat(sprintf("  Fraud rate: %.1f%%\n\n",
            mean(fraud_feat$is_fraud[
              fraud_feat$night_x_high_risk_cat==1]
              =="fraud")*100))

# F21: amt_x_risk_score
# Amount weighted by risk level
fraud_feat$amt_x_risk <- fraud_feat$log_amt *
  (fraud_feat$risk_score + 1)

cat("F21: amt_x_risk (log_amt × (risk_score+1))\n")
cat("  Amplifies amount signal for risky transactions\n")
cat(sprintf("  Correlation with fraud: %.4f\n\n",
            cor(fraud_feat$amt_x_risk,
                as.numeric(
                  fraud_feat$is_fraud=="fraud"))))

#-----------------------------------------------

# GROUP 7: RATIO & DIFFERENCE FEATURES
#========================================
cat("========================================\n")
cat("GROUP 7: RATIO & DIFFERENCE FEATURES\n")
cat("========================================\n\n")

# F22: amt_to_city_pop_ratio
# Transaction amount relative to city size
# Large amounts in small towns = suspicious
fraud_feat$amt_per_1k_pop <-
  fraud_feat$amt /
  (fraud_feat$city_pop / 1000 + 1)

cat("F22: amt_per_1k_pop (amount per 1000 residents)\n")
cat("  Large amounts in small towns = higher ratio\n")
cat(sprintf("  Fraud mean   : %.4f\n",
            mean(fraud_feat$amt_per_1k_pop[
              fraud_feat$is_fraud=="fraud"])))
cat(sprintf("  Non-fraud mean: %.4f\n",
            mean(fraud_feat$amt_per_1k_pop[
              fraud_feat$is_fraud=="not_fraud"])))
cat(sprintf("  Correlation with fraud: %.4f\n\n",
            cor(fraud_feat$amt_per_1k_pop,
                as.numeric(
                  fraud_feat$is_fraud=="fraud"))))

# F23: lat_diff / long_diff
# Absolute difference between customer and merchant
fraud_feat$lat_diff  <- abs(fraud_feat$lat -
                            fraud_feat$merch_lat)
fraud_feat$long_diff <- abs(fraud_feat$long -
                            fraud_feat$merch_long)

cat("F23: lat_diff / long_diff\n")
cat("  Absolute coordinate differences\n")
cat(sprintf("  lat_diff  correlation with fraud: %.4f\n",
            cor(fraud_feat$lat_diff,
                as.numeric(
                  fraud_feat$is_fraud=="fraud"))))
cat(sprintf("  long_diff correlation with fraud: %.4f\n\n",
            cor(fraud_feat$long_diff,
                as.numeric(
                  fraud_feat$is_fraud=="fraud"))))

#23 features so far, Do I neeed them all??
#theres no way all these are necessary

#evaluate features

#-----------------------------------------------
# FEATURE EVALUATION
# Correlation of all new features with fraud
#========================================
cat("========================================\n")
cat("FEATURE EVALUATION\n")
cat("Correlation of all features with is_fraud\n")
cat("========================================\n\n")

fraud_num <- as.numeric(fraud_feat$is_fraud == "fraud")

feature_cols <- c(
  # Original
  "amt", "log_amt", "age",
  "dist_km", "trans_hour", "city_pop",
  # Group 1: Time
  "is_night", "hour_sin", "hour_cos",
  "is_weekend", "day_num",
  "is_month_end", "quarter",
  # Group 2: Amount
  "is_high_value", "amt_zscore",
  # Group 3: Risk scores
  "category_fraud_rate",
  "state_fraud_rate",
  "is_high_risk_category",
  "risk_score",
  # Group 4: Customer
  "is_senior",
  # Group 5: Geographic
  "log_city_pop",
  # Group 6: Interactions
  "night_x_high_value",
  "night_x_high_risk_cat",
  "amt_x_risk",
  # Group 7: Ratios
  "amt_per_1k_pop",
  "lat_diff", "long_diff")

feature_eval <- data.frame()
for (f in feature_cols) {
  if (f %in% names(fraud_feat) &&
      is.numeric(fraud_feat[[f]])) {
    vals <- fraud_feat[[f]]
    vals_clean <- vals[!is.na(vals)]
    fraud_clean <- fraud_num[!is.na(vals)]
    r    <- round(cor(vals_clean,
                      fraud_clean), 4)
    feature_eval <- rbind(
      feature_eval,
      data.frame(feature    = f,
                 correlation = r,
                 abs_corr    = abs(r),
                 stringsAsFactors = FALSE))
  }
}

feature_eval <- feature_eval[
  order(-feature_eval$abs_corr), ]
rownames(feature_eval) <- NULL

cat(sprintf("%-28s  %8s  %s\n",
            "Feature", "r", "Signal strength"))
cat(strrep("-", 58), "\n")
for (i in 1:nrow(feature_eval)) {
  r   <- feature_eval$correlation[i]
  str <- ifelse(abs(r) >= 0.4,  "STRONG   ***",
                ifelse(abs(r) >= 0.2,  "MODERATE **",
                       ifelse(abs(r) >= 0.05, "WEAK     *",
                              "NEGLIGIBLE")))
  cat(sprintf("%-28s  %8.4f  %s\n",
              feature_eval$feature[i], r, str))
}

#Remember to visualize findings

#========================================
# VISUALISE FEATURE IMPORTANCE
#========================================

# Plot correlation with fraud
top_features <- head(feature_eval, 20)
top_features$feature <- factor(
  top_features$feature,
  levels = rev(top_features$feature))
top_features$direction <- ifelse(
  top_features$correlation > 0,
  "Positive", "Negative")
# colors()
p_feat <- ggplot(
  top_features,
  aes(x    = feature,
      y    = correlation,
      fill = direction)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = round(correlation, 3),
                hjust = ifelse(correlation > 0,
                               -0.1, 1.1)),
            size = 3) +
  coord_flip() +
  scale_fill_manual(
    values = c("Positive" = "coral",
               "Negative" = "cornflowerblue")) +
  scale_y_continuous(
    expand = expansion(mult = c(0.15, 0.15))) +
  geom_vline(xintercept = 0,
             color = "black",
             lwd   = 0.5) +
  labs(title    = "Feature Correlation with Fraud (Top 20)",
       subtitle = "Red = positive correlation with fraud | Blue = negative",
       x        = NULL,
       y        = "Pearson r with is_fraud",
       fill     = "Direction") +
  theme_minimal() +
  theme(plot.title = element_text(
    face="bold", size=14),
    legend.position = "bottom")
print(p_feat)

# Plot risk_score distribution by fraud
p_risk <- ggplot(
  fraud_feat,
  aes(x    = factor(risk_score),
      fill = is_fraud)) +
  geom_bar(position = "fill",
           alpha    = 0.85) +
  scale_fill_manual(
    values = c("not_fraud" = "cornflowerblue",
               "fraud"     = "coral"),
    labels = c("Not fraud", "Fraud")) +
  scale_y_continuous(
    labels = percent_format()) +
  geom_hline(yintercept = mean(
    fraud_feat$is_fraud=="fraud"),
    lty = 2, color = "black",
    lwd = 0.8) +
  labs(title    = "Fraud Rate by Composite Risk Score",
       subtitle = "Score 0 = low risk | Score 3 = high risk",
       x        = "Risk Score (0-3)",
       y        = "Proportion",
       fill     = "Status") +
  theme_minimal() +
  theme(plot.title = element_text(
    face="bold", size=14))
print(p_risk)

# Plot amt_bin fraud rates
p_amt_bin <- ggplot(
  amt_bin_fraud,
  aes(x    = amt_bin,
      y    = fraud_rate,
      fill = fraud_rate)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(fraud_rate, "%")),
            vjust = -0.4, size = 3.5,
            fontface = "bold") +
  scale_fill_gradient(
    low  = "cornflowerblue",
    high = "coral") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    labels = function(x) paste0(x, "%")) +
  labs(title    = "Fraud Rate by Transaction Amount Bin",
       subtitle = "Large and extreme transactions are highest risk",
       x        = "Amount Category",
       y        = "Fraud Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title      = element_text(
          face="bold", size=14))
print(p_amt_bin)

#========================================
# FINAL FEATURE VECTOR
#========================================
cat("\n========================================\n")
cat("FINAL FEATURE VECTOR\n")
cat("========================================\n\n")

# need to select final features for modelling
final_features <- c(
  # Target
  "is_fraud",
  # Original numeric
  "amt", "log_amt", "age",
  "trans_hour", "city_pop",
  # Time features
  "is_night", "hour_sin", "hour_cos",
  "is_weekend", "is_month_end",
  "quarter",
  # Amount features
  "amt_bin", "is_high_value",
  "amt_zscore",
  # Risk features
  "category_fraud_rate",
  "state_fraud_rate",
  "is_high_risk_category",
  "risk_score",
  # Customer
  "age_group", "is_senior",
  # Geographic
  "city_size", "log_city_pop",
  # Interaction
  "night_x_high_value",
  "night_x_high_risk_cat",
  "amt_x_risk",
  # Ratio
  "amt_per_1k_pop",
  # Categorical (for encoding)
  "category", "state")

fraud_final <- fraud_feat[, final_features[final_features %in% names(fraud_feat)]]

cat(sprintf("Original features : %d\n",
            ncol(fraud)))
cat(sprintf("Engineered features added: %d\n",
            ncol(fraud_feat) - ncol(fraud)))
cat(sprintf("Final feature vector: %d features\n\n",
            ncol(fraud_final)))

cat("FINAL FEATURE LIST:\n")
cat(strrep("-", 60), "\n")

feature_groups <- list(
  "Target variable"    = c("is_fraud"),
  "Original numeric"   = c("amt","log_amt",
                           "age","trans_hour",
                           "city_pop"),
  "Time features"      = c("is_night",
                           "hour_sin","hour_cos",
                           "is_weekend",
                           "is_month_end",
                           "quarter"),
  "Amount features"    = c("amt_bin",
                           "is_high_value",
                           "amt_zscore"),
  "Risk scores"        = c("category_fraud_rate",
                           "state_fraud_rate",
                           "is_high_risk_category",
                           "risk_score"),
  "Customer features"  = c("age_group",
                           "is_senior"),
  "Geographic"         = c("city_size",
                           "log_city_pop"),
  "Interactions"       = c("night_x_high_value",
                           "night_x_high_risk_cat",
                           "amt_x_risk"),
  "Ratios"             = c("amt_per_1k_pop"),
  "Categorical (encode)" = c("category","state"))

for (group in names(feature_groups)) {
  cat(sprintf("\n%-22s : ", group))
  feats <- feature_groups[[group]]
  feats <- feats[feats %in% names(fraud_final)]
  cat(paste(feats, collapse=", "), "\n")
}

cat("\n\n--- TOP 5 FEATURES BY CORRELATION ---\n")
top5 <- head(feature_eval, 5)
for (i in 1:5) {
  cat(sprintf("  %d. %-28s r = %.4f\n",
              i,
              top5$feature[i],
              top5$correlation[i]))
}

# 1. amt_x_risk                   r = 0.6612
# 2. amt                          r = 0.6496
# 3. amt_zscore                   r = 0.6496
# 4. risk_score                   r = 0.5645
# 5. is_high_value                r = 0.5640


cat("\n--- DROPPED FEATURES (low signal) ---\n")
dropped <- c("dist_km",
             "lat", "long",
             "merch_lat", "merch_long",
             "trans_num",
             "dob",
             "merchant",
             "job",
             "city",
             "trans_date_trans_time")
for (d in dropped) {
  reason <- switch(d,
                   "dist_km"  = "r ≈ 0 — no fraud signal",
                   "lat"      = "replaced by city_size/state",
                   "long"     = "replaced by city_size/state",
                   "merch_lat"= "replaced by dist_km/lat_diff",
                   "merch_long"="replaced by dist_km/long_diff",
                   "trans_num"= "unique ID — no signal",
                   "dob"      = "replaced by age/age_group",
                   "merchant" = "693 levels — too high cardinality",
                   "job"      = "163 levels — too high cardinality",
                   "city"     = "176 levels — replaced by city_size",
                   "trans_date_trans_time"=
                     "replaced by time components")
  cat(sprintf("  %-28s %s\n", d, reason))
}

cat(sprintf("\n\nFinal dataset: %d rows × %d features\n",
            nrow(fraud_final),
            ncol(fraud_final)))

#ok this is a good. Now on to step 7 

cat("Ready for Step 7: Model Building\n")



#--------------- Step 7
# Descriptive Modeling 

#========================================
# STEP 7: DESCRIPTIVE MODELING
# fraud_data — feature engineered dataset
#========================================

# library(ggplot2)
# library(dplyr)
# library(scales)

#========================================
# Ensure all engineered features exist from step 6
#========================================
cat("========================================\n")
cat("PREPARING MODELING DATASET\n")
cat("========================================\n\n")

# Rebuild all needed features "cleanly"
# copy fraud
fraud_model <- fraud

# Core engineered features
fraud_model$log_amt   <- log(fraud_model$amt)
fraud_model$is_night  <- as.integer(fraud_model$trans_hour %in% c(22,23,0,1,2,3))
fraud_model$is_weekend <- as.integer(fraud_model$trans_day %in% c("Sat","Sun"))

# Category fraud rate encoding
cat_rates <- fraud_model %>%
  group_by(category) %>%
  summarise(category_fraud_rate =
              mean(is_fraud=="fraud"),
            .groups="drop")
fraud_model <- left_join(
  fraud_model, cat_rates, by="category")

p95_amt <- quantile(fraud_model$amt, 0.95)
fraud_model$is_high_value <- as.integer(
  fraud_model$amt > p95_amt)
fraud_model$is_high_risk_category <- as.integer(
  fraud_model$category_fraud_rate > 0.20)
fraud_model$risk_score <-
  fraud_model$is_night +
  fraud_model$is_high_value +
  fraud_model$is_high_risk_category

# Ordered factor for category (by fraud rate)
cat_order <- cat_rates %>%
  arrange(desc(category_fraud_rate)) %>%
  pull(category)
fraud_model$category <- factor(
  fraud_model$category, levels=cat_order)

cat(sprintf("Modeling dataset: %d rows × %d cols\n\n",
            nrow(fraud_model),
            ncol(fraud_model)))

#models to assembmle: 
#logistic regression, linear regresssion,  ANOVA , and k-means clustering (if applicable) 


# MODEL 1: LOGISTIC REGRESSION
# Predict P(fraud) from key predictors
#========================================
cat("========================================\n")
cat("MODEL 1: LOGISTIC REGRESSION\n")
cat("Predicting P(is_fraud = fraud)\n")
cat("========================================\n\n")

# Fit model
logit_model <- glm(
  is_fraud ~ log_amt +
    is_night +
    category_fraud_rate +
    age +
    is_weekend,
  data   = fraud_model,
  family = binomial)

logit_summary <- summary(logit_model)
print(logit_summary)

# Extract key metrics - Mcfadden r2 , aic, null deviance, deviance, res dev
mcfadden_r2 <- round(
  1 - logit_model$deviance /
    logit_model$null.deviance, 4)
aic         <- round(logit_model$aic, 2)
null_dev    <- round(logit_model$null.deviance,2)
res_dev     <- round(logit_model$deviance, 2)
dev_red     <- round(null_dev - res_dev, 2)

cat("\n========================================\n")
cat("MODEL 1: FIT STATISTICS\n")
cat("========================================\n\n")
cat(sprintf("AIC                : %.2f\n", aic))
cat(sprintf("Null deviance      : %.2f\n", null_dev))
cat(sprintf("Residual deviance  : %.2f\n", res_dev))
cat(sprintf("Deviance reduction : %.2f\n", dev_red))
cat(sprintf("McFadden R²        : %.4f\n\n",
            mcfadden_r2))

# Odds ratios table
cat("--- ODDS RATIOS ---\n")
coef_tbl <- logit_summary$coefficients
cat(sprintf("%-25s  %10s  %10s  %10s  %8s\n",
            "Predictor", "Coef",
            "Exp(B)", "p-value", "Sig"))
cat(strrep("-", 68), "\n")
for (pred in rownames(coef_tbl)) {
  est  <- coef_tbl[pred,"Estimate"]
  expb <- exp(est)
  pval <- coef_tbl[pred,"Pr(>|z|)"]
  sig  <- ifelse(pval<0.001,"***",
                 ifelse(pval<0.01, "**",
                        ifelse(pval<0.05, "*",
                               ifelse(pval<0.1,  ".", ""))))
  cat(sprintf("%-25s  %10.4f  %10.4f  %10.6f  %s\n",
              pred, est, expb, pval, sig))
}

# Confusion matrix at 0.5 cutoff
pred_probs  <- predict(logit_model,
                       type="response")
pred_class  <- ifelse(pred_probs >= 0.5,
                      "fraud", "not_fraud")
pred_factor <- factor(pred_class,
                      levels=c("not_fraud",
                               "fraud"))

conf_mat <- table(
  Actual    = fraud_model$is_fraud,
  Predicted = pred_factor)

TP <- conf_mat["fraud",    "fraud"]
TN <- conf_mat["not_fraud","not_fraud"]
FP <- conf_mat["not_fraud","fraud"]
FN <- conf_mat["fraud",    "not_fraud"]

accuracy  <- round((TP+TN)/sum(conf_mat)*100,2)
precision <- round(TP/(TP+FP)*100, 2)
recall    <- round(TP/(TP+FN)*100, 2)
spec      <- round(TN/(TN+FP)*100, 2)
f1        <- round(2*(precision*recall)/
                     (precision+recall), 2)

cat("\n--- CONFUSION MATRIX ---\n")
print(conf_mat)
cat(sprintf("\nAccuracy   : %.2f%%\n", accuracy))
cat(sprintf("Precision  : %.2f%%\n", precision))
cat(sprintf("Recall     : %.2f%%\n", recall))
cat(sprintf("Specificity: %.2f%%\n", spec))
cat(sprintf("F1 Score   : %.2f%%\n\n", f1))

# --- Logistic Diagnostics ---
par(mfrow=c(1,2), mar=c(5,5,4,2))

# Plot 1a: Predicted prob distribution
hist(pred_probs,
     breaks = 40,
     main   = "M1: Predicted Probabilities",
     xlab   = "P(fraud)",
     ylab   = "Frequency",
     col    = rgb(0.2,0.4,0.8,0.6),
     border = "white")
abline(v=0.5, col="red", lwd=2, lty=2)
legend("topright",
       legend = "Cutoff = 0.50",
       col="red", lty=2, lwd=2, bty="n")

# Plot 1b: Predicted prob by actual class
boxplot(pred_probs ~ fraud_model$is_fraud,
        main   = "M1: Predicted Prob by \nActual Class",
        xlab   = "Actual Class",
        ylab   = "Predicted P(fraud)",
        col    = c("cornflowerblue","coral"),
        border = "black",
        outline= TRUE)
abline(h=0.5, col="red", lwd=1.5, lty=2)

par(mfrow=c(1,1)) #reset

# Commentary
cat("========================================\n")
cat("MODEL 1 COMMENTARY\n")
cat("========================================\n\n")

cat("--- MODEL FIT ---\n")
cat(sprintf("McFadden R² = %.4f — ", mcfadden_r2))
if (mcfadden_r2 >= 0.2) {
  cat("GOOD fit (>= 0.20)\n")
} else if (mcfadden_r2 >= 0.1) {
  cat("ACCEPTABLE fit (0.10-0.20)\n")
} else {
  cat("WEAK fit (< 0.10)\n")
}
cat(sprintf("Deviance reduced by %.2f (%.1f%%) —\n",
            dev_red,
            dev_red/null_dev*100))
cat("the predictors add substantial explanatory power\n")
cat("over an intercept-only baseline model.\n\n")

cat("--- PREDICTOR INTERPRETATION ---\n\n")

coefs <- coef(logit_model)

cat(sprintf("log_amt (coef=%.4f, OR=%.4f):\n",
            coefs["log_amt"], exp(coefs["log_amt"])))
cat("  The strongest continuous predictor.\n")
cat(sprintf("  Each 1-unit increase in log(amt) multiplies\n"))
cat(sprintf("  odds of fraud by %.2fx.\n",
            exp(coefs["log_amt"])))
cat("  A transaction of $500 vs $50 (log diff=2.3)\n")
cat(sprintf("  increases fraud odds by %.1fx.\n\n",
            exp(coefs["log_amt"]*2.3)))

cat(sprintf("is_night (coef=%.4f, OR=%.4f):\n",
            coefs["is_night"],
            exp(coefs["is_night"])))
cat(sprintf("  Night transactions have %.1fx higher odds\n",
            exp(coefs["is_night"])))
cat("  of fraud than daytime — the strongest\n")
cat("  binary predictor in the model.\n\n")

cat(sprintf("category_fraud_rate (coef=%.4f):\n",
            coefs["category_fraud_rate"]))
cat("  Each percentage point increase in category\n")
cat("  fraud rate meaningfully increases P(fraud).\n\n")

cat(sprintf("age (coef=%.4f, OR=%.4f):\n",
            coefs["age"], exp(coefs["age"])))
cat("  Small but significant positive effect.\n")
cat(sprintf("  Each extra year increases fraud odds by %.4fx.\n",
            exp(coefs["age"])))
cat("  A 70-year-old has ~%.1f%% higher fraud odds\n",
    (exp(coefs["age"]*20)-1)*100)
cat("  than a 50-year-old.\n\n")

cat(sprintf("is_weekend (coef=%.4f, p=%.4f):\n",
            coefs["is_weekend"],
            coef_tbl["is_weekend","Pr(>|z|)"]))
if (coef_tbl["is_weekend","Pr(>|z|)"] < 0.05) {
  cat("  Significant effect on fraud probability.\n\n")
} else {
  cat("  Not significant — weekend alone does not\n")
  cat("  meaningfully predict fraud.\n\n")
}

cat("--- CLASSIFICATION PERFORMANCE ---\n")
cat(sprintf("Accuracy   : %.2f%% — well above the naive\n",
            accuracy))
cat(sprintf("  baseline of 87.6%% (predict all not_fraud)\n"))
cat(sprintf("Recall     : %.2f%% — model catches %.0f in\n",
            recall, recall))
cat("  every 100 actual fraud cases\n")
cat(sprintf("Precision  : %.2f%% — of predicted fraud cases\n",
            precision))
cat("  this proportion are truly fraudulent\n")
cat(sprintf("F1 Score   : %.2f%% — balanced precision/recall\n\n",
            f1))

#linear regression
# MODEL 2: LINEAR REGRESSION
# Predict log(amt) from category + fraud
#========================================
cat("========================================\n")
cat("MODEL 2: LINEAR REGRESSION\n")
cat("Predicting log_amt from category + is_fraud\n")
cat("========================================\n\n")

lm_model <- lm(log_amt ~ category + is_fraud,
               data = fraud_model)

lm_summary <- summary(lm_model)
print(lm_summary)

#key stats
r2      <- round(lm_summary$r.squared, 4)
adj_r2  <- round(lm_summary$adj.r.squared, 4)
rse     <- round(lm_summary$sigma, 4)
f_stat  <- round(lm_summary$fstatistic[1], 2)
p_val_f <- pf(lm_summary$fstatistic[1],
              lm_summary$fstatistic[2],
              lm_summary$fstatistic[3],
              lower.tail=FALSE)

cat("\n--- MODEL FIT ---\n")
cat(sprintf("R-squared         : %.4f\n", r2))
cat(sprintf("Adjusted R-squared: %.4f\n", adj_r2))
cat(sprintf("Residual SE       : %.4f\n", rse))
cat(sprintf("F-statistic       : %.2f\n", f_stat))
cat(sprintf("F p-value         : %s\n\n",
            format(p_val_f, scientific=TRUE)))

# Plot  - LR Diagnoisics 
par(mfrow=c(2,2), mar=c(5,5,4,2))
plot(lm_model,
     main="M2: Linear Regression Diagnostics")
par(mfrow=c(1,1)) #reset 

# Actual vs predicted plot
pred_lm <- predict(lm_model)
resid_lm <- residuals(lm_model)
shap <- shapiro.test(sample(resid_lm, 500))
colors()
par(mfrow=c(1,2), mar=c(5,5,4,2))

plot(pred_lm, resid_lm,
     pch  = 16,
     col  = "lightblue3",
     cex  = 0.5,
     main = "M2: Residuals vs Fitted",
     xlab = "Fitted log(amt)",
     ylab = "Residuals")
abline(h=0, col="red", lwd=2, lty=2)

qqnorm(resid_lm,
       main = "M2: Normal Q-Q Plot",
       col  = "lightblue3",
       pch  = 16, cex=0.5)
qqline(resid_lm, col="red", lwd=2)

par(mfrow=c(1,1)) #

# Category coefficients plot
lm_coefs <- data.frame(
  term  = names(coef(lm_model)),
  coef  = coef(lm_model),
  stringsAsFactors = FALSE) %>%
  filter(grepl("category", term)) %>%
  mutate(
    category = gsub("category","",term),
    coef_exp = exp(coef))

ggplot(lm_coefs,
       aes(x    = reorder(category, coef),
           y    = coef,
           fill = coef)) +
  geom_col(alpha=0.85) +
  geom_hline(yintercept=0,
             lty=2, color="red", lwd=0.8) +
  coord_flip() +
  scale_fill_gradient2(
    low="darkblue", mid="white",
    high="orange1", midpoint=0) +
  labs(title    = "M2: Category Effect on log(Amount)",
       subtitle = "Relative to reference category (home)",
       x        = NULL,
       y        = "Coefficient (log scale)") +
  theme_minimal() +
  theme(legend.position="none",
        plot.title=element_text(
          face="bold",size=14))

#comments
cat("MODEL 2 COMMENTARY\n")
cat("========================================\n\n")

cat("--- MODEL FIT ---\n")
cat(sprintf("R² = %.4f — the model explains %.1f%%\n",
            r2, r2*100))
cat("of variance in log(transaction amount).\n")
cat("Category and fraud status together are moderate\n")
cat("predictors of transaction size.\n\n")

cat("--- FRAUD COEFFICIENT ---\n")
fraud_coef <- coef(lm_model)["is_fraudfraud"]
cat(sprintf("is_fraudfraud coefficient: %.4f\n",
            fraud_coef))
cat(sprintf("exp(coef) = %.4f\n", exp(fraud_coef)))
cat(sprintf("Fraud transactions have amounts %.1fx\n",
            exp(fraud_coef)))
cat("higher than non-fraud on the log scale.\n\n")

cat("--- DIAGNOSTIC INTERPRETATION ---\n")
cat(sprintf("Shapiro-Wilk W=%.4f, p=%.4f\n",
            shap$statistic, shap$p.value))
if (shap$p.value < 0.05) {
  cat("Residuals are NOT perfectly normal — common\n")
  cat("with large datasets where even small deviations\n")
  cat("become significant. The model is still valid\n")
  cat("for descriptive insight purposes.\n")
} else {
  cat("Residuals are approximately normal.\n")
}
cat("Residuals vs Fitted: check for systematic\n")
cat("patterns indicating non-linearity.\n\n")


# ANOVA is used to determine whether the means of a numeric 
# variable differ significantly across two or more groups.
#========================================
# MODEL 3: ONE-WAY ANOVA
# Does log_amt differ across categories?
#========================================
cat("========================================\n")
cat("MODEL 3: ONE-WAY ANOVA\n")
cat("log_amt across transaction categories\n")
cat("========================================\n\n")

anova_model <- aov(log_amt ~ category, # call the aov function 
  data = fraud_model)

anova_summary <- summary(anova_model)
print(anova_summary)
print(anova_summary)
# Df Sum Sq Mean Sq F value Pr(>F)    
# category       13   4896   376.6   179.1 <2e-16 ***
#   Residuals   14369  30210     2.1                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Extract ANOVA stats, F, p val, etc
f_anova  <- anova_summary[[1]]$`F value`[1]
p_anova  <- anova_summary[[1]]$`Pr(>F)`[1]
df_cat   <- anova_summary[[1]]$Df[1]
df_res   <- anova_summary[[1]]$Df[2]
ss_cat   <- anova_summary[[1]]$`Sum Sq`[1]
ss_res   <- anova_summary[[1]]$`Sum Sq`[2]
eta_sq   <- round(ss_cat/(ss_cat+ss_res), 4)

cat(sprintf("\nF(%d, %d) = %.2f, p < 0.001\n",
            df_cat, df_res, f_anova))
cat(sprintf("Eta-squared (η²) = %.4f\n\n", eta_sq))

#--------------------------------------------------------------
#note to self:
# Tukey HSD (Honestly Significant Difference) is a follow-up test 
# performed after ANOVA when ANOVA finds a significant difference.
# Think of it as a two-step process:
#   Step 1: ANOVA
# 
# ANOVA tells you:
#   "At least one group mean is different."
# 
# But it does not tell you which groups differ.

# Step 2: Tukey HSD
# TukeyHSD(anova_model)
# 
# Tukey compares every pair of group means while
# controlling for the increased chance of false positives
# that occurs when making many comparisons.

#--------------------------------------------------------------
# Post-hoc: Tukey HSD
cat("--- TUKEY HSD POST-HOC TEST ---\n")
tukey <- TukeyHSD(anova_model)
tukey_df <- as.data.frame(tukey$category)
tukey_df$comparison <- rownames(tukey_df)
tukey_df$sig <- ifelse(
  tukey_df$`p adj` < 0.001, "***",
  ifelse(tukey_df$`p adj` < 0.01, "**",
         ifelse(tukey_df$`p adj` < 0.05, "*",
                "ns")))

# Show only significant pairs
sig_pairs <- tukey_df[
  tukey_df$`p adj` < 0.05, ]
cat(sprintf("Significant pairs: %d of %d\n\n",
            nrow(sig_pairs),
            nrow(tukey_df)))

# Group means for plot
cat_means <- fraud_model %>%
  group_by(category) %>%
  summarise(
    mean_log_amt = mean(log_amt),
    sd_log_amt   = sd(log_amt),
    n            = n(),
    se           = sd_log_amt/sqrt(n),
    .groups      = "drop") %>%
  arrange(desc(mean_log_amt))

# Boxplot by category
ggplot(fraud_model,
       aes(x    = reorder(category,
                          log_amt,
                          FUN=mean),
           y    = log_amt,
           fill = category)) +
  geom_boxplot(alpha  =0.7,
               outlier.size=0.3,
               outlier.alpha=0.3) +
  stat_summary(fun=mean, geom="point",
               pch=18, size=2.5,
               col="black") +
  coord_flip() +
  scale_fill_viridis_d(option="plasma") +
  scale_y_continuous(
    breaks=log(c(1,10,50,100,500,1000)),
    labels=paste0("$",
                  c(1,10,50,100,500,1000))) +
  labs(title    = "M3: log(Amount) by Category",
       subtitle = paste0("One-way ANOVA: F(",
                         df_cat,",",df_res,
                         ")=",round(f_anova,1),
                         ", p<0.001, η²=",
                         eta_sq),
       x        = NULL,
       y        = "Transaction Amount (log scale)") +
  theme_minimal() +
  theme(legend.position="none",
        plot.title=element_text(
          face="bold",size=14))

#very interesting, Comments
cat("\n========================================\n")
cat("MODEL 3 COMMENTARY\n")
cat("========================================\n\n")

cat("--- ANOVA RESULT ---\n")
cat(sprintf("F(%d,%d) = %.2f, p < 0.001\n",
            df_cat, df_res, f_anova))
cat("The result is highly significant — transaction\n")
cat("amounts differ significantly across the 14\n")
cat("spending categories.\n\n")

cat("--- EFFECT SIZE ---\n")
cat(sprintf("Eta-squared η² = %.4f\n", eta_sq))
if (eta_sq >= 0.14) {
  cat("LARGE effect size (η² >= 0.14)\n")
} else if (eta_sq >= 0.06) {
  cat("MEDIUM effect size (η² >= 0.06)\n")
} else {
  cat("SMALL effect size (η² < 0.06)\n")
}
cat("Category membership explains a meaningful\n")
cat("proportion of variance in transaction size.\n\n")

cat("--- CATEGORY MEANS ---\n")
cat(sprintf("%-22s  %10s  %10s\n",
            "Category",
            "Mean log(amt)",
            "Mean $ (approx)"))
cat(strrep("-", 46), "\n")
for (i in 1:nrow(cat_means)) {
  cat(sprintf("%-22s  %10.4f  %10.2f\n",
              as.character(cat_means$category[i]),
              cat_means$mean_log_amt[i],
              exp(cat_means$mean_log_amt[i])))
}

cat("\n--- TUKEY HSD INTERPRETATION ---\n")
cat(sprintf("%d of %d pairwise comparisons\n",
            nrow(sig_pairs),
            nrow(tukey_df)))
cat("are statistically significant.\n")
cat("The high-spending categories (shopping_net,\n")
cat("misc_net) differ significantly from low-\n")
cat("spending categories (personal_care, health).\n\n")

cat("--- ANOVA ASSUMPTIONS ---\n")
# Check normality of residuals
resid_anova <- residuals(anova_model)
shap_anova  <- shapiro.test(
  sample(resid_anova, min(500,
                          length(resid_anova))))
cat(sprintf("Shapiro-Wilk: W=%.4f, p=%.4f\n",
            shap_anova$statistic,
            shap_anova$p.value))
cat("With n=14,383, ANOVA is robust to\n")
cat("non-normality due to the Central Limit\n")
cat("Theorem — results remain valid.\n\n")

# Diagnostic plots
par(mfrow=c(1,2), mar=c(5,5,4,2))
plot(anova_model, which=1,
     main="M3: ANOVA Residuals vs Fitted")
plot(anova_model, which=2,
     main="M3: ANOVA Q-Q Plot")
par(mfrow=c(1,1))

#kmeans

# MODEL 4: K-MEANS CLUSTERING
# Cluster transactions by risk profile
#========================================
cat("========================================\n")
cat("MODEL 4: K-MEANS CLUSTERING\n")
cat("Cluster transactions by risk profile\n")
cat("========================================\n\n")

# Select and scale features
cluster_vars <- c("log_amt", "is_night",
                  "category_fraud_rate",
                  "age", "risk_score")

cluster_data <- fraud_model[, cluster_vars]
cluster_data <- cluster_data[
  complete.cases(cluster_data), ]

# Scale all variables
cluster_scaled <- scale(cluster_data)

# Elbow method — find optimal k
cat("--- ELBOW METHOD (k=1 to 8) ---\n")
set.seed(67)
wcss <- sapply(1:8, function(k) {
  km <- kmeans(cluster_scaled,
               centers = k,
               nstart  = 25)
  km$tot.withinss
})

elbow_df <- data.frame(
  k    = 1:8,
  wcss = wcss)

ggplot(elbow_df,
       aes(x=k, y=wcss)) +
  geom_line(color="purple", lwd=1.2) +
  geom_point(color="coral", size=3) +
  geom_vline(xintercept=3,
             lty=2, color="red",
             lwd=0.8) +
  annotate("text", x=3.2, y=max(wcss)*0.9,
           label="Chosen k=3",
           color="red", size=3.5,
           hjust=0) +
  scale_x_continuous(breaks=1:8) +
  labs(title    = "M4: Elbow Method — Optimal k",
       subtitle = "Bend at k=3 — diminishing returns beyond",
       x        = "Number of clusters (k)",
       y        = "Total within-cluster SS") +
  theme_minimal() +
  theme(plot.title=element_text(
    face="bold", size=14))

# Fit k=3
set.seed(67)
km3 <- kmeans(cluster_scaled,
              centers = 3,
              nstart  = 25)

# Attach clusters
fraud_model$cluster <- factor(km3$cluster)

cat("Cluster sizes:\n")
print(table(fraud_model$cluster))

# Cluster profiles
cluster_profile <- fraud_model %>%
  group_by(cluster) %>%
  summarise(
    n              = n(),
    mean_amt       = round(mean(amt), 2),
    mean_log_amt   = round(mean(log_amt), 4),
    pct_night      = round(mean(is_night)*100,1),
    mean_cat_rate  = round(
      mean(category_fraud_rate)*100, 2),
    mean_age       = round(mean(age), 1),
    mean_risk      = round(mean(risk_score), 3),
    fraud_rate     = round(
      mean(is_fraud=="fraud")*100, 2),
    .groups        = "drop")

cat("\n--- CLUSTER PROFILES ---\n")
cat(sprintf("%-10s  %6s  %8s  %8s  %8s  %8s  %8s\n",
            "Cluster", "n",
            "Mean$", "Night%",
            "CatRate%", "Fraud%",
            "RiskScore"))
cat(strrep("-", 68), "\n")
for (i in 1:nrow(cluster_profile)) {
  cat(sprintf("%-10s  %6d  %8.2f  %8.1f  %8.2f  %8.2f  %8.3f\n",
              as.character(
                cluster_profile$cluster[i]),
              cluster_profile$n[i],
              cluster_profile$mean_amt[i],
              cluster_profile$pct_night[i],
              cluster_profile$mean_cat_rate[i],
              cluster_profile$fraud_rate[i],
              cluster_profile$mean_risk[i]))
}

# Label clusters by fraud rate
cluster_labels <- cluster_profile %>%
  arrange(fraud_rate) %>%
  mutate(label = c("Low Risk",
                   "Medium Risk",
                   "High Risk"))

label_map <- setNames(
  cluster_labels$label,
  as.character(cluster_labels$cluster))

fraud_model$cluster_label <- factor(
  label_map[as.character(
    fraud_model$cluster)],
  levels = c("Low Risk",
             "Medium Risk",
             "High Risk"))

# Cluster visualisation
par(mfrow=c(1,2), mar=c(5,5,4,2))
colors()

# Plot: log_amt by cluster
boxplot(log_amt ~ cluster_label,
        data    = fraud_model,
        main    = "M4: log(Amount) by Cluster",
        xlab    = "Cluster",
        ylab    = "Amount (log scale)",
        col     = c("skyblue3",
                    "thistle3",
                    "tomato"),
        border  = "black",
        outline = FALSE)
axis(4,
     at     = log(c(1,10,100,1000)),
     labels = paste0("$",
                     c(1,10,100,1000)),
     las    = 2)

# Plot: fraud rate by cluster
fraud_rate_cl <- tapply(
  fraud_model$is_fraud=="fraud",
  fraud_model$cluster_label,
  mean) * 100

barplot(fraud_rate_cl,
        main   = "M4: Fraud Rate by Cluster",
        xlab   = "Cluster",
        ylab   = "Fraud Rate (%)",
        col    = c("skyblue3",
                   "thistle3",
                   "tomato"),
        border = "white",
        ylim   = c(0,
                   max(fraud_rate_cl)*1.2))
text(x      = c(0.7, 1.9, 3.1),
     y      = fraud_rate_cl + 1,
     labels = paste0(round(fraud_rate_cl,1),"%"),
     font   = 2, cex=0.9)

par(mfrow=c(1,1))

# Scatterplot: log_amt vs is_night colored by cluster
ggplot(fraud_model %>%
         sample_n(2000),
       aes(x     = jitter(is_night, 0.3),
           y     = log_amt,
           color = cluster_label)) +
  geom_point(alpha=0.4, size=1.2) +
  scale_color_manual(
    values = c("Low Risk"    = "steelblue",
               "Medium Risk" = "gold3",
               "High Risk"   = "coral")) +
  scale_y_continuous(
    breaks = log(c(1,10,100,1000)),
    labels = paste0("$",
                    c(1,10,100,1000))) +
  scale_x_continuous(
    breaks = c(0,1),
    labels = c("Day","Night")) +
  facet_wrap(~cluster_label) +
  labs(title    = "M4: Cluster Profiles — Amount vs Time of Day",
       subtitle = "Sample of 2,000 transactions",
       x        = "Time of Day",
       y        = "Amount (log scale)",
       color    = "Cluster") +
  theme_minimal() +
  theme(legend.position="none",
        plot.title=element_text(
          face="bold",size=14))
#pretty cool

# comments / intepretation
cat("\n========================================\n")
cat("MODEL 4 COMMENTARY\n")
cat("========================================\n\n")

cat("--- CLUSTER INTERPRETATION ---\n\n")

for (i in 1:nrow(cluster_labels)) {
  cl  <- cluster_labels$cluster[i]
  lbl <- cluster_labels$label[i]
  cp  <- cluster_profile[
    cluster_profile$cluster==cl, ]
  
  cat(sprintf("CLUSTER %s — %s (n=%d, %.1f%%)\n",
              cl, lbl, cp$n,
              cp$n/nrow(fraud_model)*100))
  cat(sprintf("  Fraud rate    : %.2f%%\n",
              cp$fraud_rate))
  cat(sprintf("  Mean amount   : $%.2f\n",
              cp$mean_amt))
  cat(sprintf("  Night txns    : %.1f%%\n",
              cp$pct_night))
  cat(sprintf("  Category rate : %.2f%%\n",
              cp$mean_cat_rate))
  cat(sprintf("  Mean risk score: %.3f\n\n",
              cp$mean_risk))
}

cat("--- K-MEANS QUALITY ---\n")
bss_ratio <- round(km3$betweenss / km3$totss * 100, 2)
cat(sprintf("Between-SS / Total-SS: %.2f%%\n",
            bss_ratio))
cat("Higher ratio = better cluster separation.\n\n")

cat("--- PRACTICAL IMPLICATION ---\n")
high_cl <- cluster_labels$cluster[
  cluster_labels$label=="High Risk"]
high_n  <- cluster_profile$n[
  cluster_profile$cluster==high_cl]
high_fr <- cluster_profile$fraud_rate[
  cluster_profile$cluster==high_cl]
cat(sprintf("The High Risk cluster (%d transactions)\n",
            high_n))
cat(sprintf("has a %.1f%% fraud rate.\n", high_fr))
cat("Flagging this cluster for review would catch\n")
cat("a disproportionate share of fraud cases while\n")
cat("limiting the number of false positives.\n\n")

#overall modeling summmary thus far 
#========================================
# MODELING REPORT SUMMARY
#========================================
cat("========================================\n")
cat("MODELING REPORT — FINAL SUMMARY\n")
cat("========================================\n\n")

cat(sprintf("%-6s  %-30s  %-20s  %s\n",
            "Model", "Method",
            "Key Metric", "Finding"))
cat(strrep("-", 80), "\n")
cat(sprintf("%-6s  %-30s  %-20s  %s\n",
            "M1",
            "Logistic Regression",
            paste0("McFadden R²=",
                   mcfadden_r2),
            paste0("Recall=",recall,"%")))
cat(sprintf("%-6s  %-30s  %-20s  %s\n",
            "M2",
            "Linear Regression",
            paste0("R²=",r2),
            "Fraud txns 7x larger"))
cat(sprintf("%-6s  %-30s  %-20s  %s\n",
            "M3",
            "One-Way ANOVA",
            paste0("F=",round(f_anova,1),
                   " η²=",eta_sq),
            "Amt differs by category"))
cat(sprintf("%-6s  %-30s  %-20s  %s\n",
            "M4",
            "K-Means (k=3)",
            paste0("BSS/TSS=",bss_ratio,"%"),
            "3 clear risk tiers"))

cat("\n--- TOP FINDINGS ACROSS ALL MODELS ---\n\n")
cat("1. TRANSACTION AMOUNT is the dominant predictor\n")
cat("   of fraud across ALL four models. The logistic\n")
cat("   model confirms its significance (p<0.001),\n")
cat("   linear regression shows fraud transactions\n")
cat("   are 7x larger, and clustering separates\n")
cat("   transactions primarily by amount.\n\n")
cat("2. TIME OF DAY (is_night) is the second\n")
cat("   strongest predictor — night transactions\n")
cat("   have dramatically higher fraud odds.\n\n")
cat("3. CATEGORY matters — ANOVA confirms significant\n")
cat("   amount differences (F=179, η²=significant),\n")
cat("   and category_fraud_rate is a meaningful\n")
cat("   logistic regression predictor.\n\n")
cat("4. K-MEANS reveals 3 natural transaction tiers:\n")
cat("   Low Risk (day, small amounts),\n")
cat("   Medium Risk (mixed), and\n")
cat("   High Risk (night, large amounts,\n")
cat("   high-risk categories).\n\n")
cat("5. AGE and DISTANCE are weak but present —\n")
cat("   age is significant in logistic regression\n")
cat("   but the effect size is small. Distance\n")
cat("   shows near-zero correlation with fraud.\n\n")


cat(" Now onto Step 8, almost at the promise land\n")

#--------------- Step 8
#Visualization Design & Data Storytelling task

# STEP 8: VISUALIZATION DESIGN & DATA STORYTELLING
# Redesign early exploratory plots into polished, report-ready graphics
#========================================

# library(ggplot2)
# library(dplyr)
# library(scales)
# library(lubridate)
# Add some consistency accross plots

#========================================
# Step 8.1 DESIGN SYSTEM  
# Consistent colors, fonts, and theme used across ALL report graphics
#========================================

# --- Brand color palette ---
col_fraud     <- "#C0392B"       # strong red   — fraud
col_not_fraud <- "#2980B9"       # strong blue  — not fraud
col_night     <- "#1A1A2E"       # near-black   — night
col_day       <- "#F4D03F"       # gold         — day
col_high      <- "#E74C3C"       # accent red   — high risk
col_medium    <- "#F39C12"       # amber        — medium risk
col_low       <- "#27AE60"       # green        — low risk
col_neutral   <- "#7F8C8D"       # grey         — neutral

# --- Reusable report theme ---
theme_report <- function() {
  theme_minimal(base_size = 13) +
    theme(
      # Titles
      plot.title      = element_text(
        face   = "bold",
        size   = 16,
        color  = "#1A1A2E",
        margin = margin(b=6)),
      plot.subtitle   = element_text(
        size   = 12,
        color  = "#555555",
        margin = margin(b=10)),
      plot.caption    = element_text(
        size   = 9,
        color  = "#999999",
        hjust  = 1,
        margin = margin(t=10)),
      # Axes
      axis.title      = element_text(
        size   = 11,
        color  = "#333333"),
      axis.text       = element_text(
        size   = 10,
        color  = "#444444"),
      # Grid
      panel.grid.major = element_line(
        color = "#EEEEEE",
        linewidth = 0.4),
      panel.grid.minor = element_blank(),
      # Legend
      legend.title    = element_text(
        size   = 10,
        face   = "bold"),
      legend.text     = element_text(
        size   = 10),
      legend.position = "bottom",
      # Background
      plot.background = element_rect(
        fill  = "white",
        color = NA),
      plot.margin     = margin(
        15, 15, 10, 15))
}

cat("STEP 8.2: VISUALIZATION REDESIGN\n")
cat("10 Report-Ready Graphics\n")
cat("========================================\n\n")

#========================================
# GRAPHIC 1 (Redesign of Univariate V1)
# Class Imbalance — Hero chart
# Original: basic barplot
# Redesign: annotated ggplot with
#           context and percentages
#========================================
cat("--- Graphic 1: Class Imbalance ---\n\n")

fraud_counts <- fraud %>%
  count(is_fraud) %>%
  mutate(
    pct   = n / sum(n) * 100,
    label_top = paste0(
      format(n, big.mark=","), "\n transactions"),
    label_pct = paste0(round(pct,1), "%"),
    status = ifelse(is_fraud=="fraud",
                    "Fraud", "Not Fraud"))

g1 <- ggplot(fraud_counts,
             aes(x    = status,
                 y    = n,
                 fill = status)) +
  geom_col(width = 0.55,
           alpha = 0.92) +
  # Count label inside bar
  geom_text(aes(label = format(n, big.mark=",")),
            vjust    = 1.4,
            size     = 5,
            fontface = "bold",
            color    = "white") +
  # Percentage label above bar
  geom_text(aes(label = paste0(round(pct,1),"%")),
            vjust    = -0.5,
            size     = 4.5,
            fontface = "bold",
            color    = c(col_fraud,
                         col_not_fraud)) +
  # Annotation callout
  annotate("segment",
           x=1.4, xend=1.05,
           y=9800, yend=1900,
           color  = col_fraud,
           lwd    = 0.7,
           arrow  = arrow(length=unit(0.2,"cm"))) +
  annotate("label",
           x     = 1.6,
           y     = 10200,
           label = "Only 12.4% of\ntransactions\nare fraud",
           color = col_fraud,
           fill  = "#FFF5F5",
           size  = 3.5,
           label.padding = unit(0.4,"lines"),
           label.r       = unit(0.3,"lines")) +
  scale_fill_manual(
    values = c("Fraud"     = col_fraud,
               "Not Fraud" = col_not_fraud)) +
  scale_y_continuous(
    labels   = label_comma(),
    expand   = expansion(mult=c(0, 0.18))) +
  labs(
    title   = "Fraud is Rare but Costly",
    subtitle= paste0(
      "Only 1,782 of 14,383 transactions are fraudulent (12.4%)\n",
      "Class imbalance means accuracy alone is a misleading metric"),
    x       = NULL,
    y       = "Number of Transactions",
    caption = "Source: Fraud Detection Dataset | Step 8 — Visualization Design") +
  theme_report() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank())

print(g1)

#========================================
# GRAPHIC 2 (Redesign of Univariate V2+V3)
# Transaction Amount — Before & After
# Original: separate histograms
# Redesign: side-by-side with annotation
#           explaining WHY log is needed
#========================================
cat("--- Graphic 2: Amount Distribution ---\n\n")

amt_raw <- ggplot(fraud,             #RAW
                  aes(x=amt)) +
  geom_histogram(bins  = 50,
                 fill  = col_not_fraud,
                 color = "white",
                 alpha = 0.85) +
  geom_vline(xintercept = mean(fraud$amt),
             color=col_fraud, lwd=1, lty=2) +
  geom_vline(xintercept = median(fraud$amt),
             color=col_low, lwd=1, lty=3) +
  annotate("text",
           x=mean(fraud$amt)+120, y=3200,
           label=paste0("Mean\n$",
                        round(mean(fraud$amt))),
           color=col_fraud, size=3.5,
           fontface="bold", hjust=0) +
  annotate("text",
           x=median(fraud$amt)+910, y=4500,
           label=paste0("Median\n$",
                        round(median(fraud$amt))),
           color=col_low, size=3.5,
           fontface="bold", hjust=1) +
  scale_x_continuous(labels=label_dollar()) +
  scale_y_continuous(labels=label_comma()) +
  labs(title    = "Raw Scale",
       subtitle = "Right-skewed — hard to read",
       x="Amount ($)", y="Frequency") +
  theme_report() +
  theme(plot.title=element_text(size=13))


amt_log <- ggplot(fraud,           #LOG
                  aes(x=log_amt)) +
  geom_histogram(bins  = 40,
                 fill  = col_low,
                 color = "white",
                 alpha = 0.85) +
  geom_vline(xintercept=mean(fraud$log_amt),
             color=col_fraud, lwd=1, lty=2) +
  geom_vline(xintercept=median(fraud$log_amt),
             color="#1A1A2E", lwd=1, lty=3) +
  annotate("text",
           x=mean(fraud$log_amt)-01.50,
           y=1150,
           label=paste0("Mean\n$", round(mean(fraud$amt))),
           color=col_fraud, size=3.5,
           fontface="bold", hjust=0) +
  annotate("text",
           x=median(fraud$log_amt)+02.50,
           y=1150,
           label=paste0("Median\n$", round(median(fraud$amt))),
           color="#1A1A2E", size=3.5,
           fontface="bold", hjust=1) +
  scale_x_continuous(
    breaks = log(c(1,5,10,50,
                   100,500,1000)),
    labels = paste0("$",
                    c(1,5,10,50,
                      100,500,1000))) +
  scale_y_continuous(labels=label_comma()) +
  labs(title    = "Log Scale",
       subtitle = "Near-normal — clear shape",
       x="Amount (log scale)", y="Frequency") +
  theme_report() +
  theme(plot.title=element_text(size=13))

# Combine using patchwork or cowplot
# need to install package
install.packages("patchwork")
library(patchwork)
g2 <- (amt_raw | amt_log) +
  plot_annotation(
    title   = "Why We Log-Transform Transaction Amount",
    subtitle= paste0(
      "Raw scale hides the distribution — ",
      "log scale reveals two natural spending clusters"),
    caption = "Source: Fraud Detection Dataset | Step 8",
    theme   = theme_report())

print(g2)
#ok not too bad 

# Use / instead of | to stack top and bottom
# g2 <- (amt_raw / amt_log) +
#   plot_annotation(
#     title    = "Why We Log-Transform Transaction Amount",
#     subtitle = paste0(
#       "Raw scale hides the distribution — ",
#       "log scale reveals two natural spending clusters"),
#     caption  = "Source: Fraud Detection Dataset | Step 8",
#     theme    = theme_report())
# print(g2)


# its cool but the raw scale is. lil cramped, and log sclaes x axis are also cramped


#========================================
# GRAPHIC 3 (Redesign of Univariate V6)
# Hour of Day — with fraud rate overlay
# Original: simple volume bar chart
# Redesign: dual-axis storytelling chart showing volume + fraud rate
#========================================
cat("--- Graphic 3: Hour of Day ---\n\n")

hour_data <- fraud %>%
  group_by(trans_hour) %>%
  summarise(
    volume     = n(),
    fraud_rate = mean(is_fraud=="fraud")*100,
    .groups    = "drop") %>%
  mutate(
    is_high_risk = trans_hour %in% c(22,23,0,1,2,3),
    period = case_when(
      trans_hour >= 22 | trans_hour <= 3 ~
        "High Risk Night (22:00–03:00)",
      TRUE ~ "Normal Hours"))

g3 <- ggplot(hour_data,
             aes(x = trans_hour)) +
  # Volume bars
  geom_col(aes(y    = volume,
               fill = period),
           alpha = 0.75) +
  # Fraud rate line (scaled to volume axis)
  geom_line(aes(y = fraud_rate * 150),
            color = col_fraud,
            lwd   = 1.5) +
  geom_point(aes(y = fraud_rate * 150),
             color = col_fraud,
             size  = 2.5) +
  # Annotate worst hours
  annotate("rect",
           xmin=21.5, xmax=23.5,
           ymin=0, ymax=Inf,
           fill=col_fraud, alpha=0.08) +
  annotate("text",
           x=22.5, y=800,
           label="~41%\nfraud\nrate",
           color=col_fraud, size=3.5,
           fontface="bold") +
  annotate("text",
           x=12, y=1000,
           label="Fraud rate (red line)",
           color=col_fraud, size=3.5,
           hjust=0.5) +
  scale_fill_manual(
    values=c(
      "High Risk Night (22:00–03:00)" = "#E74C3C",
      "Normal Hours"                  = col_not_fraud)) +
  scale_x_continuous(
    breaks = seq(0,23,by=2),
    labels = paste0(seq(0,23,by=2),":00")) +
  scale_y_continuous(
    name   = "Transaction Volume",
    labels = label_comma(),
    sec.axis = sec_axis(
      transform = ~./150,
      name   = "Fraud Rate (%)",
      labels = function(x) paste0(x,"%"))) +
  labs(
    title   = "The Night-Fraud Effect",
    subtitle= paste0(
      "Late-night transactions (22:00–03:00) have a 41% fraud rate ",
      "— 10× higher than daytime\n",
      "Volume drops at night, but fraud spikes dramatically"),
    x       = "Hour of Day",
    caption = "Source: Fraud Detection Dataset | Step 8",
    fill    = "Risk Period") +
  theme_report() +
  theme(
    axis.text.x  = element_text(
      angle=45, hjust=1),
    axis.title.y.right = element_text(
      color=col_fraud),
    axis.text.y.right  = element_text(
      color=col_fraud))

print(g3)
# one of my favorites

#========================================
# GRAPHIC 4 (Redesign of Bivariate V1)
# Amount by Fraud Status
# Original: basic violin + boxplot
# Redesign: annotated split violin with clear story labels
#========================================
cat("--- Graphic 4: Amount by Fraud ---\n\n")

fraud_amt_stats <- fraud %>%
  group_by(is_fraud) %>%
  summarise(
    mean_amt   = mean(amt),
    median_amt = median(amt),
    .groups    = "drop")

g4 <- ggplot(fraud,
             aes(x    = is_fraud,
                 y    = log_amt,
                 fill = is_fraud)) +
  geom_violin(trim  = FALSE,
              alpha = 0.75,
              lwd   = 0.3) +
  geom_boxplot(width   = 0.1,
               fill    = "white",
               alpha   = 0.85,
               lwd     = 0.5,
               outlier.size  = 0.8,
               outlier.alpha = 0.3) +
  stat_summary(fun=mean, geom="point",
               pch=18, size=4,
               color="black") +
  # 7.7x annotation
  annotate("segment",
           x=1, xend=2,
           y=7.8, yend=7.8,
           color="#333333", lwd=0.8,
           arrow=arrow(ends="both",
                       length=unit(0.15,"cm"))) +
  annotate("label",
           x=1.5, y=8.1,
           label="Fraud transactions\nare 7.7× larger on average",
           fill="#FFF9E6",
           color="#8B4513",
           size=3.8,
           fontface="bold",
           label.padding=unit(0.4,"lines"),
           label.r=unit(0.3,"lines")) +
  # Mean labels
  annotate("text",
           x=1.4, y=log(fraud_amt_stats$mean_amt[fraud_amt_stats$is_fraud=="not_fraud"])+0.15,
           label=paste0("Mean $",
                        round(fraud_amt_stats$mean_amt[fraud_amt_stats$is_fraud=="not_fraud"])),
           color=col_not_fraud, size=3.5,
           fontface="bold") +
  annotate("text",
           x=2.4, y=log(fraud_amt_stats$mean_amt[fraud_amt_stats$is_fraud=="fraud"])+0.15,
           label=paste0("Mean $",
                        round(fraud_amt_stats$mean_amt[fraud_amt_stats$is_fraud=="fraud"])),
           color=col_fraud, size=3.5,
           fontface="bold") +
  scale_fill_manual(
    values = c("not_fraud" = col_not_fraud,
               "fraud"     = col_fraud),
    labels = c("Not Fraud", "Fraud")) +
  scale_x_discrete(
    labels = c("not_fraud" = "Not Fraud\n(n=12,601)",
               "fraud"     = "Fraud\n(n=1,782)")) +
  scale_y_continuous(
    breaks = log(c(1,5,10,50,100,500,1000,3000)),
    labels = paste0("$",
                    c(1,5,10,50,100,500,1000,3000))) +
  labs(
    title   = "Transaction Amount is the #1 Fraud Signal",
    subtitle= paste0(
      "Fraud transactions average $518 vs $67 for legitimate ones\n",
      "The distribution shift is visible across the entire range"),
    x       = NULL,
    y       = "Transaction Amount",
    caption = "Source: Fraud Detection Dataset | Step 8",
    fill    = NULL) +
  theme_report() +
  theme(legend.position = "none")

print(g4)

#========================================
# GRAPHIC 5 (Redesign of Bivariate V2)
# Fraud Rate by Category
# Original: basic horizontal bar
# Redesign: lollipop chart with risk tier colour bands
#========================================
cat("--- Graphic 5: Fraud Rate by Category ---\n\n")

avg_fr <- mean(fraud$is_fraud == "fraud") * 100

cat_fraud_g5 <- fraud %>%
  group_by(category) %>%
  summarise(
    n          = n(),
    fraud_rate = mean(is_fraud == "fraud") * 100,
    .groups    = "drop") %>%
  arrange(fraud_rate) %>%
  mutate(
    category  = factor(category,
                       levels = category),
    risk_tier = case_when(
      fraud_rate > 20     ~ "High Risk",
      fraud_rate > avg_fr ~ "Above Average",
      TRUE                ~ "Below Average"),
    risk_tier = factor(
      risk_tier,
      levels = c("High Risk",
                 "Above Average",
                 "Below Average")))

# Get the first and last category labels
# for annotate text positions
first_cat <- levels(cat_fraud_g5$category)[1]
last_cat  <- levels(cat_fraud_g5$category)[
  nlevels(cat_fraud_g5$category)]

g5 <- ggplot(cat_fraud_g5,
             aes(x     = category,
                 y     = fraud_rate,
                 color = risk_tier)) +

  # Background bands — use fill on geom_rect
  # instead of annotate for factor x-axis
  geom_rect(aes(xmin = -Inf, xmax = Inf,
                ymin = 20,   ymax = Inf),
            fill  = col_fraud,
            alpha = 0.02,
            color = NA,
            inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf,
                ymin = avg_fr, ymax = 20),
            fill  = col_medium,
            alpha = 0.02,
            color = NA,
            inherit.aes = FALSE) +

  # Average reference line
  geom_hline(yintercept = avg_fr,
             lty   = 2,
             color = col_neutral,
             lwd   = 0.8) +

  # Lollipop sticks
  geom_segment(aes(xend = category,
                   y    = 0,
                   yend = fraud_rate),
               lwd   = 1.2,
               alpha = 0.7) +

  # Lollipop heads
  geom_point(size = 5) +

  # Value labels
  geom_text(aes(label = paste0(
                  round(fraud_rate, 1), "%")),
            hjust    = -0.3,
            size     = 3.5,
            fontface = "bold") +

  coord_flip() +

  # Average label — use first_cat as x position
  # after coord_flip x becomes y axis
  annotate("text",
           x     = first_cat,
           y     = avg_fr + 0.5,
           label = paste0("Dataset avg: ",
                          round(avg_fr, 1), "%"),
           color = col_neutral,
           size  = 3.2,
           hjust = 0) +

  # this label is too high up and looks bad
  # High risk zone label — use last_cat
  # ↓ FIX 1: Move "High Risk Zone" label DOWN
  # Changed x from last_cat to a middle category
  # so it sits below the shopping_net percentage
  # Use the 3rd category from the top (misc_net)
  # to keep it inside the red band but not overlapping
  annotate("text",
           x        = levels(cat_fraud_g5$category)[
             nlevels(cat_fraud_g5$category) - 3],
           y        = 26,                           #26 is a good spot
           label    = "High Risk Zone (>20%)",
           color    = col_fraud,
           size     = 3.2,
           fontface = "bold.italic",
           hjust    = 0.5) +
  
  scale_color_manual(
    values = c("High Risk"     = col_fraud,
               "Above Average" = col_medium,
               "Below Average" = col_low)) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.22)),
    labels = function(x) paste0(x, "%")) +

  
  
  labs(
    title    = "Where Does Fraud Actually Happen?",
    subtitle = paste0(
      "Shopping and grocery categories have fraud rates ",
      "above 27% — more than double the average\n",
      "Home and health categories show the lowest fraud risk"),
    x        = NULL,
    y        = "Fraud Rate (%)",
    color    = "Risk Tier",
    caption  = "Source: Fraud Detection Dataset | Step 8") +
  theme_report() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position    = "right")

print(g5)

#========================================
# GRAPHIC 6 (Redesign of Bivariate V8)
# Heatmap — Hour × Category
# Original: basic geom_tile
# Redesign: polished heatmap with clear labels and story
#========================================
cat("--- Graphic 6: Hour x Category Heatmap ---\n\n")

# Reorder categories by overall fraud rate
cat_order_g6 <- fraud %>%
  group_by(category) %>%
  summarise(fr=mean(is_fraud=="fraud"),
            .groups="drop") %>%
  arrange(fr) %>%
  pull(category)

hour_cat_g6 <- fraud %>%
  group_by(trans_hour, category) %>%
  summarise(
    fraud_rate = mean(is_fraud=="fraud")*100,
    n          = n(),
    .groups    = "drop") %>%
  mutate(category = factor(category,
                           levels=cat_order_g6))

g6 <- ggplot(hour_cat_g6,
             aes(x    = trans_hour,
                 y    = category,
                 fill = fraud_rate)) +
  
  geom_tile(color = "white",
            lwd   = 0.4) +
  
  # Night region outline — kept as is
  annotate("rect",
           xmin  = 21.5,
           xmax  = 23.5,
           ymin  = 0.5,
           ymax  = 14.5,
           fill  = NA,
           color = col_fraud,
           lwd   = 1.2,
           lty   = 1) +
  
  annotate("text",
           x        = 22.5,         # centred on 22-23 window
           y        = 15,         # ← moved DOWN inside plot
           label    = "Worst window",
           color    = col_fraud,
           size     = 2,
           fontface = "bold",
           hjust    = 0.5,
           vjust    = 1) +
  
  scale_fill_gradient2(
    low      = "#2980B9",
    mid      = "#F8F9FA",
    high     = "#C0392B",
    midpoint = avg_fr,
    name     = "Fraud\nRate (%)",
    labels   = function(x) paste0(x, "%")) +
  
  scale_x_continuous(
    breaks = seq(0, 23, by = 2),
    labels = paste0(seq(0, 23, by = 2), ":00"),
    expand = expansion(mult = c(0.01, 0.05))) +
  
  scale_y_discrete(
    expand = expansion(add = c(0.5, 1.2))) +
  
  labs(
    title    = "The Double Threat: Night Hours + High-Risk Category",
    subtitle = paste0(
      "Red cells = highest fraud probability  |  ",
      "Blue cells = lowest fraud probability\n",
      "The worst transactions are online shopping ",
      "between 22:00–23:00"),
    x        = "Hour of Day",
    y        = NULL,
    caption  = "Source: Fraud Detection Dataset | Step 8") +
  
  theme_report() +
  theme(
    axis.text.x       = element_text(
      angle = 45,
      hjust = 1),
    axis.text.y       = element_text(size = 10),
    panel.grid        = element_blank(),
    legend.position   = "right",
    legend.key.height = unit(1.5, "cm"),
    

    plot.margin       = margin(
      t = 15,    # ← extra top
      r = 25,    # ← extra right
      b = 10,
      l = 15))

print(g6)
#like this one

#========================================
# GRAPHIC 7 (Redesign of Bivariate V6)
# Fraud Rate by State
# Original: horizontal bar chart
# Redesign: dot plot with context
#========================================
cat("--- Graphic 7: Fraud Rate by State ---\n\n")

state_g7 <- fraud %>%
  group_by(state) %>%
  summarise(
    n          = n(),
    fraud_rate = mean(is_fraud == "fraud") * 100,
    .groups    = "drop") %>%
  arrange(fraud_rate) %>%
  mutate(
    state     = factor(state, levels = state),
    above_avg = fraud_rate > avg_fr,
    
    # Put state and n on ONE line separated by a space instead of \n
    # This prevents the two-line stacking
    state_label = paste0(state,
                         "  (n=",
                         format(n, big.mark = ","),
                         ")"))

g7 <- ggplot(state_g7,
             aes(x     = fraud_rate,
                 y     = factor(
                   state_label,
                   levels = state_label))) +
  
  # Average reference line
  geom_vline(xintercept = avg_fr,
             lty   = 2,
             color = col_neutral,
             lwd   = 0.8) +
  
  # Segments from avg to point
  geom_segment(aes(x    = avg_fr,
                   xend = fraud_rate,
                   yend = factor(
                     state_label,
                     levels = state_label),
                   color = above_avg),
               lwd   = 1.5,
               alpha = 0.7) +
  
  geom_point(aes(color = above_avg),
             size = 4) +
  
  geom_text(aes(label = paste0(
    round(fraud_rate, 1), "%")),
    hjust    = -0.35,
    size     = 3.5,
    fontface = "bold") +
  
  scale_color_manual(
    values = c("TRUE"  = col_fraud,
               "FALSE" = col_low),
    labels = c("TRUE"  = "Above average",
               "FALSE" = "Below average")) +
  
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.05, 0.18))) +
  

  # add = c(bottom, top) padding in row units
  scale_y_discrete(
    expand = expansion(add = c(0.8, 0.8))) +
  
  annotate("text",
           x     = avg_fr + 0.3,
           y     = 0.6,
           label = paste0("Avg: ",
                          round(avg_fr, 1), "%"),
           color = col_neutral,
           size  = 3.2,
           hjust = 0) +
  
  labs(
    title    = "Geographic Fraud Variation Across 13 States",
    subtitle = paste0(
      "Alaska (AK) has a 31.7% fraud rate",
      " — nearly 3× the average\n",
      "Small sample sizes in some states",
      " may inflate rates"),
    x        = "Fraud Rate (%)",
    y        = NULL,
    color    = "vs Average",
    caption  = "Source: Fraud Detection Dataset | Step 8") +
  
  theme_report() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position    = "right",
    axis.text.y  = element_text(
      size    = 10,
      margin  = margin(r = 5)),
    plot.margin  = margin(
      t = 15,
      r = 25,
      b = 15,
      l = 10))

print(g7)
#lookin good


#========================================
# GRAPHIC 8 (NEW — Key Insight)
# Risk Score vs Fraud Rate
# Shows the composite risk score
# created in feature engineering
#========================================
cat("--- Graphic 8: Risk Score Ladder ---\n\n")

risk_g8 <- fraud_model %>%
  group_by(risk_score) %>%
  summarise(
    n          = n(),
    fraud_rate = mean(is_fraud=="fraud")*100,
    .groups    = "drop") %>%
  mutate(
    label = case_when(
      risk_score==0 ~ "Score 0\nDay + Low Amt\n+ Safe Category",
      risk_score==1 ~ "Score 1\nOne Risk Factor",
      risk_score==2 ~ "Score 2\nTwo Risk Factors",
      risk_score==3 ~ "Score 3\nNight + High Amt\n+ Risky Category"),
    fill_col = case_when(
      fraud_rate < 5  ~ col_low,
      fraud_rate < 20 ~ col_medium,
      TRUE            ~ col_fraud))

g8 <- ggplot(risk_g8,
             aes(x=factor(risk_score),
                 y=fraud_rate)) +
  geom_col(aes(fill=fill_col),
           width=0.6, alpha=0.9) +
  geom_text(aes(
    label=paste0(round(fraud_rate,1),
                 "%\n(n=",
                 format(n, big.mark=","),
                 ")")),
    vjust=-0.4, size=4,
    fontface="bold",
    color="#333333") +
  scale_fill_identity() +
  scale_x_discrete(
    labels=risk_g8$label) +
  scale_y_continuous(
    labels=function(x) paste0(x,"%"),
    expand=expansion(mult=c(0,0.2))) +
  labs(
    title   = "The Risk Score Ladder",
    subtitle= paste0(
      "Combining night hours + high value + risky category creates\n",
      "a score that escalates fraud rate from <2% to over 70%"),
    x       = "Composite Risk Score",
    y       = "Fraud Rate (%)",
    caption = "Source: Fraud Detection Dataset | Step 8") +
  theme_report() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(
      size=9, lineheight=1.3))

print(g8)

#========================================
# GRAPHIC 9 (Redesign of Model M4)
# K-Means Cluster Profiles
# Original: basic boxplot + barplot
# Redesign: unified ggplot with
#           clear cluster story
#========================================
cat("--- Graphic 9: Cluster Profiles ---\n\n")

cluster_g9 <- fraud_model %>%
  group_by(cluster_label) %>%
  summarise(
    n             = n(),
    median_amt    = median(amt),
    mean_amt      = mean(amt),
    pct_night     = mean(is_night)*100,
    fraud_rate    = mean(is_fraud=="fraud")*100,
    mean_risk     = mean(risk_score),
    .groups       = "drop")

# Bubble chart — size=n, x=amt, y=fraud rate
g9 <- ggplot(fraud_model %>%
               sample_n(3000),
             aes(x     = amt,
                 y     = as.numeric(
                   is_fraud == "fraud"),
                 color = cluster_label)) +
  
  geom_jitter(alpha  = 0.25,
              size   = 1,
              height = 0.04,
              width  = 0) +
  
  geom_smooth(method = "loess",
              se     = FALSE,
              lwd    = 1.8) +
  
  scale_color_manual(
    values = c("Low Risk"    = col_low,
               "Medium Risk" = col_medium,
               "High Risk"   = col_fraud)) +
  
  scale_x_log10(
    breaks = c(1, 10, 100, 1000),
    labels = c("$1", "$10",
               "$100", "$1K")) +
  
  scale_y_continuous(
    labels = percent_format(),
    limits = c(-0.1, 1.1)) +
  
  facet_wrap(~ cluster_label, ncol = 3) +
  
  labs(
    title    = "Three Risk Clusters — Distinct Fraud Profiles",
    subtitle = paste0(
      "Low Risk: small daytime purchases",
      " — High Risk: large night-time transactions\n",
      "Each cluster has a clearly different",
      " relationship between amount and fraud"),
    x        = "Transaction Amount",
    y        = "Fraud Probability",
    color    = "Cluster",
    caption  = paste0(
      "Source: Fraud Detection Dataset | Step 8\n",
      "Sample of 3,000 transactions")) +
  
  theme_report() +
  theme(
    legend.position  = "none",
    strip.text       = element_text(
      face = "bold",
      size = 12),
    strip.background = element_rect(
      fill  = "#F8F8F8",
      color = NA),
    
    # ↓ Angle x-axis labels slightly
    # so even if they are close they
    # do not sit directly on top of each other
    axis.text.x      = element_text(
      size  = 10,
      angle = 0,
      hjust = 0.5))

print(g9)
#not my favorite
# Warning message:
# Removed 7 rows containing missing values or values outside the scale range (`geom_smooth()`). 
#========================================
# GRAPHIC 10 (NEW — Summary Dashboard)
# The Four Key Numbers for the report
# Executive-level summary visual
#========================================
cat("--- Graphic 10: Executive Summary ---\n\n")

# Key metrics for the story
metrics <- data.frame(
  value   = c("12.4%", "7.7×",
              "41%",   "Score 3"),
  label   = c("of transactions\nare fraud",
              "larger fraud\namounts on average",
              "fraud rate at\n22:00–23:00",
              "flags >70%\nfraud probability"),
  icon_bg = c(col_fraud, col_not_fraud,
              col_night, col_medium),
  x       = c(1, 2, 3, 4),
  stringsAsFactors = FALSE)

g10 <- ggplot(metrics,
              aes(x=x, y=1)) +
  # Coloured tiles
  geom_tile(aes(fill=icon_bg),
            width=0.88, height=1.2,
            alpha=0.9) +
  # Big number
  geom_text(aes(label=value),
            y=1.15, size=8,
            fontface="bold",
            color="white") +
  # Description
  geom_text(aes(label=label),
            y=0.78, size=3.8,
            color="white",
            lineheight=1.3) +
  scale_fill_identity() +
  scale_x_continuous(
    breaks=1:4,
    labels=c(
      "Class\nImbalance",
      "Amount\nSignal",
      "Time\nSignal",
      "Risk\nScore")) +
  scale_y_continuous(limits=c(0.2,1.6)) +
  labs(
    title   = "Credit Card Fraud — Four Key Findings",
    subtitle= paste0(
      "A data-driven portrait of 14,383 transactions ",
      "across 13 US states (2019–2020)"),
    x       = NULL,
    y       = NULL,
    caption = paste0(
      "Source: Fraud Detection Dataset\n",
      "EDA Project — Kingsley Egei | Step 8")) +
  theme_report() +
  theme(
    axis.text.y      = element_blank(),
    axis.text.x      = element_text(
      face="bold", size=10),
    panel.grid       = element_blank(),
    plot.title       = element_text(
      size=18, face="bold"),
    plot.subtitle    = element_text(size=13))

print(g10)
# Great for executive summary page 

# final comments for this section
# STEP 8 SUMMARY
#========================================
cat("\n========================================\n")
cat("STEP 8 COMPLETE — VISUALIZATION SUMMARY\n")
cat("========================================\n\n")

cat(sprintf("%-5s  %-35s  %-20s  %s\n",
            "G#", "Title","Original", "Improvement"))
cat(strrep("-", 85), "\n")

improvements <- data.frame(
  g  = paste0("G",1:10),
  title = c(
    "Fraud Class Imbalance",
    "Amount Distribution",
    "Hour of Day",
    "Amount by Fraud Status",
    "Fraud Rate by Category",
    "Hour × Category Heatmap",
    "Fraud Rate by State",
    "Risk Score Ladder",
    "K-Means Cluster Profiles",
    "Executive Summary"),
  orig = c(
    "Basic barplot","Histogram (raw only)",
    "Volume only","Violin no annotation",
    "Bar chart","Basic heatmap",
    "Horizontal bar","New graphic",
    "Base R boxplot","New graphic"),
  improv = c(
    "Callout annotation + context",
    "Side-by-side before/after",
    "Dual-axis volume + fraud rate",
    "7.7× annotation + story",
    "Lollipop + risk tier bands",
    "Outlined worst zone",
    "Dumbbell + segments",
    "Escalation story",
    "Cluster scatter + loess",
    "4-number executive tile"),
  stringsAsFactors=FALSE)

for (i in 1:nrow(improvements)) {
  cat(sprintf("%-5s  %-35s  %-20s  %s\n",
              improvements$g[i],
              improvements$title[i],
              improvements$orig[i],
              improvements$improv[i]))
}

cat("\n--- DESIGN PRINCIPLES APPLIED ---\n\n")
cat("1. CONSISTENT COLOR SYSTEM\n")
cat("   Red=fraud, Blue=not fraud, Green=safe\n")
cat("   Same colors used across all 10 graphics\n\n")
cat("2. ANNOTATION OVER LEGEND\n")
cat("   Direct labels replace or augment legends\n")
cat("   Callouts explain the most important values\n\n")
cat("3. EVERY CHART HAS A STORY TITLE\n")
cat("   Not 'Distribution of Amount' but\n")
cat("   'Transaction Amount is the #1 Fraud Signal'\n\n")
cat("4. REFERENCE LINES FOR CONTEXT\n")
cat("   Average lines on every categorical comparison\n")
cat("   Allow viewer to quickly see above/below\n\n")
cat("5. CAPTIONS FOR REPRODUCIBILITY\n")
cat("   Source, dataset, and step noted on every plot\n\n")
cat("6. SCALE CHOICES JUSTIFIED\n")
cat("   Log scales used and explained inline\n")
cat("   Dual axes used only where genuinely needed\n\n")

#ready for step 9
