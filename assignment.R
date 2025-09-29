# Read the CSV file (assuming it's named "zulaikha_mohammadi_itc255.csv")
data <- read.csv("zulaikha_mohammadi_itc255.csv")

# Define the FDTQL function
FDTQL = function(x){
  AbsFreq = table(x)
  RelFreq = round(prop.table(AbsFreq), 2)
  CumFreq = cumsum(RelFreq)
  FDTx = cbind(AbsFreq, RelFreq, CumFreq)
  return(FDTx)
}

# Step 1: Descriptive Statistics for Salary
# Compute minimum, maximum, mean, and median
min_salary <- min(data$Salary...., na.rm = TRUE)
max_salary <- max(data$Salary...., na.rm = TRUE)
mean_salary <- mean(data$Salary...., na.rm = TRUE)
median_salary <- median(data$Salary...., na.rm = TRUE)

# Print descriptive statistics to console
cat("Descriptive Statistics for Salary ($):\n")
cat("Minimum:", round(min_salary, 2), "\n")
cat("Maximum:", round(max_salary, 2), "\n")
cat("Mean:", round(mean_salary, 2), "\n")
cat("Median:", round(median_salary, 2), "\n\n")

# Step 2: Create Frequency Distribution Table (FTD) for Salary using FDTQL
# Bin the Salary data into intervals
salary_bins <- cut(data$Salary...., 
                   breaks = seq(200, 2000, by = 200),  
                   right = FALSE)                  

# Apply FDTQL function
salary_fdt <- FDTQL(salary_bins)
colnames(salary_fdt) <- c("Absolute Frequency", "Relative Frequency", "Cumulative Frequency")

# Print the FDT
cat("Frequency Distribution Table for Salary ($):\n")
print(salary_fdt)

# Step 3: Transform Salary (Quantitative) to Qualitative Categories
# Define categories: Low (<600), Medium (600-1000), High (>1000)
data$Salary_Category <- vector("character", length = nrow(data))  # Initialize new column

# Using a loop with if-else to assign categories
for (i in 1:nrow(data)) {
  if (data$Salary....[i] < 600) {
    data$Salary_Category[i] <- "Low"
  } else if (data$Salary....[i] >= 600 & data$Salary....[i] <= 1000) {
    data$Salary_Category[i] <- "Medium"
  } else {
    data$Salary_Category[i] <- "High"
  }
}

# Step 4: Display the first few rows of the dataset with the new qualitative column
cat("\nDataset with Qualitative Salary Category (first 10 rows):\n")
print(head(data[, c("Salary....", "Salary_Category")], 10))

# Step 5: Create a frequency table for the qualitative Salary_Category using FDTQL
salary_cat_fdt <- FDTQL(data$Salary_Category)
colnames(salary_cat_fdt) <- c("Absolute Frequency", "Relative Frequency", "Cumulative Frequency")

# Print the frequency table for Salary Categories
cat("\nFrequency Table for Salary Categories:\n")
print(salary_cat_fdt)