# --- Step 1: Load dataset from CSV ---
df <- read.csv("zulaikha_mohammadi_itc255.csv")   # change to your actual file name
View(df)

# --- Step 2: FDT for a QL variable (e.g., Level_of_Satisfaction) ---
AbsFreq <- table(df$Level.of.Satisfaction)       # Absolute Frequency
RelFreq <- round(prop.table(AbsFreq), 2)         # Relative Frequency
CumFreq <- cumsum(RelFreq)                       # Cumulative Frequency

FDT_satisfaction <- cbind(AbsFreq, RelFreq, CumFreq)
FDT_satisfaction
View(FDT_satisfaction)

# --- Step 3: Histogram and Density for QN variable (e.g., Salary) ---
hist(df$Salary....,
     col = "blue",
     main = "Salary Distribution",
     xlab = "Salary ($)")

plot(density(df$Salary....),
     col = "#003377",
     main = "Salary Density Distribution",
     xlab = "Salary ($)")