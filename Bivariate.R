#ITC 255/Statistical Data Analysis - 
#BivarMethods examples
#dataset
dtTips = read.csv('zulaikha_mohammadi_itc255.csv')
head(dtTips)
dim(dtTips)
View(dtTips)

# we ranme the column "Salary...." → "Salary"
names(dtTips)[6] <- "Salary"

# Check again
names(dtTips)

#case of 2 Ql Varibales 
#Joint FDT table
#gender and level of statisfaction association? whether which on the man or female do sport more
table(dtTips$Gender, dtTips$Sport)
addmargins(table(dtTips$Gender, dtTips$Sport))

#now, let's check for the age and height 
table(dtTips$Age, dtTips$Height.cm.)
addmargins(table(dtTips$Age, dtTips$Height.cm.))

#age and salary
table(dtTips$Age, dtTips$Salary)
addmargins(table(dtTips$Age, dtTips$Salary))

#gender and level of Satisfaction checking 
table(dtTips$Gender, dtTips$Level.of.Satisfaction)
addmargins(table(dtTips$Gender, dtTips$Level.of.Satisfaction))

# Mean salary by gender
aggregate(Salary ~ Gender, data = dtTips, FUN = mean)

# Median salary by gender
aggregate(Salary ~ Gender, data = dtTips, FUN = median)

#Boxplot
boxplot(Salary ~ Gender, data = dtTips,
        main = "Salary by Gender", col = c("red", "green"))

# Joint Frequency Table between Gender and Sport
jtSG <- table(dtTips$Gender, dtTips$Sport)

dtTips$Gender <- trimws(dtTips$Gender)
dtTips$Sport <- trimws(dtTips$Sport)

#  the Joint Frequency Table
jtSG <- table(dtTips$Gender, dtTips$Sport)

# View the table
jtSG

# the Bar Plot
barplot(jtSG,
        beside = TRUE,
        col = rainbow(10),
        main = "Gender vs Sport Participation",
        xlab = "Sport (Y/N)",
        ylab = "Frequency",
        legend.text = rownames(jtSG))

#  the horizonatal and vertical line in the graph 
abline(v = 4, h = 0)
#checking their relationship whether is there any relationship - strong, weak or moderate among the chosen vairbales 
chisq.test(jtSG)

#the bar plopt for age and height 
jtSG <- table(dtTips$Age, dtTips$Height.cm.)

# View the joint FDT
jtSG

barplot(jtSG,
        beside = TRUE,
        col = rainbow(nrow(jtSG)),    
        main = "Relationship between Age and Height",
        xlab = "Height Category (Y/N)",
        ylab = "Frequency",
        legend.text = rownames(jtSG),
        args.legend = list(x = "topright"))

# Add a horizontal line at y = 0
abline(h = 0)

# Chi-square test to check association
chisq_result <- chisq.test(jtSG)
chisq_result

# Interpretation
if (chisq_result$p.value < 0.05) {
  cat("There is a significant relationship between the two variables (p < 0.05).")
} else {
  cat("There is no significant relationship between the two variables (p >= 0.05).")
}
