library(ggplot2)
 #dataset
mydata <- read.csv('zulaikha_mohammadi_itc255.csv')

# renaming Salary column for consistency
names(mydata)[names(mydata) == "Salary...."] <- "Salary"
head(mydata)
colnames(mydata)

 
# Scatter plot: Age vs Salary
ggplot(mydata, aes(x = Age, y = Salary)) +
  geom_point(aes(color = Gender, shape = Sport, size = Salary)) +
  theme_minimal() +
  labs(
    title = "Age vs Salary Scatter Plot",
    x = "Age",
    y = "Salary"
  )

# Scatter plot: Gender vs Sport (categorical)
ggplot(mydata, aes(x = Gender, y = Sport)) +
  geom_jitter(width = 0.2, height = 0.2, aes(color = Gender)) +
  theme_minimal() +
  labs(title = "Gender vs Sport")

# Bar chart: Gender count filled by Sport
ggplot(mydata) +
  geom_bar(aes(x = Gender, fill = Sport)) +
  theme_minimal() +
  labs(title = "Gender and Sport")

# Scatter plot: Age vs Salary with regression line
ggplot(mydata, aes(x = Age, y = Salary)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal() +
  labs(title = "Age vs Salary with Trend Line")

# Bar chart: Level of Satisfaction filled by Gender
ggplot(mydata) +
  geom_bar(aes(x = Level.of.Satisfaction, fill = Gender)) +
  theme_minimal() +
  labs(title = "Level of Satisfaction by Gender")

# Scatter plot: Age vs Height colored by Gender
ggplot(mydata, aes(x = Age, y = Height.cm., colour = Gender)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Age vs Height Colored by Gender")

# Scatter plot: Height vs Age with multiple aesthetics
ggplot(mydata, aes(x = Age, y = Height.cm.)) +
  geom_point(aes(colour = Gender, shape = Sport, size = Salary)) +
  theme_minimal() +
  labs(
    title = "Height vs Age Scatter Plot",
    x = "Age",
    y = "Height (cm)"
  )

# scatter plot: shows realtionship betwwen two categorial varibales ( Age vs Height  )
ggplot(mydata, aes(x = Age, y = Height.cm.)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Age vs Height")

# Line plot: Age vs Salary  
ggplot(mydata, aes(x = Age, y = Salary)) +
  geom_line() +
  theme_minimal() +
  labs(title = " Age vs Salary")

# Bar chart 
ggplot(mydata, aes(x = Gender)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Gender")

# Bar chart: Salary by Sport
ggplot(mydata, aes(x = Sport, y = Salary)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Salary by Sport")

# Histogram: Age distribution
ggplot(mydata, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Graph of Age")
