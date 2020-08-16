'Employee Database'
##################
# Read/Load the data: '.csv file' (comma-separated values file)
employeedata = read.csv(file.choose(),header=TRUE)
View(employeedata)

# view the first 6 rows
head(employeedata)

# view the last 6 rows
tail(employeedata)

# what is this employeedata object?
class(employeedata)

# Information about data frame
str(employeedata)

'Subsetting data'
#################
# Accessing the particular column from the data set
# -------------------------------------------------
# Method-1
employeedata$gender
# Method-2
employeedata['gender']
# Method-3
employeedata[,2]

# Access the first five data entries of the 'gender' variable
head(employeedata['gender'])

# Accessing Multiple columns 
employeedata[,2:4]
employeedata[,c(1:3,5)]

# Accessing the columns names in the data sets (Variable names)
colnames(employeedata)

# Accessing a particular row
employeedata[1,]
employeedata[100,]
employeedata[c(1,4,7),]

# Command to Extract an Element
employeedata[4,5] # Element at 4th row, 5th column

# Selecting all the Observations based on condition(s)
employeedata[which(employeedata$gender=='Male'
                         & employeedata$educ > 19),]

employeedata[which(employeedata$gender=='Female'
                   & employeedata$jobcat == 'Manager'),]

# Coverting the int type variable to Factor type
class(employeedata$educ) 
# Set up factors.
employeedata$educ <- as.factor(employeedata$educ)
class(employeedata$educ) # Data type now 'factor'

' Data Analysis and Visulization'
#################################
# 1. How many male and female working in the company?
'----------------------------------------------------'
# One-Way Table
table(employeedata$gender)

# Simple Bar Plot
Gender <- table(employeedata$gender)
barplot(Gender, main="Gender Wise Employee",xlab="Gender", 
        ylab="Employee Count")

# Simple Horizontal Bar Plot
Gender <- table(employeedata$gender)
barplot(Gender, main="Gender Wise Employee", horiz=TRUE, 
        ylab="Gender", xlab="Employee Count")

# Color Bar Plot with border 
barplot(Gender, main="Gender Wise Employee",xlab="Gender", 
        ylab="Employee Count", col="green",border="blue")

# Pie Chart
Gender <- table(employeedata$gender)
pie(Gender, main="Gender Wise Employee")

# Pie Chart with % lables and Legend
piepercent<- round(100*Gender/sum(Gender), 1)
pie(Gender, labels = piepercent, main = "Gender Wise Employee",
    col = rainbow(length(Gender)))
legend("topright", c("Female", "Male"), cex = 1,
       fill = rainbow(length(Gender)))

# 3D Pie Chart 
library(plotrix)
piepercent<- round(100*Gender/sum(Gender), 1)
lbl<- paste(c("Female", "Male"),"\n",piepercent)
pie3D(Gender, labels = lbl, main = "Gender Wise Employee")

# 2. How many managers, clerical staff etc. (i.e. Job Catagory) 
# working in the company by gender? 
'--------------------------------------------------------------'
# Multiple Bar Plot (Grouped Bar Plot)
table(employeedata$gender, employeedata$jobcat)
GJ <- table(employeedata$gender, employeedata$jobcat)
barplot(GJ, main="Gender wise Job Categories",
        ylab = "Employee Count", xlab="Gender", 
        col=c("darkblue","red"),
        legend = rownames(GJ), beside=TRUE)

# Stacked Bar Plot with Colors and Legend
barplot(GJ, main="Gender wise Job Categories",
        ylab = "Employee Count", xlab="Gender", 
        col=c("darkblue","red"),
        legend = rownames(GJ), beside=FALSE)

# NUMERIAL DATA (Quantitative Data Analysis)
'-------------------------------------------'
'Histogram and Box Plot
-----------------------'
# HIstogram
hist(employeedata$salary)
hist(employeedata$salary , main='Employee Salary')
hist(employeedata$salary , main='Employee Salary',xlab='Salary', ylab ='Frequency')
hist(employeedata$salary , main='Employee Salary',xlab='Salary', ylab ='Frequency', col='blue')

'Perform the Descriptive Analysis'
# Measures of Centeral Tendency
mean(employeedata$salary)
median(employeedata$salary)
quantile(employeedata$salary)
quantile(employeedata$salary, 0.33)
quantile(employeedata$salary, 0.90)
table(employeedata$salary) # for Mode

# Measures of Variations
range(employeedata$salary) # Return smallest & largest value of the vector
max(employeedata$salary) - min(employeedata$salary)
IQR(employeedata$salary)
sd(employeedata$salary)
var(employeedata$salary)
CV <- (sd(employeedata$salary)/mean(employeedata$salary))*100
CV

summary(employeedata$salary)
summary(employeedata)

library(moments)
skewness(employeedata$salary)

# Box Plot
'Simple Box Plot'
par(mfrow=c(1,1))
boxplot(employeedata$salary)
boxplot(employeedata$salary,main='Employee Salary',ylab='Salary')

'Gender wise comparison using Box plot in salary'
emp.m <- employeedata[employeedata$gender=="Male",]
emp.f <- employeedata[employeedata$gender=="Female",]
boxplot(emp.m$salary,emp.f$salary)
boxplot(emp.m$salary,emp.f$salary, main='Employee Salary', ylab='Salary', 
        names = c('Male','Female'))

boxplot(emp.m$salary,emp.f$salary, main='Employee Salary', ylab='Salary', 
        names = c('Male','Female'),col = c("lightgreen", "lightblue"))
legend("topleft", legend=c('Male','Female'), 
       fill=c("lightgreen", "lightblue"), cex = 0.5)

'Job Category wise comparison using Box plot in salary'
levels(employeedata$jobcat)
emp.cl <- employeedata[employeedata$jobcat=="Clerical",] 
emp.cu <- employeedata[employeedata$jobcat=="Custodial",]
emp.Ma <- employeedata[employeedata$jobcat=="Manager",]
boxplot(emp.cl$salary, emp.cu$salary, emp.Ma$salary, main='Employee Salary', ylab='Salary',
        xlab='Job Category', names = c('Clerical','Custodial','Manager'))


library("vioplot")
vioplot(emp.cl$salary, emp.cu$salary, emp.Ma$salary, main='Employee Salary', ylab='Salary',
        xlab='Job Category', names = c('Clerical','Custodial','Manager'))

vioplot(emp.cl$salary, emp.cu$salary, emp.Ma$salary, main='Employee Salary', ylab='Salary',
        xlab='Job Category', names = c('Clerical','Custodial','Manager'),col='red')

vioplot(emp.cl$salary, emp.cu$salary, emp.Ma$salary, main='Employee Salary', ylab='Salary',
        xlab='Job Category', names = c('Clerical','Custodial','Manager'),
        col=c("lightgreen", "lightblue", "pink"))
legend("topleft", legend=c('Clerical','Custodial','Manager'), 
       fill=c("lightgreen", "lightblue", "pink"), cex = 0.5)


'Graphical Analysis Using GGPLOT2 Package'
# ---------------------------------------
library(ggplot2)

# 1. How many male and female working in the company?
#####################################################
table(employeedata$gender)
ggplot(employeedata, aes(x = gender)) + 
  geom_bar()

# Add some customization for labels and theme.
ggplot(employeedata, aes(x = gender)) + 
  # theme_bw() +
  # theme_classic()+
  theme_dark() +
  # theme_get() +
  #theme_gray() +
  geom_bar() +
  labs(y = "Employee Count",
       title = "Gender Wise Employee")

'With GGPLOT2 Package'
# Multiple Bar Chart (Grouped)
ggplot(employeedata, aes(x = gender, fill = jobcat)) + 
  theme_gray() +
  geom_bar(position="dodge") +
  labs(y = "Employee Count", title = "Gender wise Job Categories")

# Multiple Bar Chart (Stacked)
ggplot(employeedata, aes(x = gender, fill = jobcat)) + 
  theme_gray() +
  geom_bar() +
  labs(y = "Employee Count", title = "Gender wise Job Categories")

# 3. What is the salary of managers, clerical staff etc. (i.e. Job Category) 
# working in the company by gender? 
'---------------------------------------------------------------------------'
# Grouped
options(scipen=999)
ggplot(employeedata, aes(x = gender, y=salary, fill = jobcat)) + 
  theme_gray() +
  geom_bar(position="dodge", stat="identity") +
  labs(y = "Salary", title = "Gender wise Job Categories")

# Stacked
ggplot(employeedata, aes(x = gender, y=salary, fill = jobcat)) + 
  theme_gray() +
  geom_bar(stat="identity") +
  labs(y = "Salary", title = "Gender wise Job Categories")

# 4. Classify the employees with respect to their education? 
#############################################################
ggplot(employeedata, aes(x = educ, fill = gender)) + 
  theme_bw() +
  geom_bar(position="dodge") +
  labs(y = "Employee Count", title = "Education Wise Employees data")

# 5. What is the Job category with respect to education and gender?
###################################################################
# We can leverage facets to further segment the data and enable
# "visual drill-down" into the data.
ggplot(employeedata, aes(x = educ, fill = jobcat)) + 
  theme_bw() +
  facet_wrap(~ gender) +
  geom_bar(position="dodge") +
  labs(y = "Job Category", title = "Employees Data by Education and Job Catagory")


# 6. What is the distribution of Employee Salary?
#################################################
# The histogram is a staple of visualizing numeric data as it very 
# powerfully communicates the distrubtion of a variable (i.e., column).
ggplot(employeedata, aes(x = salary)) +
  theme_bw() +
  geom_histogram(binwidth = 20000) +
  labs(y = "Employee Count",
       x = "Salary (binwidth = 20000)",
       title = "Employee Salary Distribtion")

# 7. What is the distribution of male and female (i.e. employee) Salary?
########################################################################
ggplot(employeedata, aes(x = salary, fill = gender)) +
  theme_bw() +
  geom_histogram(binwidth = 20000) +
  labs(y = "Employee Count",
       x = "Salary (binwidth = 20000)",
       title = "Employee Salary Distribtion")

# Another great visualization for this question is the box-and-whisker plot.
ggplot(employeedata, aes(x = gender, y = salary)) +
  theme_bw() +
  geom_boxplot() +
  labs(y = "Salary",
       x = "Gender",
       title = "Gender Wise Employees Salary")

# Vilion Plot
ggplot(employeedata, aes(x = gender, y = salary)) +
  theme_bw() +
  geom_violin() +
  labs(y = "Salary",
       x = "Gender",
       title = "Gender Wise Employees Salary")

# Vilion Plot
ggplot(employeedata, aes(x = gender, y = salary)) +
  theme_bw() +
  geom_violin() +
  labs(y = "Salary",
       x = "Gender",
       title = "Gender Wise Employees Salary")

# Rotate the violin plot
ggplot(employeedata, aes(x = gender, y = salary)) +
  theme_bw() +
  geom_violin() +
  coord_flip() + # Rotate the plot 
  labs(y = "Salary",
       x = "Gender",
       title = "Gender Wise Employees Salary")


# Add summary statistics on a violin plot
# 1. Add mean Points
ggplot(employeedata, aes(x = gender, y = salary)) +
  theme_bw() +
  geom_violin() +
  stat_summary(fun=mean, geom="point", shape=23, size=2) # Add mean points 
  labs(y = "Salary",
       x = "Gender",
       title = "Gender Wise Employees Salary")

# 2. Add median Points
ggplot(employeedata, aes(x = gender, y = salary)) +
  theme_bw() +
  geom_violin() +
  stat_summary(fun=median, geom="point", size=2, color="red") # Add median points 
  labs(y = "Salary",
      x = "Gender",
      title = "Gender Wise Employees Salary")
  
# 3. Add median and quartile
ggplot(employeedata, aes(x = gender, y = salary)) +
  theme_bw() +
  geom_violin() +
  geom_boxplot(width=0.1) + # Add median and quartile
  #stat_summary(fun=median, geom="point", size=2, color="red") # Add median points 
  labs(y = "Salary",
       x = "Gender",
       title = "Gender Wise Employees Salary")
