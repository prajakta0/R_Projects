getwd()
setwd("/Users/prajaktasalunke/Desktop/udemy/Data cleaning/")

# import data in R along with replaceing empty string values as missing values.
finance = read.csv("Future_500.csv",na.strings=c(""))

# Assess data programmatically
# look at a first few records
head(finance)

# understand the summary of the data
summary(finance)

# understand the structure of the data
str(finance)

# ------Data  Assessment--------
#1. Id is recognised as a numeric column when it should be a categorical column
# change non-factor to factor
finance$ID <- factor(finance$ID)

#change non-factor to factor
finance$Inception <- factor(finance$Inception)

#Run test for both
str(finance)
# ------Clean expenses column------
# slice out dollars from Expenses
finance$Expenses <- gsub(" Dollars", "", finance$Expenses)
finance$Expenses <- gsub(",", "", finance$Expenses)

#Run test to check changes
head(finance)

#slice out $ from Revenue 
finance$Revenue <- gsub("\\$",'',finance$Revenue)
finance$Revenue <- gsub(",",'',finance$Revenue)

#Run test to chceck changes
head(finance)

# Slice % sign in Growth column
finance$Growth <- gsub("\\%",'',finance$Growth)

# Run test to check change
head(finance)

# convert expense, revenue to numeric for arithmetic operations
finance$Expenses <- as.numeric(finance$Expenses)
typeof(finance$Expenses)

finance$Revenue <- as.numeric(finance$Revenue)
typeof(finance$Revenue)

#Run test to check changes
summary(finance)
finance$Growth <- as.numeric(finance$Growth)
str(finance)

#----------Missing data-----------
#check completeness of your dataset
complete.cases(finance)

# get only the subset of the dataset with missing values
finance[!complete.cases(finance),]

# -----Filetring dataset-------
# using which() locate data
finance[which(finance$Industry == 'Health'),]

# without using which u get back NA values too
finance[finance$Employees == 45,]

# Using which in locating data gives only data wher it matches the excat criteria
finance[which(finance$Employees == 45),]

#locate NA on data using is.na
finance[is.na(finance$Expenses),]
finance[is.na(finance$Inception),]
finance[is.na(finance$Profit),]

finance[!complete.cases(finance),]

#Remove missing values from dataset
#create copy of the data for backup
fin <- finance


fin <- fin[!is.na(fin$Industry),]
fin[complete.cases(fin),]

# Reorder row numbers after deleting rows to handle missing data
rownames(fin) <- NULL

# fill in missing values in state column
# extract data with missing vakues in state column
fin[is.na(fin$State),]

#Set data in missing value
fin[is.na(fin$State) & fin$City == "New York",]
fin[is.na(fin$State) & fin$City == "New York", "State"] <- "NY"

#check:
fin[c(11,377),]

# Set state CA for city california
fin[is.na(fin$State) & fin$City == "San Francisco","State"] <- "CA"

#check:
fin[c(82,265),]
fin[!complete.cases(fin),]

# Replace missing value wth median in employee column
# find median for employees column
fin[!complete.cases(fin),]

#assign calulation to a variable then calculate median for employee column where industry is retail
med_emp_retail <- median(fin[fin$Industry == 'Retail','Employees'], na.rm = TRUE)
med_emp_retail

# Apply median in employee column
fin[is.na(fin$Employees) & fin$Industry == 'Retail', 'Employees'] <- med_emp_retail

#check
fin[is.na(fin$Employees),]

# Find Na values in the column
fin[!complete.cases(fin),]

#assign calulation to a variable then calculate median for employee column where industry is Financial Services also remove NA values
med_emp_services <- median(fin[fin$Industry == "Financial Services","Employees"],na.rm = TRUE)

#apply value in the column
fin[is.na(fin$Employees) & fin$Industry == "Financial Services", 'Employees'] <- med_emp_services

#check by seeing incomplete cases in the dataframe
fin[!complete.cases(fin),]
# check with row index
fin[330,]

#Now check median for whole column again
median(fin$Employees)

# Find NA values in dataframe
fin[!complete.cases(fin),]

#calculate median for growth column and industry is construction
med_grwth_cons <- median(fin[fin$Industry == "Construction","Growth"], na.rm =TRUE)

#apply median at the right location
fin[is.na(fin$Growth) & fin$Industry == "Construction", "Growth" ] <- med_grwth_cons

#check
fin[8,]
fin[!complete.cases(fin),]

# Check other NA in the dataframe that you need to fix
fin[!complete.cases(fin),]

#Identifed Revenue column to be fixed for NA hence calculating median
med_rev <- median(fin[fin$Industry == "Construction","Revenue"],na.rm=TRUE)

#Apply the median
fin[is.na(fin$Revenue) & fin$Industry == "Construction","Revenue"] <- med_rev

#check
fin[!complete.cases(fin),]

# Fix expenses column with NA
fin[!complete.cases(fin),]

#calculate median 
med_exp <- median(fin[fin$Industry == "Construction","Expenses"], na.rm = TRUE)

#apply median
fin[is.na(fin$Expenses) & fin$Industry =="Construction" & is.na(fin$Profit),"Expenses"] <- med_exp

fin[!complete.cases(fin),]

#calcuakte profit for NA
# Revenue - expenses = profit
fin[is.na(fin$Profit),"Profit"] <- fin[is.na(fin$Profit),"Revenue"] - fin[is.na(fin$Profit),"Expenses"]
fin[is.na(fin$Profit),"Profit"]
fin[c(8,42),]

#Calculate expense 
# Expenses = Revenue - Profit
fin[is.na(fin$Expenses),"Expenses"] <- fin[is.na(fin$Expenses),"Revenue"] - fin[is.na(fin$Expenses), "Profit"]
fin[15,]

fin[!complete.cases(fin),]

# Create Visualizations
#Scatter plot for revenue, expense as per profit
library(ggplot2)
a <- ggplot( data = fin, aes(x= Revenue, y = Expenses, color = Industry, size = Profit)) + geom_point()

# Scatter plot Revenue and expense as per industry
b <- ggplot( data = fin, aes(x= Revenue, y = Expenses, color = Industry))
b + geom_point() + geom_smooth(fill = NA, size = 1.2)

#Boxplot 

c <- ggplot(data = fin, aes(x= Industry, y = Growth, color = Industry))
c + geom_boxplot(size = 0.8, alpha = 0.5, outlier.color = NA) + geom_jitter(size = 0.5)




