# ------------------------------------------------------------------------------
# Assignment 2
# Programmer name: 1. Hana Shah Binti Faizal Shah
#                  2. Muhammad Fakhri Azam bin Mohd Fadzil
#                  3. Mohammad Ryyan Rashid
# ------------------------------------------------------------------------------

# Question 1 -------------------------------------------------------------------
#load the dataset 
# Set working directory to the folder that contains the dataset
setwd("C:/Data")

df <- read.csv("StudentsPerformance.csv")
# display summary statistics
summary(df)

#check the datatypes of variables
str(df)

#display percentage of missing values in each column
(colMeans(is.na(df)))*100

#drop lunch column
df <- subset(df, select = -c(lunch))
colnames(df)

#change colnames
colnames(df)[1] = "STU_Gender"
colnames(df)[2] = "STU_Ethnic"
colnames(df)[3] = "PAR_Education"
colnames(df)[4] = "PREP_CourseStatus"
colnames(df)[5] = "Maths"
colnames(df)[6] = "Reading"
colnames(df)[7] = "Writing"

df1 <- df

#Factorize Categorical Variables
df1$STU_Gender <- as.factor(df$STU_Gender)
df1$STU_Ethnic<- as.factor(df$STU_Ethnic)
df1$PAR_Education <- as.factor(df$PAR_Education)
df1$PREP_CourseStatus <- as.factor(df$PREP_CourseStatus)

#display categorical variables
table(df1$STU_Gender)
table(df1$PSTU_Ethnic)
table(df1$PAR_Education)
table(df1$PREP_CourseStatus)

#display non-vategorical variables
summary(df1$Maths)
summary(df1$Writing)
summary(df1$Reading)

#display colnames of the dataset
colnames(df1)

df <- df1
# Question 2 -------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(plotly)

#---------------------------PIE CHART FOR ETHINIC-------------------------------
#generate pie chart for ethnicity
groups <- table(df$STU_Ethnic)
lbls <- paste(names(groups), "\n", groups, sep="")
lgd <- c("Group A","Group B","Group C","Group D","Group E")
pie(groups, labels = groups, 
    col = rainbow(length(groups)), 
    cex=.75,
    main="Student Ethnic"
)
legend("topright", lgd, fill = rainbow(length(groups)), cex=.75)

#--------------------------BAR CHART FOR GENDER---------------------------------

#Group and summarize gender
grp_gender <- df %>% group_by(STU_Ethnic, STU_Gender) %>% summarise(count = n())

#plot the barplot using ggplot
p0<-grp_gender %>% 
  ggplot(aes(x=STU_Ethnic, y=count, fill = STU_Gender)) +
  geom_col() +
  labs(fill = "Gender") +
  labs(title="Distribution of Student by Ethnicity", x ="Ethnic", y = "Count")

p0<-ggplotly(p0)
p0

#---------------------BAR CHART FOR PREPARATION COURSE STATUS-------------------

#Group and summarize preparation course status
grp_prep <- df %>% group_by(STU_Ethnic, PREP_CourseStatus) %>% summarise(count = n())

#plot the barplot using ggplot
inter0 <- grp_prep %>% 
  ggplot(aes(x=STU_Ethnic, y=count, fill = PREP_CourseStatus)) +
  geom_col() +
  labs(fill = "Status") +
  labs(title="Distribution of Preparation Course Status by Ethnicity", x ="Ethnic", y = "Count")

inter0 <- ggplotly(inter0)
inter0

#------------------------BAR CHART FOR PARENT EDUCATION-------------------------
#Group and summarize Parent Education Level
grp_education <- df %>% group_by(STU_Ethnic, PAR_Education) %>% summarise(count = n())

#plot the barplot using ggplot
p1 <- grp_education %>% 
  ggplot(aes(x=STU_Ethnic, y=count, fill = PAR_Education)) +
  geom_col() +
  scale_y_continuous(limits=c(0,300),breaks = seq(0,300,50))+
  labs(fill = "Education Level") +
  labs(title="Distribution of Parent Education by Ethnicity", x ="Ethnic", y = "Count")+
  theme_bw()

p1 <- ggplotly(p1)
p1
# Question 3 -------------------------------------------------------------------
#function to determine mark range
getGrade <- function(mark) {
  if(mark >= 90 & mark <= 100) {
    return("90-100")
  } else {
    if(mark >= 80 & mark <= 89) {
      return("80-89")
    } else {
      if(mark >= 70 & mark <= 79) {
        return("70-79")
      } else {
        if(mark >= 60 & mark <= 69) {
          return("60-69")
        } else {
          if(mark >= 50 & mark <= 59) {
            return("50-59")
          } else {
            if(mark >= 40 & mark <= 49) {
              return ("40-49")
            } else {
              if(mark >= 30 & mark <= 39) {
                return ("30-39")
              } else {
                if(mark > 20 & mark <= 29) {
                  return ("20-29")
                } else {
                  if(mark > 10 & mark <= 19) {
                    return ("10-19")
                  } else{
                    return("0-9")
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

#assign mark range
df$Maths_MarkRange <- sapply(df$Maths, FUN=getGrade)
df$Reading_MarkRange <- sapply(df$Reading, FUN=getGrade)
df$Writing_MarkRange <- sapply(df$Writing, FUN=getGrade)

#maths barplot
p2<-ggplot(df, mapping=aes(x = Maths_MarkRange, fill=STU_Ethnic)) + 
  geom_bar() +
  scale_y_continuous(limits=c(0,300),breaks = seq(0,300,50))+
  labs(y = "Count", 
       x = "Mark Distribution",
       title = "Maths Mark Distribution Based on Ethnic") +
  theme_bw()
p2<-ggplotly(p2)
p2

#Reading barplot
p3<-ggplot(df, mapping=aes(x = Reading_MarkRange, fill=STU_Ethnic)) + 
  geom_bar() +
  scale_y_continuous(limits=c(0,300),breaks = seq(0,300,50))+
  labs(y = "Count", 
       x = "Mark Distribution",
       title = "Reading Mark Distribution Based on Ethnic") +
  theme_bw()
p3<-ggplotly(p3)
p3

#Writing barplot
p4 <- ggplot(df, mapping=aes(x = Writing_MarkRange, fill=STU_Ethnic)) + 
  geom_bar() +
  scale_y_continuous(limits=c(0,300),breaks = seq(0,300,50))+
  labs(y = "Count", 
       x = "Mark Distribution",
       title = "Writing Mark Distribution Based on Ethnic") +
  theme_bw()
p4 <- ggplotly(p4)
p4
# Question 4 -------------------------------------------------------------------
#plot boxplot maths vs parent education graph
p5 <- ggplot(data=df, aes(y=Maths, x=PAR_Education , fill= PAR_Education))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Maths Score V/S Parent Education")+xlab("Parent Education Level")+ylab("Maths Score")
p5 <- ggplotly(p5)
p5

#plot boxplot reading vs parent education graph
p6 <- ggplot(data=df, aes(y=Reading, x=PAR_Education , fill= PAR_Education))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Reading Score V/S Parent Education")+xlab("Parent Education Level")+ylab("Reading Score")
p6 <- ggplotly(p6)
p6

#plot boxplot Writing vs parent education graph
p7 <- ggplot(data=df, aes(y=Writing, x=PAR_Education , fill= PAR_Education))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Writing Score V/S Parent Education")+xlab("Parent Education Level")+ylab("Writing Score")
p7 <- ggplotly(p7)
p7
# Question 5 -------------------------------------------------------------------
# We will plot the box plot for each of the scores against the parental education.

ggplot(df, aes(x = PAR_Education, y = Maths, fill = PAR_Education)) +
  geom_boxplot() +
  facet_wrap(~ STU_Ethnic) +
  labs(title = "Maths Scores vs Parental Education  (based on ethnicity)", y = "Maths Score", x = "Parental Education")

ggplot(df, aes(x = PAR_Education, y = Reading, fill = PAR_Education)) +
  geom_boxplot() +
  facet_wrap(~ STU_Ethnic) +
  labs(title = "Reading Scores vs Parental Education (based on ethnicity)", y = "Reading Score", x = "Parental Education")

ggplot(df, aes(x = PAR_Education, y = Writing, fill = PAR_Education)) +
  geom_boxplot() +
  facet_wrap(~ STU_Ethnic) +
  labs(title = "Writing Scores vs Parental Education (based on ethnicity)", y = "Writing Score", x = "Parental Education")
# Question 6 -------------------------------------------------------------------
ggplot(df, aes(x = PREP_CourseStatus, y = Maths, fill = PREP_CourseStatus)) +
  geom_boxplot() +
  labs(title = "Maths Scores vs Course Preparation Status", y = "Maths Score", x = "Course Preparation Status")

ggplot(df, aes(x = PREP_CourseStatus, y = Reading, fill = PREP_CourseStatus)) +
  geom_boxplot() +
  labs(title = "Reading Scores vs Course Preparation Status", y = "Reading Score", x = "Course Preparation Status")

ggplot(df, aes(x = PREP_CourseStatus, y = Writing, fill = PREP_CourseStatus)) +
  geom_boxplot() +
  labs(title = "Writing Scores vs Course Preparation Status", y = "Writing Score", x = "Course Preparation Status")
# Question 7 -------------------------------------------------------------------
# Trend 1 - GENDER
ggplot(df, aes(x = STU_Gender, y = Maths, fill = STU_Gender)) +
  geom_boxplot() +
  labs(title = "Maths Scores by Gender", y = "Maths Score", x = "Gender")

ggplot(df, aes(x = STU_Gender, y = Reading, fill = STU_Gender)) +
  geom_boxplot() +
  labs(title = "Reading Scores by Gender", y = "Reading Score", x = "Gender")

ggplot(df, aes(x = STU_Gender, y = Writing, fill = STU_Gender)) +
  geom_boxplot() +
  labs(title = "Writing Scores by Gender", y = "Writing Score", x = "Gender")

# Trend 2 - GENDER & ETHNICITY
ggplot(df, aes(x = STU_Gender, y = Maths, fill = STU_Gender)) +
  geom_boxplot() +
  facet_wrap(~ STU_Ethnic) +
  labs(title = "Maths Scores vs Gender  (based on ethnicity)", y = "Maths Score", x = "Gender")

ggplot(df, aes(x = STU_Gender, y = Reading, fill = STU_Gender)) +
  geom_boxplot() +
  facet_wrap(~ STU_Ethnic) +
  labs(title = "Reading Scores vs Gender  (based on ethnicity)", y = "Reading Score", x = "Gender")

ggplot(df, aes(x = STU_Gender, y = Writing, fill = STU_Gender)) +
  geom_boxplot() +
  facet_wrap(~ STU_Ethnic) +
  labs(title = "Writing Scores vs Gender  (based on ethnicity)", y = "Writing Score", x = "Gender")
# Question 8 -------------------------------------------------------------------
# Import the necessary libraries
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyverse)

# Set working directory to the folder that contains the dataset
setwd("C:/Data")

# Read CSV file containing the dataset using read.csv() function
superstore <- read.csv("SuperStoreOrders.csv")

# Create a backup for the data
superstore_1 <- superstore

# Data Pre-processing and Data Cleaning

# Display data classes
str(superstore_1)

# Display the summary statistics of the data
summary(superstore_1)

# Check for duplicates
sum(duplicated(superstore_1)) #0

# Remove commas, empty spaces and convert data to numeric
numberize <- function(input){
  input <- gsub(",","",input)
  input <- gsub(" ","",input)
  return(as.numeric(input))
}

# Based on the dataset, there are 9 categorical variables. So, the variables can be changed into factor.

# Factorize categorical variables
superstore_1$ship_mode <- as.factor(superstore$ship_mode)
superstore_1$segment <- as.factor(superstore$segment)
superstore_1$state <- as.factor(superstore$state)
superstore_1$country <- as.factor(superstore$country)
superstore_1$market <- as.factor(superstore$market)
superstore_1$region <- as.factor(superstore$region)
superstore_1$category <- as.factor(superstore$category)
superstore_1$sub_category <- as.factor(superstore$sub_category)
superstore_1$order_priority <- as.factor(superstore$order_priority)

# Based on the dataset, there are three variables that are not in the correct format.

# Change 'sales' variable from character to numeric
superstore_1$sales <- numberize(superstore_1$sales)
# Change 'order_date' and 'ship_date' variables from character to date
superstore_1$order_date <- as.Date(superstore_1$order_date,"%d/%m/%Y")
superstore_1$ship_date <- as.Date(superstore_1$ship_date,"%d/%m/%Y")

summary(superstore_1)

# Display categorical variables in tables
table(superstore_1$ship_mode) 
table(superstore_1$segment) 
table(superstore_1$state) 
table(superstore_1$country) 
table(superstore_1$market) 
table(superstore_1$region) 
table(superstore_1$category) 
table(superstore_1$sub_category) 
table(superstore_1$order_priority)

# Display summary of non-categorical variables
summary(superstore_1$sales)
summary(superstore_1$order_date)
summary(superstore_1$ship_date)
summary(superstore_1$profit)

# Based on the summary statistics and the tables, it can be seen that column order_date, ship_date
# have missing values. Meanwhile, column profit has negative values.

# Display percentage of missing values in each column of the dataset
(colMeans(is.na(superstore_1)))*100

# order_date and ship_date have 60.88% and 61.33% of missing values respectively

# Check number of negative values in column profit
nrow(superstore_1[superstore_1$profit<0,]) #12543 values with negative value

# The date columns and negative values will be ignored in the data visualization

# Maintain only the top 1000 instances
superstore_df <- superstore_1[1:1000, ]

# Display the summary statistics of the cleaned dataset
summary(superstore_df)
# Question 9 -------------------------------------------------------------------
# 1. Group customer based on segments and determine the category distribution for each.

cat_segment <- table(superstore_df$category,superstore_df$segment)

# Create a barplot
catperseg <- barplot(cat_segment,
                     main = "Category Distribution Per Customer Segment",
                     xlab = "Customer Segment", ylab = "Frequency of Product Category",
                     col = c("#004E98", "#74F2CE", "#379634"),
                     legend.text = rownames(cat_segment),
                     beside = TRUE) # Grouped bars
catperseg

# 2. Find the Top 10 sub-categories.

subcat_sales <- superstore_df %>% 
  select(c("sub_category","sales")) %>% 
  group_by(sub_category) %>% 
  summarise_each(funs(sum))

# Create a barplot
sales_eachsubcat <- subcat_sales %>% 
  arrange(desc(sub_category)) %>%
  slice(1:10) %>%
  ggplot(., aes(x=reorder(sub_category,+sales),sales,fill = sales))+
  geom_bar(stat='identity') + 
  scale_fill_gradient(low="#C0C0C0",high="#004E98")+
  ggtitle("Top 10 Sub-Categories") +
  coord_flip() + 
  labs(y = "Sales", x = "Sub-Categories", fill="Sales") +
  geom_text(aes(label=sales), hjust=-0.3) + theme(panel.background = element_blank())
sales_eachsubcat

# 3. For each category determine the 3 most frequent-bought sub-categories.

# The 3 most frequent-bought sub-categories in the Furniture category
# Filter the data to select only rows with the category "Furniture"
filtered_data1 <- superstore_df %>%
  filter(category == "Furniture")

# Group filtered data by sub_category column
top_sub_furniture <- filtered_data1 %>%
  group_by(sub_category)

# Count frequency of each sub-category
f1 <- as.data.frame(dplyr::count(top_sub_furniture, sub_category))

# Create a barplot
subcat_furniture <- f1 %>% 
  arrange(desc(n)) %>%
  top_n(3) %>%
  ggplot(., aes(x=reorder(sub_category,+n),n,fill = n))+
  geom_bar(stat='identity') + 
  scale_fill_gradient(low="#C0C0C0",high="#004E98")+
  ggtitle("Top 3 Sub-Categories in Furniture") +
  labs(y = "Frequency", x = "Sub-Categories", fill="Frequency") +
  geom_text(aes(label=n), vjust=-0.3) + theme(panel.background = element_blank())
subcat_furniture 

# The 3 most frequent-bought sub-categories in the Office Supplies category
# Filter the data to select only rows with the category "Office Supplies"
filtered_data2 <- superstore_df %>%
  filter(category == "Office Supplies")

# Group filtered data by sub_category column
top_sub_office <- filtered_data2 %>%
  group_by(sub_category)

# Count frequency of each sub-category
f2 <- as.data.frame(dplyr::count(top_sub_office, sub_category))

# Create a barplot
subcat_office <- f2 %>% 
  arrange(desc(n)) %>%
  top_n(3) %>%
  ggplot(., aes(x=reorder(sub_category,+n),n,fill = n))+
  geom_bar(stat='identity') + 
  scale_fill_gradient(low="#C0C0C0",high="#004E98")+
  ggtitle("Top 3 Sub-Categories in Office Supplies") +
  labs(y = "Frequency", x = "Sub-Categories", fill="Frequency") +
  geom_text(aes(label=n), vjust=-0.3) + theme(panel.background = element_blank())
subcat_office

# The 3 most frequent-bought sub-categories in the Technology category
# Filter the data to select only rows with the category "Technology"
filtered_data3 <- superstore_df %>%
  filter(category == "Technology")

# Group filtered data by sub_category column
top_sub_tech <- filtered_data3 %>%
  group_by(sub_category)

# Count frequency of each sub-category
f3 <- as.data.frame(dplyr::count(top_sub_tech, sub_category))

# Create a barplot
subcat_tech <- f3 %>% 
  arrange(desc(n)) %>%
  top_n(3) %>%
  ggplot(., aes(x=reorder(sub_category,+n),n,fill = n))+
  geom_bar(stat='identity') + 
  scale_fill_gradient(low="#C0C0C0",high="#004E98")+
  ggtitle("Top 3 Sub-Categories in Technology") +
  labs(y = "Frequency", x = "Sub-Categories", fill="Frequency") +
  geom_text(aes(label=n), vjust=-0.3) + theme(panel.background = element_blank())
subcat_tech

# 4.  Where does most customer come from? Highlight the Top10 countries.

# Count number of customers of each country
# Note: There are duplicate values in the column "customer_name" so in order to get the actual
# number of customers, we need to get the number of distinct values
cust_country <- superstore_df %>% 
  select(c("country","customer_name")) %>% 
  group_by(country) %>%
  summarise(n = n_distinct(customer_name))

# Create a barplot
cust_eachcountry <- cust_country %>% 
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(., aes(x=reorder(country,+n),n,fill = n))+
  geom_bar(stat='identity') + 
  scale_fill_gradient(low="#C0C0C0",high="#004E98")+
  ggtitle("Top 10 Countries") +
  coord_flip() + 
  labs(y = "Number Of Customers", x = "Countries", fill="Customers") +
  geom_text(aes(label=n), hjust=-0.2) + theme(panel.background = element_blank())
cust_eachcountry
# Question 10 ------------------------------------------------------------------
# 1. Number of Orders Across Different Markets
# Count number of orders of each market
# Note: There are duplicates in the order_id column because purchase of different products can be
# under one order id. One order id corresponds to one order.
market_order <- superstore_df %>% 
  select(c("market","order_id")) %>% 
  group_by(market) %>%
  summarise(n = n_distinct(order_id))

# Create a barplot
order_eachmarket <- market_order %>% 
  arrange(desc(n)) %>%
  ggplot(., aes(x=reorder(market,-n),n,fill = n))+
  geom_bar(stat='identity') + 
  scale_fill_gradient(low="#C0C0C0",high="#004E98")+
  ggtitle("Number of orders across different markets") +
  labs(y = "Number Of Orders",x="Market", fill="Orders") +
  geom_text(aes(label=n), vjust=-0.3)  + theme(panel.background = element_blank())
order_eachmarket

# 2. Top 10 products by profit generated
# Count number of distinct values in product_name column
n_distinct(superstore_df$product_name)

# Group by and calculate the total sales of each product_name
product_sales <- superstore_df %>% 
  select(c("product_name","sales")) %>% 
  group_by(product_name) %>% 
  summarise_each(funs(sum))

# Create a barplot
topprod <- product_sales %>% 
  arrange(desc(sales)) %>%
  slice(1:10) %>%
  ggplot(., aes(x=reorder(product_name,+sales),sales,fill = sales))+
  geom_bar(stat='identity') + 
  scale_fill_gradient(low="#C0C0C0",high="#004E98")+
  ggtitle("Top 10 Products by Total Sales") +
  coord_flip() + 
  labs(y = "Sales", x = "Product", fill="Sales") + 
  geom_text(aes(label=sales), hjust=-0.1) + theme(panel.background = element_blank())
topprod

# 3. Profit across regions
# Group by and calculate the total profit generated by each region
region_profit <- superstore_df %>% 
  select(c("region","profit")) %>% 
  group_by(region) %>% 
  summarise_each(funs(sum))

# Create a bar plot
top_reg <- region_profit %>% 
  arrange(desc(profit)) %>%
  slice(1:10) %>%
  ggplot(., aes(x=reorder(region,+profit),profit,fill = profit))+
  geom_bar(stat='identity') + 
  scale_fill_gradient(low="#C0C0C0",high="#004E98")+
  ggtitle("Region by Profit Generated") +
  coord_flip() + 
  labs(y = "Profit ($)", x = "Region", fill="Profit ($)") + 
  geom_text(aes(label=round(profit,2)), hjust=-0.1) + theme(panel.background = element_blank())
top_reg

# 4. Number of Sales by Customer Segment
# Group by segment and calculate sum of sales
sales_by_segment <- superstore_df %>%
  group_by(segment) %>%
  summarize(sales = sum(sales))

# Create barplot
seg_sales <- sales_by_segment %>% 
  arrange(desc(sales)) %>%
  slice(1:10) %>%
  ggplot(., aes(x=reorder(segment,+sales),sales,fill = sales))+
  geom_bar(stat='identity') + 
  scale_fill_gradient(low="#C0C0C0",high="#004E98")+
  ggtitle("Number of Sales by Customer Segment") +
  labs(y = "Sales", x = "Segment", fill="Sales") + theme(panel.background = element_blank())+
  geom_text(aes(label=sales), vjust=-0.3)
seg_sales