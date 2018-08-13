# Scope of work #

# Marketing the right set of properties to customers to maximize property sales. #
# Arriving at the right number of property to market is the key to optimization. #
# As suggesting higher number of properties will have an additional operational cost #
# and suggesting lesser numbers might end up in lesser properties being converted #

# Available Datasets #

# Accounts : Customer details, we need to market properties to these customers #
# Opportunities : Transactions that have already happened between a customer and property #
# Accounts to Properties : Information on property already bought by a customer #
# Deal to Properties : Information on the deals that has materialized on the properties #
# Properties Database : Master list containing property details.

# Test Dataset : Has accounts for which you need to suggest properties.

#--------------------------------------------#

# Loading the files
accounts <- read.csv("Accounts.csv",header = TRUE, sep = ",")
opportunities <- read.csv("Opportunities.csv", header = TRUE, sep = ",")
account_prop <- read.csv("Accounts_properties.csv", header = TRUE, sep = ",")
deal_prop <- read.csv("Deals_to_Properties.csv", header = TRUE, sep = ",")
property_master <- read.csv("Properties.csv", header = TRUE, sep = ",")
test_data <- read.csv("Test_Data.csv",header = TRUE, sep = ",")

#--------------------------------------------#

# Loading the required libraries

library(caret)
library(ggplot2)
library(caTools)
library(stringr)
library(car)
library(dplyr)
library(ranger)
library(MASS)
library(recommenderlab)
library(cowplot)
library(ggpubr)
library(tidyr)
library(outliers)

#--------------------------------------------#
#             ACCOUNT DATASET
#--------------------------------------------#
# Basic data quality checks for accounts dataset.
# Checking for duplicate values in accounts dataset.

dup_account <- duplicated(accounts$id_accs)
sum(dup_account) # 0
# No duplicates found.

# Checking for missing values in the entire account dataset.
sapply(accounts, function(x) sum(is.na(x)))
# No missing values found in any of the columns.

# Lets look at the basic structure of the dataset.
str(accounts)
# We see that there are a combination of logical, interger and factor variables.

#--------------------------------------------#
# CATEGORICAL VARIABLES - Account Dataset
#--------------------------------------------#
# Lets do some exploratory analysis (Univariate Analysis)
# We will start with the logical variables.
continuous_var_accounts <- colnames(accounts)[c(1:3,6,15:20)]
View(continuous_var_accounts)

# Creating all logical variables: 
logical_var_accounts <- accounts[,!colnames(accounts)%in%continuous_var_accounts]
View(logical_var_accounts)

# Lets plot and analyse all the logical variables.
Graph1 <- plot_grid(ggplot(logical_var_accounts, aes(x=buyer_book)) + geom_bar(fill = "orange"), 
                    ggplot(logical_var_accounts, aes(x=servicing_contract)) + geom_bar(fill = "orange"),
                    ggplot(logical_var_accounts, aes(x=cmbs)) + geom_bar(fill = "orange"),
                    ggplot(logical_var_accounts, aes(x=consultant)) + geom_bar(fill = "orange"),
                    ggplot(logical_var_accounts, aes(x=correspondent)) + geom_bar(fill = "orange"),
                    ggplot(logical_var_accounts, aes(x=foreign)) + geom_bar(fill = "orange"),
                    align = "h")

Graph1
#--------------------------------------------#
# Buyer_book #
summary(as.factor(logical_var_accounts$buyer_book))
# FALSE  TRUE 
# 567  2189
# Almost 80% of the times customer prefer an investement advisory. 
#--------------------------------------------#
# Servicing_contracts #
summary(as.factor(logical_var_accounts$servicing_contract))
# FALSE  TRUE 
# 2700    56
# Almost 98% of the times we do not have contracts to service customers loans.
#--------------------------------------------#
# CMBS #
summary(as.factor(logical_var_accounts$cmbs))
# FALSE  TRUE 
# 2749     7
# Nearly 100% of the times customers are not involved in Mortgage Backed Securities.
#--------------------------------------------#
# Consultant #
summary(as.factor(logical_var_accounts$consultant))
# FALSE 
# 2756
# None of the customers prefer consulting for buying properties.
#--------------------------------------------#
# Correspondent #
summary(as.factor(logical_var_accounts$correspondent))
# FALSE  TRUE 
# 2741    15
# More than 99% of the times customers haven't bought anything from the origination secondary market.
#--------------------------------------------#
# Foreign #
summary(as.factor(logical_var_accounts$foreign))
# FALSE  TRUE 
# 2578   178
# Only 6 to 7% of the properties have attracted foreign investment
#--------------------------------------------#

Graph2 <-plot_grid(ggplot(logical_var_accounts, aes(x=master_servicer)) + geom_bar(fill = "orange"), 
                   ggplot(logical_var_accounts, aes(x=lender_book)) + geom_bar(fill = "orange"),
                   ggplot(logical_var_accounts, aes(x=loan_sales_book)) + geom_bar(fill = "orange"),
                   ggplot(logical_var_accounts, aes(x=loan_servicing)) + geom_bar(fill = "orange"),
                   align = "h")

Graph2
#--------------------------------------------#
# Master_servicer #
summary(as.factor(logical_var_accounts$master_servicer))
# FALSE  TRUE 
# 2754     2
# Almost all the times customers do not take the service of a master services that mostly
# service loans from pool to maturity.
#--------------------------------------------#
# Lender_book #
summary(as.factor(logical_var_accounts$lender_book))
# FALSE  TRUE 
# 1509  1247
# Around 55% of times customers do not prefer taking loans from private lenders, but
# 45% of the times customers see this as an option to count on.
#--------------------------------------------#
# Loan_sales_book #
summary(as.factor(logical_var_accounts$loan_sales_book))
# FALSE  TRUE 
# 964  1792
# 65% of the customers prefer this option, as they don't mind selling their property,
# while the property still has a pending loan amount attached to it.
#--------------------------------------------#
# Loan_servicing #
summary(as.factor(logical_var_accounts$loan_servicing))
# FALSE  TRUE 
# 2755     1
# All of the customers do not prefer mortgage servicing as a option.
#--------------------------------------------#
# CONTINIOUS VARIABLES - Account Dataset
#--------------------------------------------#
# Lets now look at the continious variables for the accounts dataset.
continuous_var_accounts <- accounts[,colnames(accounts)%in%continuous_var_accounts]
View(continuous_var_accounts)

# Checking the structure
str(continuous_var_accounts)

# Id_accs is just the unique id, nothing to do with that.
# We will look at all the other variables and take up investor_type in the end.

#--------------------------------------------#
# active_deals #

# How are active_deals distributed?

# Checking the range
range(continuous_var_accounts$active_deals)
# The range is between 0 to 2215 deals that are actively in pipeline.

# Checking the quantile distribution
quantile(continuous_var_accounts$active_deals,seq(0,1,0.01))
# Around 7% of customners do not have any active deals,
# almost 1/3 of customers have 1 active deal and less than that.
# Only 1% of the data has deals numbering more than 280.

# Creating a plot to check
active_deal_graph <- ggplot(continuous_var_accounts, aes(x = active_deals))

plot_grid(active_deal_graph + geom_histogram(bins = 30, color = "black", fill = "orange") + 
            geom_vline(aes(xintercept = mean(active_deals)), linetype = "dashed", size = 0.6, color = "#0000FF"),
          active_deal_graph + geom_density(aes(y = ..count..), fill = "gray") +  
            geom_vline(aes(xintercept = mean(active_deals)), linetype = "dashed", size = 0.6, color = "#0000FF"))

# This variable is right skewed and highly dense on the lower side

median(continuous_var_accounts$active_deals)
# With a median value of 3 we see that most of the customers have few deals at hand.

#--------------------------------------------#
# activity_count #

# How are activity_count distributed?

# Checking the range
range(continuous_var_accounts$activity_count)
# The range is between 0 to 217301 activity counts.

# Checking the quantile distribution
quantile(continuous_var_accounts$activity_count,seq(0,1,0.01))
# Around 9% of customners do not have any activity.
# Only 1% of the data have activity numbering more than 110848.

# Creating a plot to check
activity_count_graph <- ggplot(continuous_var_accounts, aes(x = activity_count))

plot_grid(activity_count_graph + geom_histogram(bins = 30, color = "black", fill = "orange") + 
            geom_vline(aes(xintercept = mean(activity_count)), linetype = "dashed", size = 0.6, color = "#0000FF"),
          activity_count_graph + geom_density(aes(y = ..count..), fill = "gray") +  
            geom_vline(aes(xintercept = mean(activity_count)), linetype = "dashed", size = 0.6, color = "#0000FF"))

# This variable is right skewed and highly dense on the lower side

median(continuous_var_accounts$activity_count)
mean(continuous_var_accounts$activity_count)
# With a median value of 2354 and mean value of 9460 we see that 25% of the customers have
# done more that 10000 activity.

#--------------------------------------------#
# num_deals_as_client #

# How are num_deals_as_client distributed?

# Checking the range
range(continuous_var_accounts$num_deals_as_client)
# The range is between 0 to 333 deals.

# Checking the quantile distribution
quantile(continuous_var_accounts$num_deals_as_client,seq(0,1,0.01))
# Around 50% of times company hasn't hired anyone to get the deal.
# 99% of the data constitutes of less than 80 deals for which company hired. 

# Creating a plot to check
num_deals_as_client_graph <- ggplot(continuous_var_accounts, aes(x = num_deals_as_client))

plot_grid(num_deals_as_client_graph + geom_histogram(bins = 30, color = "black", fill = "orange") + 
            geom_vline(aes(xintercept = mean(num_deals_as_client)), linetype = "dashed", size = 0.6, color = "#0000FF"),
          num_deals_as_client_graph + geom_density(aes(y = ..count..), fill = "gray") +  
            geom_vline(aes(xintercept = mean(num_deals_as_client)), linetype = "dashed", size = 0.6, color = "#0000FF"))

# This variable is right skewed and highly dense on the lower side

mean(continuous_var_accounts$num_deals_as_client)
# For 20% of the deals that a company did, it hired more than 7 times.

#--------------------------------------------#
# num_deals_as_investor #

# How are num_deals_as_investor distributed?

# Checking the range
range(continuous_var_accounts$num_deals_as_investor)
# The range is between 0 to 2205 deals.

# Checking the quantile distribution
quantile(continuous_var_accounts$num_deals_as_investor,seq(0,1,0.01))
# Around 9% of times company was not the investor.
# 99% of the data constitutes of less than 186 deals for which company invested. 

# Creating a plot to check
num_deals_as_investor_graph <- ggplot(continuous_var_accounts, aes(x = num_deals_as_investor))

plot_grid(num_deals_as_investor_graph + geom_histogram(bins = 30, color = "black", fill = "orange") + 
            geom_vline(aes(xintercept = mean(num_deals_as_investor)), linetype = "dashed", size = 0.6, color = "#0000FF"),
          num_deals_as_investor_graph + geom_density(aes(y = ..count..), fill = "gray") +  
            geom_vline(aes(xintercept = mean(num_deals_as_investor)), linetype = "dashed", size = 0.6, color = "#0000FF"))

# This variable is right skewed and highly dense on the lower side

mean(continuous_var_accounts$num_deals_as_investor)
# For 10% of the times company invested for more than 10 times, 90% of the times it was less than 10.

#--------------------------------------------#
# number_of_properties #

# How are number_of_properties distributed?

# Checking the range
range(continuous_var_accounts$number_of_properties)
# The range is between 0 to 1335 properties.

# Checking the quantile distribution
quantile(continuous_var_accounts$number_of_properties,seq(0,1,0.01))
# Around 10% of times customers do not have any property.
# 99% of the data constitutes of less than 294 properties that customers own. 

# Creating a plot to check
number_of_properties_graph <- ggplot(continuous_var_accounts, aes(x = number_of_properties))

plot_grid(number_of_properties_graph + geom_histogram(bins = 30, color = "black", fill = "orange") + 
            geom_vline(aes(xintercept = mean(number_of_properties)), linetype = "dashed", size = 0.6, color = "#0000FF"),
          number_of_properties_graph + geom_density(aes(y = ..count..), fill = "gray") +  
            geom_vline(aes(xintercept = mean(number_of_properties)), linetype = "dashed", size = 0.6, color = "#0000FF"))

# This variable is right skewed and highly dense on the lower side

median(continuous_var_accounts$number_of_properties)
mean(continuous_var_accounts$number_of_properties)
# 95% of customers own less than 21 properties
# 50% of customers have 2 or less properties.

#--------------------------------------------#
# number_of_related_deals #

# How are number_of_related_deals distributed?

# Checking the range
range(continuous_var_accounts$number_of_related_deals)
# The range is between 0 to 2137 for related deals.

# Checking the quantile distribution
quantile(continuous_var_accounts$number_of_related_deals,seq(0,1,0.01))
# Around 6% of times customers do not have any related deals.
# 99% of the data constitutes of less than 336 related deals for customers. 

# Creating a plot to check
number_of_related_deals_graph <- ggplot(continuous_var_accounts, aes(x = number_of_related_deals))

plot_grid(number_of_related_deals_graph + geom_histogram(bins = 30, color = "black", fill = "orange") + 
            geom_vline(aes(xintercept = mean(number_of_related_deals)), linetype = "dashed", size = 0.6, color = "#0000FF"),
          number_of_related_deals_graph + geom_density(aes(y = ..count..), fill = "gray") +  
            geom_vline(aes(xintercept = mean(number_of_related_deals)), linetype = "dashed", size = 0.6, color = "#0000FF"))

# This variable is right skewed and highly dense on the lower side

mean(continuous_var_accounts$number_of_related_deals)
median(continuous_var_accounts$number_of_related_deals)
# 95% of customers have related deals that are less than 21
# 50% of customers have 4 or less related deals.

#--------------------------------------------#
# number_of_related_properties #

# How are number_of_related_properties distributed?

# Checking the range
range(continuous_var_accounts$number_of_related_properties)
# The range is between 1 to 1479 for related properties.

# Checking the quantile distribution
quantile(continuous_var_accounts$number_of_related_properties,seq(0,1,0.01))
# Around 19% of times customers have only one related property.
# 99% of the data constitutes of less than 381 related properties for customers. 

# Creating a plot to check
number_of_related_properties_graph <- ggplot(continuous_var_accounts, aes(x = number_of_related_properties))

plot_grid(number_of_related_properties_graph + geom_histogram(bins = 30, color = "black", fill = "orange") + 
            geom_vline(aes(xintercept = mean(number_of_related_properties)), linetype = "dashed", size = 0.6, color = "#0000FF"),
          number_of_related_properties_graph + geom_density(aes(y = ..count..), fill = "gray") +  
            geom_vline(aes(xintercept = mean(number_of_related_properties)), linetype = "dashed", size = 0.6, color = "#0000FF"))

# This variable is right skewed and highly dense on the lower side

median(continuous_var_accounts$number_of_related_properties)
mean(continuous_var_accounts$number_of_related_properties)
# 80% of customers have related properties that are less than 28
# 50% of customers have 7 or less related deals.

#--------------------------------------------#
# number_of_won_deals_as_client #

# How are number_of_won_deals_as_client distributed?

# Checking the range
range(continuous_var_accounts$number_of_won_deals_as_client)
# The range is between 0 to 330 for deals won.

# Checking the quantile distribution
quantile(continuous_var_accounts$number_of_won_deals_as_client,seq(0,1,0.01))
# Around 54% of times customers have won a deal.
# 99% of the data constitutes of less than 69 won deals for customers. 

# Creating a plot to check
number_of_won_deals_as_client_graph <- ggplot(continuous_var_accounts, aes(x = number_of_won_deals_as_client))

plot_grid(number_of_won_deals_as_client_graph + geom_histogram(bins = 30, color = "black", fill = "orange") + 
            geom_vline(aes(xintercept = mean(number_of_won_deals_as_client)), linetype = "dashed", size = 0.6, color = "#0000FF"),
          number_of_won_deals_as_client_graph + geom_density(aes(y = ..count..), fill = "gray") +  
            geom_vline(aes(xintercept = mean(number_of_won_deals_as_client)), linetype = "dashed", size = 0.6, color = "#0000FF"))

# This variable is right skewed and highly dense on the lower side

mean(continuous_var_accounts$number_of_won_deals_as_client)
# 80% of customers have won 5 and less deals.

#--------------------------------------------#
# investor_type #

# How are investor_type distributed?

# Lets check the summary.
summary(continuous_var_accounts$investor_type)
# 1517 customers are private Investors/Developer.

# Computing the frequency.
invest_freq <- continuous_var_accounts %>% group_by(investor_type) %>% summarise(counts = n())

# Setting the theme function to theme_pubr()
theme_set(theme_pubr())

# creating a bar theme
bar_theme <- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

# Creating a plot to check
ggplot(invest_freq, aes(x = investor_type, y = counts)) +  
  geom_bar(fill = "#0073C2FF", stat = "identity") + 
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() + bar_theme

# The highest number (1517) of investors are private investors/developers followed
# by advisors (326) and banks (320).

#--------------------------------------------#
#         OPPORTUNITY DATASET
#--------------------------------------------#

# Checking the structure
str(opportunities)

# We will not look at all the variables for descreptive stat purpose.
# We will leave id_deals, id_accs and id_deals.1 as they are id columns
# We will also not look at all the date columns as of now.
# What we are going to consider are deal_type, deal_update_flag, fiscalyear
# platform, property_group & property_type
#--------------------------------------------#

# deal_type #

# Lets check the summary.
summary(opportunities$deal_type)
# The largest number of deal type is fixed rate  .

# Computing the frequency.
dealtype_freq <- opportunities %>% group_by(deal_type) %>% summarise(counts = n())

# Creating a plot to check
ggplot(dealtype_freq, aes(x = deal_type, y = counts)) +  
  geom_bar(fill = "#0073C2FF", stat = "identity") + 
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() + bar_theme

# So the top three deal types are
# Fixed Rate - 12740
# Property Sale - 4729
# Floating Rate - 3115
#--------------------------------------------#

# deal_update_flag #

# Lets check the summary.
summary(opportunities$deal_update_flag)
# FALSE    TRUE 
# 80      25949

# Plotting a graph to check
ggplot(opportunities, aes(x=deal_update_flag)) + geom_bar(fill = "orange")
# 97% of the times the deal_update_flag has a true state.  
#--------------------------------------------#

# fiscalyear #

# Lets check the summary.
summary(as.factor(opportunities$fiscalyear))
# 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 
# 7058  999 1062 1523 1343 1076  552  530  985 1217 1456 1754 2021 2270 2183
# We see that most of the deals have come in 2002 followed by 2015.

opportunities$fiscalyear <- as.factor(opportunities$fiscalyear)

# Computing the frequency.
fiscalyear_freq <- opportunities %>% group_by(fiscalyear) %>% summarise(counts = n())

# Creating a plot to check
ggplot(fiscalyear_freq, aes(x = fiscalyear, y = counts)) +  
  geom_bar(fill = "#0073C2FF", stat = "identity") + 
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() + bar_theme

# So the top three fiscal years are
# 2002 - 7058
# 2015 - 2270
# 2016 - 2183
#--------------------------------------------#

# platform #

# Lets check the summary.
summary(as.factor(opportunities$platform))
# Blanks        Debt         Equity Placement  Investment Sales   Loan Sales       Securities 
# 6            18685             1800             5187              275               76
# We see that most of the times platform is Debt.

# Computing the frequency.
platform_freq <- opportunities %>% group_by(platform) %>% summarise(counts = n())

# Creating a plot to check
ggplot(platform_freq, aes(x = platform, y = counts)) +  
  geom_bar(fill = "#0073C2FF", stat = "identity") + 
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() + bar_theme

# So the top two platforms are
# Debt - 18685
# Investment_Sales - 5187
#--------------------------------------------#

# property_group #

# Lets check the summary.
summary(as.factor(opportunities$property_group))
# We see that most of the times product group is multi-housing.

# Computing the frequency.
property_group_freq <- opportunities %>% group_by(property_group) %>% summarise(counts = n())

# Creating a plot to check
ggplot(property_group_freq, aes(x = property_group, y = counts)) +  
  geom_bar(fill = "#0073C2FF", stat = "identity") + 
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() + bar_theme

# So the top three property_group are
# Multi-Housing - 8798
# Office - 5670
# Retail - 5293
#--------------------------------------------#

# property_type #

# Lets check the summary.
summary(as.factor(opportunities$property_type))
# We see that most of the times property_type is apartments.

# Computing the frequency.
property_type_freq <- opportunities %>% group_by(property_type) %>% summarise(counts = n())

# Creating a plot to check
ggplot(property_type_freq, aes(x = property_type, y = counts)) +  
  geom_bar(fill = "#0073C2FF", stat = "identity") + 
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() + bar_theme

# So the top three property_type are
# apartments - 8272
# Office - 5616
# Retail - 5293

#--------------------------------------------#
#         PROPERTY MASTER DATASET
#--------------------------------------------#

# Basic data quality checks for property master dataset.
# Checking for duplicate values in property master dataset.

dup_prop_master <- duplicated(property_master$id_props)
sum(dup_prop_master) # 0
# No duplicates found.

# Checking the structure
str(property_master)

# We will not consider id_props, id_deals for the descriptive stats part
#--------------------------------------------#

# Building Status #

# Computing the frequency.
building_status_freq <- property_master %>% group_by(building_status) %>% summarise(counts = n())

# Creating a plot to check
ggplot(building_status_freq, aes(x = building_status, y = counts)) +  
  geom_bar(fill = "#0073C2FF", stat = "identity") + 
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() + bar_theme

# "Existing" building status has the highest frequency
# We see that for 8217 properties building status is not recorded.
#--------------------------------------------#

# Class #

# Computing the frequency.
class_freq <- property_master %>% group_by(class) %>% summarise(counts = n())

# Creating a plot to check
ggplot(class_freq, aes(x = class, y = counts)) +  
  geom_bar(fill = "#0073C2FF", stat = "identity") + 
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() + bar_theme

# For 6948 properties the class is not known.
# There are 5430 properties that come under class A
#--------------------------------------------#

# sale_status #

# Computing the frequency.
sale_status_freq <- property_master %>% group_by(sale_status) %>% summarise(counts = n())

# Creating a plot to check
ggplot(sale_status_freq, aes(x = sale_status, y = counts)) +  
  geom_bar(fill = "#0073C2FF", stat = "identity") + 
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() + bar_theme

# Most of the properties (16501) that are sold have status as "Z" 
#--------------------------------------------#

# property_type_1 #

# Computing the frequency.
property_type_1_freq <- property_master %>% group_by(property_type_1) %>% summarise(counts = n())

# Creating a plot to check
ggplot(property_type_1_freq, aes(x = property_type_1, y = counts)) +  
  geom_bar(fill = "#0073C2FF", stat = "identity") + 
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() + bar_theme

# Top three property types are 
# Multi-Housing - 5627
# Office - 3744
# Retail - 3474
#--------------------------------------------#

# region__c #

# Computing the frequency.
region__c_freq <- property_master %>% group_by(region__c) %>% summarise(counts = n())

# Creating a plot to check
ggplot(region__c_freq, aes(x = region__c, y = counts)) +  
  geom_bar(fill = "#0073C2FF", stat = "identity") + 
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean() + bar_theme

# Top two regions where properties are listed are
# Southwest - 4778 
# West - 4083
#--------------------------------------------#

# num_buildings # 

# Checking the range
range(property_master$num_buildings, na.rm = T)
# The range is between 0 to 1980 buildings

# Checking the quantile distribution
quantile(property_master$num_buildings,seq(0,1,0.01), na.rm = T)
# Around 54% of times number of building is only 1.
# 99% of the data constitutes of less than 50 buildings. 

mean(property_master$num_buildings, na.rm = T)
# 75% of this variables consitutes of properties that have 6 and less buildings.
#--------------------------------------------#

# num_floors #

# Checking the range
range(property_master$num_floors, na.rm = T)
# The range is between 0 to 242 floors

# Checking the quantile distribution
quantile(property_master$num_floors,seq(0,1,0.01), na.rm = T)
# Around 32% of times number of floors is only 1.
# 99% of the data constitutes of less than 35 floors. 

mean(property_master$num_floors, na.rm = T)
# 78% of this variables consitutes of properties that have 4 and less floors.
#--------------------------------------------#

# num_parking_spaces #

# Checking the range
range(property_master$num_parking_spaces, na.rm = T)
# The range is between 1 to 13975 parking spaces

# Checking the quantile distribution
quantile(property_master$num_parking_spaces,seq(0,1,0.01), na.rm = T)
# 99% of the data constitutes of less than 2030 parking spaces. 

mean(property_master$num_parking_spaces, na.rm = T)
# 66% of this variables consitutes of properties that have 365 and less parking spaces.
#--------------------------------------------#

# occupancy_rate #

# Checking the range
range(property_master$occupancy_rate, na.rm = T)
# The range is between 0 to 100 occupants

# Checking the quantile distribution
quantile(property_master$occupancy_rate,seq(0,1,0.01), na.rm = T)
# 3% of the properties have no occupancy.
# 1/3 of the properties have 100 occupancy. 

mean(property_master$occupancy_rate, na.rm = T)
# 25% of this variables consitutes of properties that have 87 and less occupants.
#--------------------------------------------#

# size_units #

# Checking the range
range(property_master$size_units, na.rm = T)
# The range is between 0 to 1220054 size units

# Checking the quantile distribution
quantile(property_master$size_units,seq(0,1,0.01), na.rm = T)
# 99% of the data constitutes of less than 55636 size units. 

median(property_master$size_units, na.rm = T)
# 50% of this variables consitutes of properties that have 188 and less size units.
#--------------------------------------------#

# year_built #

# Checking the range
range(property_master$year_built, na.rm = T)
# The range is between 1 to 2021 build year.
# Clearly 1 cannot be a year when the property was build.

# Checking the quantile distribution
quantile(property_master$year_built,seq(0,1,0.01), na.rm = T)
# 90% of the data constitutes of properties that are built after 1963. 
#--------------------------------------------#

# Account prop and Deal prop are dataset that have ids in general.
# Hence we are not going to do any descriptive stats on them.

#--------------------------------------------#
#      SUMMARY OF UNIVARIATE ANALYSIS
#--------------------------------------------#

# Buyer_book - Almost 80% of the times customer prefer an investement advisory.
# Servicing_contracts - Almost 98% of the times we do not have contracts to service customers loans.
# CMBS - Nearly 100% of the times customers are not involved in Mortgage Backed Securities.
# Consultant - None of the customers prefer consulting for buying properties.
# Correspondent - More than 99% of the times customers haven't bought anything from the origination secondary market.
# Foreign - Only 6 to 7% of the properties have attracted foreign investment
# Master_servicer - Almost all the times customers do not take the service of a master services that mostly service loans from pool to maturity.
# Lender_book - Around 55% of times customers do not prefer taking loans from private lenders, but 45% of the times customers see this as an option
# to count on.
# Loan_sales_book - 65% of the customers prefer this option, as they don't mind selling their property, while the property still has a pending loan
# amount attached to it.
# Loan_servicing - None of the customers prefer mortgage servicing as a option.
# Active_deals - Around 7% of customners do not have any active deals, almost 1/3 of customers have 1 active deal and less than that. Only 1% of
# the data has deals numbering more than 280.
# Activity_count - Around 9% of customners do not have any activity. Only 1% of the data have activity numbering more than 110848.
# Num_deals_as_client - Around 50% of times company hasn't hired anyone to get the deal. 99% of the data constitutes of less than 80 deals for
# which company hired.
# Num_deals_as_investor - Around 9% of times company was not the investor. 99% of the data constitutes of less than 186 deals for which company
# invested.
# Number_of_properties - Around 10% of times customers do not have any property. 99% of the data constitutes of less than 294 properties that
# customers own.
# Number_of_related_deals - Around 6% of times customers do not have any related deals. 99% of the data constitutes of less than 336 related deals
# for customers.
# Number_of_related_properties - Around 19% of times customers have only one related property. 99% of the data constitutes of less than 381 related
# properties for customers.
# Number_of_won_deals_as_client - Around 54% of times customers have won a deal. 99% of the data constitutes of less than 69 won deals for
# customers.
# Investor_type - The highest number (1517) of investors are private investors/developers followed by advisors (326) and banks (320).
#--------------------#

# Deal_type - The top three deal types are: Fixed Rate - 12740, Property Sale - 4729, Floating Rate - 3115
# Deal_update_flag - 97% of the times the deal_update_flag has a true state.
# Fiscalyear - We see that most of the deals have come in 2002 (7058) followed by 2015 (2270).
# Platform - We see that most of the times platform is Debt (18685) followed by Investment_Sales (5187).
# Property_group - The top three property_group are: Multi-Housing - 8798, Office - 5670, Retail - 5293
# Property_type - The top three property_type are: apartments - 8272, Office - 5616, Retail - 5293 
#--------------------#

# Building Status - "Existing" building status has the highest frequency. We see that for 8217 properties building status is not recorded.
# Class - For 6948 properties the class is not known. There are 5430 properties that come under class A.
# Sale_status - Most of the properties (16501) that are sold have status as "Z"
# Property_type_1 - Top three property types are: Multi-Housing - 5627, Office - 3744, Retail - 3474
# Region__c - Top two regions where properties are listed are: Southwest - 4778 & West - 4083
# Num_buildings - Around 54% of times number of building is only 1. 99% of the data constitutes of less than 50 buildings. 75% of this variables
# consitutes of properties that have 6 and less buildings.
# Num_floors - Around 32% of times number of floors is only 1. 99% of the data constitutes of less than 35 floors. 78% of this variables consitutes
# of properties that have 4 and less floors.
# Num_parking_spaces - 99% of the data constitutes of less than 2030 parking spaces. 66% of this variables consitutes of properties that have 365
# and less parking spaces.
# occupancy_rate - 3% of the properties have no occupancy. 1/3 of the properties have 100 occupancy. 25% of this variables consitutes of properties
# that have 87 and less occupants.
# Size_units - 99% of the data constitutes of less than 55636 size units. 50% of this variables consitutes of properties that have 188 and less
# size units.
# Year_built - The range is between 1 to 2021 build year. Clearly 1 cannot be a year when the property was build. 90% of the data constitutes of
# properties that are built after 1963.
#--------------------------------------------#

# Looking at the requirement, we will have to do a clustering of customers to see
# what all customers fall under which bucket.

# We will use a k-means clustering technique to arrive at the clusters.
# The steps that we will follow are.


# 1. Take only the numerical variables.
# 2. Treatement of missing variables.
# 3. Scaling up the variables if required.


#--------------------------------------------#
#       Clustering the accounts
#--------------------------------------------#

# Removing the logical and categorical variables.
accounts_new <- accounts[,-c(4:14)]

# Scaling the variables
accounts_new[, -c(1)] <- scale(accounts_new[, -c(1)])

# Creating a new dataframe
accounts_new1 <- accounts_new[,-1]

# Finding the optimal value of K
r_sq<- rnorm(20)

# Here we are creating a function to understand how many clusters will be good for accounts dataset
for (number in 1:20){clus <- kmeans(accounts_new1, centers = number, nstart = 50)
r_sq[number]<- clus$betweenss/clus$totss
}
# betweenss is the inter-cluster sum of squares of the distance
# and totss is measures the total spread in the data by calculating
# the total sum of squares of distance

plot(r_sq)
# We see that forming clusters with 4,5 & 6 would be good as they are at the bend.

# Creating the clusters
cluster4_acc <- kmeans(accounts_new1, centers = 4, iter.max = 50, nstart = 50)
# cluster sizes are 27,2519,6,204

cluster5_acc <- kmeans(accounts_new1, centers = 5, iter.max = 50, nstart = 50)
# cluster sizes are 240,25,6,2475,10

cluster6_acc <- kmeans(accounts_new1, centers = 6, iter.max = 50, nstart = 50)
# cluster sizes are 233,19,2467,23,10,4

# Appending the cluster id to accounts_new dataset for all three clusters created.
accounts_new_c4 <- cbind(accounts_new,cluster4_acc$cluster)
colnames(accounts_new_c4)[10]<- "ClusterID"
summary(factor(accounts_new_c4$ClusterID))
# 1    2    3    4 
# 27 2519    6  204

accounts_new_c5 <- cbind(accounts_new,cluster5_acc$cluster)
colnames(accounts_new_c5)[10]<- "ClusterID"
summary(factor(accounts_new_c5$ClusterID))
# 1    2    3    4    5 
# 240   25    6 2475   10


accounts_new_c6 <- cbind(accounts_new,cluster6_acc$cluster)
colnames(accounts_new_c6)[10]<- "ClusterID"
summary(factor(accounts_new_c6$ClusterID))
# 1    2    3    4    5    6 
# 233   19 2467   23   10    4


# Now lets check in which bucket does our test datasets account id fall.
t1 <- accounts_new_c4[,c(1,10)]
t2 <- accounts_new_c5[,c(1,10)]
t3 <- accounts_new_c6[,c(1,10)]
t1_merge <- merge(test_data, t1, by.x = "id_accs", by.y = "id_accs", all.x = TRUE)
colnames(t1_merge)[2]<- "Cluster_4"
t1_merge <- merge(t1_merge, t2, by.x = "id_accs", by.y = "id_accs", all.x = TRUE)
colnames(t1_merge)[3]<- "Cluster_5"
t1_merge <- merge(t1_merge, t3, by.x = "id_accs", by.y = "id_accs", all.x = TRUE)
colnames(t1_merge)[4]<- "Cluster_6"


summary(factor(t1_merge$Cluster_4))
# 1  2  4 
# 10  2 17
summary(factor(t1_merge$Cluster_5))
# 1  2  4  5 
# 17  9  2  1
summary(factor(t1_merge$Cluster_6))
# 1  2  3  4  5 
# 14  4  2  8  1

#--------------------------------------------#

new_account <- accounts[,-c(4:14)]
new_acc_c4 <- cbind(new_account,cluster4_acc$cluster)
colnames(new_acc_c4)[10]<- "ClusterID"
c4_group <- group_by(new_acc_c4, ClusterID)
c4_summ <- summarise(c4_group, Mean_active_deal=mean(active_deals), Mean_activity_count=mean(activity_count), 
                     Mean_properties=mean(number_of_properties))

plot_grid(ggplot(c4_summ, aes(x= factor(ClusterID), y=Mean_active_deal)) + geom_bar(stat = "identity"),
          ggplot(c4_summ, aes(x= factor(ClusterID), y=Mean_activity_count)) + geom_bar(stat = "identity"),
          ggplot(c4_summ, aes(x= factor(ClusterID), y=Mean_properties)) + geom_bar(stat = "identity"))

# This cluster with 4 buckets looks promising for bucket 1 and 4 where most of our test datasets id lies.
#--------------------------------------------#

new_acc_c5 <- cbind(new_account,cluster5_acc$cluster)
colnames(new_acc_c5)[10]<- "ClusterID"
c5_group <- group_by(new_acc_c5, ClusterID)
c5_summ <- summarise(c5_group, Mean_active_deal=mean(active_deals), Mean_activity_count=mean(activity_count), 
                     Mean_properties=mean(number_of_properties))

plot_grid(ggplot(c5_summ, aes(x= factor(ClusterID), y=Mean_active_deal)) + geom_bar(stat = "identity"),
          ggplot(c5_summ, aes(x= factor(ClusterID), y=Mean_activity_count)) + geom_bar(stat = "identity"),
          ggplot(c5_summ, aes(x= factor(ClusterID), y=Mean_properties)) + geom_bar(stat = "identity"))

# Does not look as promising as cluster 4
#--------------------------------------------#

new_acc_c6 <- cbind(new_account,cluster6_acc$cluster)
colnames(new_acc_c6)[10]<- "ClusterID"
c6_group <- group_by(new_acc_c6, ClusterID)
c6_summ <- summarise(c6_group, Mean_active_deal=mean(active_deals), Mean_activity_count=mean(activity_count), 
                     Mean_properties=mean(number_of_properties))

plot_grid(ggplot(c6_summ, aes(x= factor(ClusterID), y=Mean_active_deal)) + geom_bar(stat = "identity"),
          ggplot(c6_summ, aes(x= factor(ClusterID), y=Mean_activity_count)) + geom_bar(stat = "identity"),
          ggplot(c6_summ, aes(x= factor(ClusterID), y=Mean_properties)) + geom_bar(stat = "identity"))

# Does not look as promising as cluster 4
# We will go with cluster 4

#--------------------------------------------#
# Adding cluster number to account prop dataset
#--------------------------------------------#

# Creating a new dataset with account id and cluster id
clus_4 <- accounts_new_c4[,c(1,10)]

# Creating a new dataset by removing duplicates from property id column
prop_uni <- account_prop[!duplicated(account_prop[c("id_props")]),]

# Merging newly created datasets
account_prop_new <- merge(prop_uni, clus_4, by.x = "id_accs", by.y ="id_accs", all.x = TRUE)

sum(is.na(account_prop_new$ClusterID))
# 0 NA's should be case and it is.

# Adding a new column called Not_Available_for_sale
account_prop_new[, "Sales_Status"] <- "Not_Available_for_sale"

#--------------------------------------------#
#       Clustering the properties
#--------------------------------------------#

# Taking only the numeric variables
prop_num_sub <- property_master[,c(1,11:16,19,21:23)]

prop_num_sub <- na.omit(prop_num_sub)

# Scaling the variables
prop_num_sub[, -c(1)] <- scale(prop_num_sub[, -c(1)])

# Creating a new dataframe
prop_num_sub1 <- prop_num_sub[,-1]

# Here we are creating a function to understand how many clusters will be good for accounts dataset
for (number in 1:20){clus <- kmeans(prop_num_sub1, centers = number, iter.max = 30,  nstart = 50)
r_sq[number]<- clus$betweenss/clus$totss
}
# betweenss is the inter-cluster sum of squares of the distance
# and totss is measures the total spread in the data by calculating
# the total sum of squares of distance

plot(r_sq)

# Lets run the K-maens now on property dataset by creating 10 clusters as that is
# where the curve took the bend.
cluster10_prop <- kmeans(prop_num_sub1, centers = 10, iter.max = 50, nstart = 50)

# Adding the cluster numbers to all other variables by creating a new dataset. 
prop_new_c10 <- cbind(prop_num_sub,cluster10_prop$cluster)
colnames(prop_new_c10)[10]<- "ClusterID_10_prop"

# Taking only property id and cluster id
prop_new_c10 <- prop_new_c10[,c(1,12)]

# Merging prop id, accs id , cluster id of accounts and cluster id of prop.
prop_new_c10 <- merge(prop_new_c10, account_prop_new, by.x = "id_props", by.y = "id_props", all.x = TRUE)
colnames(prop_new_c10)[2]<- "ClusterID_prop"
colnames(prop_new_c10)[6]<- "ClusterID_accs"

# Removing id, id accs and id deals from this dataset
prop_new_c10 <- prop_new_c10[,-c(3:5)]

# Making the NA's in sales status column as Available
prop_new_c10$Sales_Status <- as.character(prop_new_c10$Sales_Status)
prop_new_c10$Sales_Status[which(is.na(prop_new_c10$Sales_Status))] <- "Available"

# Creating a concat field by taking both the cluster id's
prop_new_c10$concat_id <- do.call(paste, c(prop_new_c10[c("ClusterID_prop","ClusterID_accs")], sep = "|"))

# The reason why we concatenated the id is to find which id combination is
# holding the highest number for property-account. We will use these buckets
# as the properties to be advised for the accounts.

# First we will subset on sales status by taking not available for sales.
# from there we will aggregate to see the numbers and find the bucket with
# highest number of properties for each account cluster.

not_available <- subset(prop_new_c10, Sales_Status == "Not_Available_for_sale")
not_available_final <- not_available %>% group_by(concat_id) %>% count(concat_id)
# Remember that our account cluster ids are 1, 2, 3 after checking the combinations
# we see that for for 3 acc cluster, 4 prop cluster has the highest freq. similarly
# for 1 acc cluster 4 prop cluster and for 2 acc cluster its 1 prop cluster.
# Hence from available list we will take
# cluster 4 properties for cluster 3 accounts
# cluster 4 properties for cluster 1 accounts
# cluster 1 properties for cluster 2 accounts

# Creating the buckets from the avaiable list
available_4_na <- subset(prop_new_c10, concat_id == "4|NA")
ava_4_na <- as.data.frame(available_4_na$id_props)
available_1_na <- subset(prop_new_c10, concat_id == "1|NA")
ava_1_na <- as.data.frame(available_1_na$id_props)

# Merging the cluster ids of accounts in the test_data set 
test_data <- merge(test_data, clus_4, by.x = "id_accs", by.y = "id_accs", all.x = TRUE)
summary(factor(test_data$ClusterID))
# 1  2  3 
# 10 17  2

#----------------------------------------------# cluster 1 accounts

v1 <- "0012A000023XlbuQAC"
exp1 <- cbind(data.frame(id_accs = v1), available_4_na$id_props)

v2 <- "0012A000023XlkNQAS"
exp2 <- cbind(data.frame(id_accs = v2), available_4_na$id_props)

v3 <- "0012A000023Xo0OQAS"
exp3 <- cbind(data.frame(id_accs = v3), available_4_na$id_props)

v4 <- "0012A000023Y4bYQAS"
exp4 <- cbind(data.frame(id_accs = v4), available_4_na$id_props)

v5 <- "0012A000023Y9gNQAS"
exp5 <- cbind(data.frame(id_accs = v5), available_4_na$id_props)

v6 <- "0012A000023YB67QAG"
exp6 <- cbind(data.frame(id_accs = v6), available_4_na$id_props)

v7 <- "0012A000023YBFRQA4"
exp7 <- cbind(data.frame(id_accs = v7), available_4_na$id_props)

v8 <- "0012A000023YDnWQAW"
exp8 <- cbind(data.frame(id_accs = v8), available_4_na$id_props)

v9 <- "0012A000029ZzX1QAK"
exp9 <- cbind(data.frame(id_accs = v9), available_4_na$id_props)

v10 <- "0012A00002AXHJUQA5"
exp10 <- cbind(data.frame(id_accs = v10), available_4_na$id_props)

#----------------------------------------------# cluster 3 accounts

v11 <- "0012A000023XmmFQAS"
exp11 <- cbind(data.frame(id_accs = v11), available_4_na$id_props)

v12 <- "0012A000023Xp5LQAS"
exp12 <- cbind(data.frame(id_accs = v12), available_4_na$id_props)

#----------------------------------------------# cluster 2 accounts

v13 <- "0012A000023Xlh9QAC"
exp13 <- cbind(data.frame(id_accs = v13), available_1_na$id_props)

v14 <- "0012A000023XlhdQAC"
exp14 <- cbind(data.frame(id_accs = v14), available_1_na$id_props)

v15 <- "0012A000023XlnqQAC"
exp15 <- cbind(data.frame(id_accs = v15), available_1_na$id_props)

v16 <- "0012A000023XlnWQAS"
exp16 <- cbind(data.frame(id_accs = v16), available_1_na$id_props)

v17 <- "0012A000023XlWaQAK"
exp17 <- cbind(data.frame(id_accs = v17), available_1_na$id_props)

v18 <- "0012A000023Xm35QAC"
exp18 <- cbind(data.frame(id_accs = v18), available_1_na$id_props)

v19 <- "0012A000023XmRwQAK"
exp19 <- cbind(data.frame(id_accs = v19), available_1_na$id_props)

v20 <- "0012A000023XoRjQAK"
exp20 <- cbind(data.frame(id_accs = v20), available_1_na$id_props)

v21 <- "0012A000023Xp4LQAS"
exp21 <- cbind(data.frame(id_accs = v21), available_1_na$id_props)

v22 <- "0012A000023XrTNQA0"
exp22 <- cbind(data.frame(id_accs = v22), available_1_na$id_props)

v23 <- "0012A000023XwMRQA0"
exp23 <- cbind(data.frame(id_accs = v23), available_1_na$id_props)

v24 <- "0012A000023Y1rtQAC"
exp24 <- cbind(data.frame(id_accs = v24), available_1_na$id_props)

v25 <- "0012A000023YAupQAG"
exp25 <- cbind(data.frame(id_accs = v25), available_1_na$id_props)

v26 <- "0012A000023YKsuQAG"
exp26 <- cbind(data.frame(id_accs = v26), available_1_na$id_props)

v27 <- "0012A000023YLFWQA4"
exp27 <- cbind(data.frame(id_accs = v27), available_1_na$id_props)

v28 <- "0012A000026WwqxQAC"
exp28 <- cbind(data.frame(id_accs = v28), available_1_na$id_props)

v29 <- "0012A00002JF3YmQAL"
exp29 <- cbind(data.frame(id_accs = v29), available_1_na$id_props)

final_1 <- as.data.frame(rbind(exp1,exp2,exp3,exp4,exp5,exp6,exp7,exp8,exp9,exp10))
final_2 <- as.data.frame(rbind(exp11,exp12))
final_3 <- as.data.frame(rbind(exp13,exp14,exp15,exp16,exp17,exp18,exp19,exp20,exp21,
                               exp22,exp23,exp24,exp25,exp26,exp27,exp28,exp29))


colnames(final_1)[2]<- "id_prop"
colnames(final_2)[2]<- "id_prop"
colnames(final_3)[2]<- "id_prop"

final_main <- as.data.frame(rbind(final_1,final_2,final_3))
write.csv(final_main,"Submission_Satya_Mishra.csv",row.names = FALSE)

##---------END OF CODE----------##
























