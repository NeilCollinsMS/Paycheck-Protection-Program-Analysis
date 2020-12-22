# Project by Neil Collins

# Data Source: https://www.kaggle.com/susuwatari/ppp-loan-data-paycheck-protection-program

# This project aims to analyze trends in the Paycheck Protection Program 
# and to generate predictive models based on demographics to predict the likelihood of receiving a loan.

# Overarching Business Question: What factors played the largest roles in PPP loan distribution?

ppp <- read.csv('c://Users/Neil/Desktop/PPP_data_150k_plus.csv')

# Given that I am using some unoptimized cleaning methods, I like to runthe below code periodically so I have a CSV backup if my workspace is cleared on accident
# library(openxlsx)
# write.xlsx(distppp, "c://Users/Neil/Desktop/filteredPPP.csv")

# Data Exploration

head(ppp) #Taking a look at the first 6 columns of the data

sapply(ppp,class) #finding the data type of each column

summary(ppp)

# Data Cleaning - Utilizes Tidyverse / dplyr
# I will be utilizing the max value of loans available in the data set's range as the exact loan distribution is unknown. 
# In this section I will be cleaning the extra characters from the formatted data so that it can be more easily utilized in a regression.

library(tidyverse)

distppp <- select(ppp, LoanRange, City, State, NAICSCode, BusinessType, NonProfit, JobsRetained, Lender) 

# Business name, address, zip code too broad for now. RaceEthnicity, Gender, Veteran too many missing data points, CD is repeat info

# DateApproved left out initially but may be looked at later though all approvals were within an approximate 1 month range before the funds depleted

# Removing extra characters, spaces, coercing numeric

library(tm)

distppp$LoanRange <- sub(". ","", distppp$LoanRange) # Removes the letter and space before each value

distppp$LoanRange <- sub(".*-","", distppp$LoanRange) # Removes the - and everything prior

distppp$LoanRange <- removeWords(distppp$LoanRange," million") # Removes "million" from the million string values

distppp$LoanRange <- sub(",000","",distppp$LoanRange) # Removes the ,000 from 350k values so the values can be coerced to numeric

distppp$LoanRange <- as.numeric(distppp$LoanRange) # Coerce values to numeric so they can be manipulated

for(i in 1:length(distppp$LoanRange)){
  
  if(distppp$LoanRange[i] < 350){
    distppp$LoanRange[i] <- distppp$LoanRange[i] * 1000000
  }
  
  else if(distppp$LoanRange[i] == 350){
    distppp$LoanRange[i] <- distppp$LoanRange[i] * 1000
  }
  
}

# This could be sped up with vectorization, but for the purpose of this project I will be using the less efficient for loops.

colnames(distppp)[1] <- "MaxLoan"

for(i in 1:length(distppp$NonProfit)){
  if(distppp$NonProfit[i] == 'Y'){
    distppp$NonProfit[i] <- 1
  }
  else if(distppp$NonProfit[i] == ''){
    distppp$NonProfit[i] <- 0
  }
}

distppp$City <- NULL # At this point, I realized there were far too many cities to encode or create meaningful buckets, state will suffice

# Filling in blank business types with placeholder "Unknown"

distppp$BusinessType <- as.factor(distppp$BusinessType)

distppp$BusinessType <- sub("^$","Unknown",distppp$BusinessType)

# Cleaning up column names so dummy encoding is a bit nicer looking. 

distppp$BusinessType <- sub("Limited  Liability Company" ,"",distppp$BusinessType) #There was something funky going on with this string, had to break it down

distppp$BusinessType <- gsub("[()]","",distppp$BusinessType)

distppp$BusinessType <- gsub(" ","_",distppp$BusinessType)

distppp$BusinessType <- sub("Employee_Stock_Ownership_PlanESOP" ,"ESOP",distppp$BusinessType)

distppp$BusinessType <- sub("Rollover_as_Business_Start-Ups_ROB" ,"Rollover",distppp$BusinessType)

distppp$BusinessType <- sub("Non-Profit_Childcare_Center" ,"NP_Child_Care",distppp$BusinessType)

distppp$BusinessType <- sub("Non-Profit_Organization" ,"Non_Profit",distppp$BusinessType)

distppp$BusinessType <- sub("Self-Employed_Individuals" ,"Self_Employed",distppp$BusinessType)

distppp$BusinessType <- sub("Subchapter_S_Corporation" ,"Subchapter_S",distppp$BusinessType)

# Dummy Coding 

for(i in unique(distppp$BusinessType)){
  distppp[paste("Type", i, sep = "_")] <- ifelse(distppp$BusinessType == i, 1, 0)}

distppp <- distppp[!(distppp$Type_Rollover == 1),] # Only 4 companies, not enough to work with and I want to avoid skewing

distppp$Type_Rollover <- NULL 

distppp <- distppp[!(distppp$Type_Tenant_in_Common == 1),] # Only 20 companies, same reasoning as above

distppp$Type_Tenant_in_Common <- NULL

distppp <- distppp[!(distppp$Type_Joint_Venture == 1),] # Only 76 entries out of tens of thousands. Not quite enough for me.

distppp$Type_Joint_Venture <- NULL

# Type_Independent_Contractors has 157 entries, I've accepted that as a valid baseline, but I'll adjust it in the regression if it causes issues.

# There are over 4,000 individual lenders, obviously too many to dummy encode. I'll decide what to do with these at a later date. 

# My current plan is to run the regressions with them intact and filter to generate different models at a later point. 

# Replacing NULL values in the Jobs Retained Column with 0

distppp$JobsRetained[is.na(distppp$JobsRetained)] <- 0

#################################

# General Statistical Information:

count(distppp,distppp$BusinessType) # Number of each type of business

count(distppp,distppp$State) # Number records from each state

distppp <- distppp[!(distppp$State == 'XX'),] # Unsure was XX was, but there were only 16 records so I'm going to live without it

count(distppp,distppp$NAICSCode) 

# After doing some research into these industry clasificiation codes (NAICS), I'm going to leave them as is...for the moment

unique(distppp$ï..MaxLoan)

count(distppp,distppp$ï..MaxLoan) # Smaller loans more common...makes sense

#################################

# General Regression Model 1















