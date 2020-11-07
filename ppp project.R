# Project by Neil Collins | 8/18/2020

# Data Source: https://www.kaggle.com/susuwatari/ppp-loan-data-paycheck-protection-program

# This project aims to analyze trends in the Paycheck Protection Program 
# and to generate predictive models based on demographics to predict the likelihood of receiving a loan.

# Overarching Business Question: What factors played the largest roles in PPP loan distribution?

ppp <- read.csv('c://Users/Neil/Desktop/PPP_data_150k_plus.csv')

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

distppp$LoanRange <- sub(". ","", distppp$LoanRange)

distppp$LoanRange <- sub(".*-","", distppp$LoanRange)

distppp$LoanRange <- removeWords(distppp$LoanRange," million")

distppp$LoanRange <- sub(",000","",distppp$LoanRange)

distppp$LoanRange <- as.numeric(distppp$LoanRange)

for(i in 1:length(distppp$LoanRange)){
  
  if(distppp$LoanRange[i] < 350){
    distppp$LoanRange[i] <- distppp$LoanRange[i] * 1000000
  }
  
  else if(distppp$LoanRange[i] == 350){
    distppp$LoanRange[i] <- distppp$LoanRange[i] * 1000
  }
  
}

colnames(distppp)[1] <- "MaxLoan"

# Turning NonProfit into a dummy variable

for(i in 1:length(distppp$NonProfit)){
  if(distppp$NonProfit[i] == 'Y'){
    distppp$NonProfit[i] <- 1
  }
  else if(distppp$NonProfit[i] == ''){
    distppp$NonProfit[i] <- 0
  }
}

# Generate dummy variables for every business type, potentially lender

# Multivariate regression analysis utilizing necessary variables

# Post-hoc analysis

















