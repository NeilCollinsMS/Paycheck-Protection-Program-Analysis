# REVIEW OF MY FAILURES: 

# 1. Using straight linear regression when using the maximums made this a categorical type problem

# 2. Not pre-splitting my data into test and training data sets & not using cross validation

# 3. Not looking into MSE/RMSE/etc...

##############################

# General Regression Model 1

# With this model, I will form a baseline and look for multicolinearity. I'll work towards an accurate general model, and then look into buckets. 

# The Lender column eats way too much RAM, I am going to exclude until/if I find an efficient bucketing solution.

# I also don't currently intend to dummy encode all 50 states + DC and territories, so I will give that its own exploration later.

# I might remove JobsRetained as it is more of an effect than a cause...but I might draw some conclusions from it...stay tuned

hist(distppp$JobsRetained) # Checking for outliers quickly

Model1 <- lm(ï..MaxLoan ~ NAICSCode + NonProfit + JobsRetained + Type_Non_Profit + Type_Subchapter_S + Type_Corporation + Type_Unknown
              + Type_Partnership + Type_Professional_Association + Type_Sole_Proprietorship + Type_ESOP + Type_Trust + Type_Limited_Liability_Partnership
              + Type_NP_Child_Care + Type_Independent_Contractors + Type_Self_Employed, data = distppp)

summary(Model1)

# Type_Unknown, Type_NP_Child_care, Type_Independent_Contractors, Type_Self_Employed not statistically significant

Model1b <- lm(ï..MaxLoan ~ NAICSCode + NonProfit + JobsRetained + Type_Non_Profit + Type_Subchapter_S + Type_Corporation
           + Type_Partnership + Type_Professional_Association + Type_Sole_Proprietorship + Type_ESOP + Type_Trust 
           + Type_Limited_Liability_Partnership, data = distppp)

summary(Model1b)

vif(Model1b)

Model1c <- lm(ï..MaxLoan ~ NAICSCode + NonProfit + JobsRetained + Type_Subchapter_S + Type_Corporation
               + Type_Partnership + Type_Professional_Association + Type_Sole_Proprietorship + Type_ESOP + Type_Trust 
               + Type_Limited_Liability_Partnership, data = distppp)

summary(Model1c)

vif(Model1c) # Removed Type_Non_Profit, because it very understandably had high multicollinearity with NonProfit

# VIF was 61+, but now every feature is VIF ~1 so multicollinearity is realistically dealt with. AIC time. 

stepAIC(Model1c)

Model1d <- lm(ï..MaxLoan ~ NAICSCode + NonProfit + Type_Subchapter_S + Type_Corporation
               + Type_Partnership + Type_Professional_Association + Type_Sole_Proprietorship + Type_ESOP + Type_Trust 
               + Type_Limited_Liability_Partnership, data = distppp)

# Jobs Retained has a noticeably higher AIC than the rest of the variables

stepAIC(Model1d)

# AIC actually got worse between these two models, removing NAICSCode on a hunch.

Model1e <- lm(ï..MaxLoan ~ NonProfit + Type_Subchapter_S + Type_Corporation
               + Type_Partnership + Type_Professional_Association + Type_Sole_Proprietorship + Type_ESOP + Type_Trust 
               + Type_Limited_Liability_Partnership, data = distppp)

stepAIC(Model1e)

# Overall Model Evaluation

AIC(Model1) # 19807380
summary(Model1) # R-squared = .4597, F is statistically significant

AIC(Model1b) # 19807377
summary(Model1b) # R-squared = .4597, F is statistically significant

AIC(Model1c) # 19807415
summary(Model1c) # R-squared = .4597, F is statistically significant

# Models 1d and 1e are by far the worst models, removing JobsRetained seems to have triggered this

AIC(Model1d) # 20203740
summary(Model1d) # R-squared = .009819, F is statistically significant

AIC(Model1e) # 20411753
summary(Model1e) # R-squared = .005908, F is statistically significant

#################################

# Targeted Regression Model 1

# At this point, looking at how poor these models are, I'm going to subset the data and see if I can gauge factors more effectively.

sub1_ppp <- subset(distppp, distppp$ï..MaxLoan == 1e+07) # Subsetting by the highest loan values first

count(sub1_ppp,BusinessType)

# Just going to immediately remove Type_Non_Profit because I already know there is high multicollinearity 

Model2 <- lm(ï..MaxLoan ~ NAICSCode + NonProfit + JobsRetained + Type_Subchapter_S + Type_Corporation + Type_Unknown
              + Type_Partnership + Type_Professional_Association + Type_Sole_Proprietorship + Type_ESOP + Type_Trust + Type_Limited_Liability_Partnership
              + Type_NP_Child_Care + Type_Independent_Contractors + Type_Self_Employed, data = sub1_ppp)

summary(Model2)

# Excluded features with too few instances

Model2b <- lm(ï..MaxLoan ~ NAICSCode + NonProfit + JobsRetained + Type_Subchapter_S + Type_Corporation + Type_Partnership + Type_LLC, data = sub1_ppp)

summary(Model2b)

# Neither of these models are generating statistically significant figures. Time to re-evaluate my subsetting and approach.

# I am going to completely remove data points that don't meat the 5 target business type features and see if there is any adjustment

# However, we'll first take the distributions of the maximum loan value among business types. These models may be scrapped, but basic stats go a long way.

count(sub1_ppp,BusinessType)

# Cooperative: 13/4839 = 0.27%

# Corporation: 2005/4839 = 41.43%

# ESOP: 31/4839 = 0.64%

# Limited Liabiliaty Partnership: 76/4839 = 1.57%

# LLC: 1120/4839 = 23.15%

# Non Profit: 407/4839 = 8.41%

# Non Profit Child Care: 3/4839 = 0.06%

# Partnership: 119/4839 = 2.46%

# Professional Association: 18/4839 = 0.37%

# Sole Proprietorship: 28/4839 = 0.58%

# Subchapter_S: 1003/4839 = 20.73%

# Trust: 5/4839 = 0.10%

# Unknown: 11/4839 = .23%

#############################

# Average Jobs Retained by Business Type

aggregate(sub1_ppp[,6],list(sub1_ppp$BusinessType),mean)

aggregate(sub1_ppp[,6],list(sub1_ppp$BusinessType),max)

aggregate(sub1_ppp[,6],list(sub1_ppp$BusinessType),min)

ggplot(sub1_ppp, aes(x = BusinessType, y = JobsRetained)) + stat_summary(fun="mean", geom="bar")

ggplot(sub1_ppp, aes(x=reorder(BusinessType, JobsRetained, mean), y = JobsRetained)) + stat_summary(fun="mean",geom="bar")

# Non Profit Childcare reports an average of 500 saved jobs; which is the data max. This number could be higher or they could just be reporting the max.

# Nonprofit, Subchapter_S, Corporation, Partnership, LLC being left in subset 2

sub2_ppp <- sub1_ppp[!(sub1_ppp$BusinessType == 'Cooperative'),]

sub2_ppp <- sub2_ppp[!(sub2_ppp$BusinessType == 'ESOP'),]

sub2_ppp <- sub2_ppp[!(sub2_ppp$BusinessType == 'Limited_Liability_Partnership'),]

sub2_ppp <- sub2_ppp[!(sub2_ppp$BusinessType == 'NP_Child_Care'),]

sub2_ppp <- sub2_ppp[!(sub2_ppp$BusinessType == 'Professional_Association'),]

sub2_ppp <- sub2_ppp[!(sub2_ppp$BusinessType == 'Sole_Proprietorship'),]

sub2_ppp <- sub2_ppp[!(sub2_ppp$BusinessType == 'Trust'),]

sub2_ppp <- sub2_ppp[!(sub2_ppp$BusinessType == 'Unknown'),]

Model2c <- lm(ï..MaxLoan ~ NAICSCode + NonProfit + JobsRetained + Type_Subchapter_S + Type_Corporation + Type_Partnership + Type_LLC, data = sub1_ppp)

summary(Model2c)

# Scrapping this model, nothing changed, as expected, but I figured I would try. Only NonProfit is even close to being significant. 

# Back to the drawing board, but at least I was able to get some summary statistics of the highest loan distribution

#############################

# At this point, I realized that even though I moved the values to maximums, I was dealing with categorical data

# My multivariate regression models weren't working because I needed to use multinomial logistic regression

Model3 <- multinom(ï..MaxLoan ~ NAICSCode + NonProfit + JobsRetained + Type_Subchapter_S + Type_Corporation + Type_Unknown
              + Type_Partnership + Type_Professional_Association + Type_Sole_Proprietorship + Type_ESOP + Type_Trust + 
                Type_Limited_Liability_Partnership + Type_NP_Child_Care + Type_Independent_Contractors + Type_Self_Employed, data = distppp)

summary(Model3)
coeftest(Model3)

exp(coef(Model3))
head(fitted(Model3))
