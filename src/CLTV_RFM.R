# ======================================================================================================
# Objective and Background - 
#             Calculating Customer Lifetime Value (CLTV) with Recency, Frequency, and Monetary (RFM)
#             CLTV by definition is "the present value of future cash flow attributed to a customer's tenure with a company", 
#             can be determined by variety of approaches. Based on a simple [equation](), one can calculate the cumulative 
#             profit (value) from a customer based on assumptions such as retention rate and historical profit margin from customers. 
#             However, a customer's retention can be influenced by factors such as demographics (age, geography, education background), 
#             consumer behavior of vintage data and RFM (Recency, Purchase Frequency, Monetary Contribution), competitions, 
#             performance and bureau scores, product holding information (no of active accounts, no of loans, no of credit cards), 
#             peer influences, etc..  As a result, a dynamic approach should be taken to account for such variations.
# 
# By definition, RFM represents -
# 	- R(ecency): how recently did customer purchase any product or services?
# 	- F(rquency): how often do customer purchase product / services?
# 	- M(onetary Value): how much do they spend (each time on an average)?
#
# The determination of Recency, Frequency, and Monetary of a customer, should provide insights to the following:
# 	1) How to segment customers to determine who are more likely to response to advertisements / promotions / to purchase etc?
# 	2) Which type of customers to send advertisements in order to breakeven and make profit?
# 
# Here, computation of CLTV is done by predicting the retention rate (r) of customers' future purchasing cycle 
# using Logistic Regresssion based on his/her Recency of purchase, purchase Frequency, and Monetary contribution from past purchases.
# 
# Code Workflow
# 	1) Initialization and Data set preperation.
# 	2) Set time frame for CLTV computation
# 		a. Imported data set scope (becomes historical / training data).
# 		b. Observe / forecast period (similar to testing data).
# 	3) Prepare the input data frame of sample dataset(masked for demonstration) transaction records for RFM scoring and/or CLTV computation.
# 	4) Run Logistics Regression model on Customer's Purchase (DV) based on his/her Recency, Frequency, and/or Monetary trends (IV).
# 	5) Calculate customer's value (total profit in present value) within the forecast period (user specify).
# 
# Data
# 	The sample data set, consists of 69,659 transaction records, by 23,570 unique customers, captures the purchase records 
#   between Jan 1997 and June 1998.
# 
# ======================================================================================================


# ======================================================================================================
# Import required libraries
library(ggplot2)
library(ggvis)
library(dplyr)
library(plotly)
py <- plotly()

# Set working directory here<>
setwd("C:/Users/Projects")

# Invoke common data prep functions
source ("./CLTV_dataPrepFunctions.R")
# Function getDataFrame(df, startDate, endDate, tIDColName = "ID", tDateColName = "Date", tAmountColName = "Amount") 
# Function getIndepRFMScore(df, r = 5, f = 5, m = 5)
# Function scoring(df, column, r = 5)
# Function getScoreWithBreaks(df, r, f, m)
# Function getPercentages <- function(df, colNames)
# Function getCLTV <- function(r, f, rev, cost, n, periods, dr, pModel)


# ======================================================================================================
# Main program 
## 1) Initialization and Data set preperation
# Set your working directory accordingly
#dir_Input = "C:/Users/Projects"

# Import raw data
setwd(dir_Input)
my_df <- read.table("my_master.txt", header = FALSE)

# Create columns [Customer ID], [Transaction Date], [Amount paid by customer]
# -- Data set covers 01Jan1997 - 30Jun1998 --
my_df <- as.data.frame(cbind(my_df[,1], my_df[,2], my_df[,4]))
names <- c("ID", "Date", "Amount")
names(my_df) <- names
# transform Date column from text to date format
my_df[,2] <- as.Date( as.character(my_df[,2]), "%Y%m%d")
## End of 1) ===========================================================================================


# ======================================================================================================
## 2) Set time frame for CLTV calculation 
# 2a) Historical transaction scope (18 months) == 'Training data'
startDate_Hist <- as.Date("19970101", "%Y%m%d")
# endDate_Hist <- as.Date("19980701", "%Y%m%d")
endDate_Hist <- as.Date("19980228", "%Y%m%d")

# 2b) Forecast transaction scope (2 months)
startDate_Forecast <- as.Date("19980301", "%Y%m%d")
endDate_Forecast <- as.Date("19980430", "%Y%m%d")

# 2c) Retrieve data set with distinct customer
Hist_df <- getDataFrame(my_df, startDate_Hist, endDate_Hist)					
Forecast_df <- getDataFrame(my_df, startDate_Forecast, endDate_Forecast)	
## End of 2) ===========================================================================================


# ======================================================================================================
## 3) Prepare the input data frame of dataset's transaction records for RFM scoring and/or CLTV calculation.
# Set purchasing cycle from days to 2-months unit (similar to specified forecast period)
Forecast_Period <- as.numeric(difftime(endDate_Forecast, startDate_Forecast))
Hist_df$Recency <- Hist_df$Recency %/% Forecast_Period

# Categorize Monetary values into bins with size of $10 each
breaks <- seq(0, round(max(Hist_df$Monetary) + 9), by = 10)		# Sample reference: http://www.endmemo.com/program/R/seq.php
Hist_df$Monetary <- as.numeric( cut(Hist_df$Monetary, breaks, labels = FALSE) ) # Sample reference: https://www.r-bloggers.com/r-function-of-the-day-cut-2/

# Add "Buy" / "No Buy" column to data frame: Hist_df[]
Buy <- rep(0, nrow(Hist_df))
Hist_df <- cbind(Hist_df, Buy)

# Identify customers who purchased during the forecast period (01Mar1998 - 30Apr1998)
Hist_df[Hist_df$ID %in% Forecast_df$ID, ]$Buy <- 1

# Create Training data set: Training_df
Training_df <- Hist_df
## End of 3) ===========================================================================================


# ======================================================================================================
## Compute RFM Score 

# Calc. the "Buy" percentage based on Recency
pRecency <- getPercentages(Training_df, "Recency")
pFreq <- getPercentages(Training_df, "Frequency")
pMonetary <- getPercentages(Training_df, "Monetary")

#Remove NAs if any in these data frames
pRecency_withoutNAs <- delete.na(pRecency)
pFreq_withoutNAs <- delete.na(pFreq)
pMonetary_withoutNAs <- delete.na(pMonetary)

# plot and draw fit curves of Percentage ~ r,f,m
par( mfrow = c(1, 3), oma = c(0,0,2,0) )  # set canvas for 1 row - 3 columns, with specified outer margin area

plot(pRecency_withoutNAs$Recency, pRecency_withoutNAs$Percentage * 100, xlab = "Recency", ylab = "Prob of Purchasing (%)")
lines(lowess(pRecency_withoutNAs$Recency, pRecency_withoutNAs$Percentage * 100), col="blue", lty = 2)

plot(pFreq_withoutNAs$Frequency, pFreq_withoutNAs$Percentage * 100, xlab = "Frequency", ylab = "Prob of Purchasing (%)")
lines(lowess(pFreq_withoutNAs$Frequency, pFreq_withoutNAs$Percentage * 100), col="blue", lty = 2)

plot(pMonetary_withoutNAs$Monetary, pMonetary_withoutNAs$Percentage * 100, xlab = "Monetary", ylab = "Prob of Purchasing (%)")
lines(lowess(pMonetary_withoutNAs$Monetary, pMonetary_withoutNAs$Percentage * 100), col="blue", lty = 2)

title("Percentages ~ (Recency, Frequency, Monetary)", y=10, outer=TRUE)

# logistics regression on Purchase Pctg ~ Recency
r.glm = glm(Percentage~Recency, family = quasibinomial(link = "logit"), data = pRecency_withoutNAs)

# logistics regression on Purchase Pctg ~ Frequency
f.glm = glm(Percentage~Frequency, family = quasibinomial(link = "logit"), data = pFreq_withoutNAs)

# logistics regression on Purchase Pctg ~ Monetary
m.glm = glm(Percentage~Monetary, family = quasibinomial(link = "logit"), data = pMonetary_withoutNAs)

par( mfrow = c(1, 1) )
# Sample reference: Interpreting Logistics Regression: https://stats.idre.ucla.edu/r/dae/logit-regression/
model <- glm(Buy ~ Recency + Frequency, data = Training_df, family = quasibinomial(link = "logit"))
pred_01 <- predict(model, data.frame(Recency = c(0), Frequency = c(1)), type = "response")
pred_02 <- predict(model, data.frame(Recency = c(0), Frequency = c(2)), type = "response")
pred_03 <- predict(model, data.frame(Recency = c(0), Frequency = c(3)), type = "response")
## End of "Computation of RFM Score" ===================================================================


# ======================================================================================================
## 4) Run Logistics Regression model on Customer's Purchase (DV) based on his/her Recency, Frequency, and/or Monetary trends (IV).
LogReg_Results.Buy_RF <- glm(Buy ~ Recency + Frequency, data = Training_df, family = quasibinomial(link = "logit"))
## End of 4) ===========================================================================================


# ======================================================================================================
## 5) Calculate customer's value (total profit in present value) within the forecast period (user specify).
# Set observations / known-values
r = 0 			# init. Recency state (e.g., 0)
f = 1 			# init. Frequency state (e.g., 1)
Rev = 100 		# Anticipated/Expected revenue from customer.
Cost = 0 		# Associated cost for each potential customer (Buy or No-Buy) per period.
n = 1 			# No of customers with the same Recency and Frequency value.
periods = 3 	# No of period(s) customer will stay before churning.  Note, periods was specified as time range for Forecast data set above.
dr = 0.02 		# Discount rate
model = LogReg_Results.Buy_RF

# Cust_Value <- getCLTV(0, 1, 100, 0, 1, 3, 0.02, model)
Cust_Value <- getCLTV(r, f, Rev, Cost, n, periods, dr, model)
print(Cust_Value)
## End of 5) ============================================================================================

# End of main program ===================================================================================
