# ======================================================================================================
# Contains all generic data preparation routines / functions which are generic and can be invoked from 
# other programs.


# ======================================================================================================
# Function getDataFrame(df, startDate, endDate, tIDColName = "ID", tDateColName = "Date", tAmountColName = "Amount")
# 
# Description
#   Prepare the input data frame of dataset's transaction records for RFM scoring.
#   a) Remove duplicate records with same Customer ID.
#   b) Find the most recent date per Customer ID, then calculate the days to [endDate] to get the Recency data.
#   c) Calculate the quantity of transaction per customer to get the Frequency data.
#   d) Sum up the amount spent by each customer and divide by Frequency to get the average amount spent per transaction, 
#      this becomes the Monetary data.
# 
# Arguments explained
#   df: A data frame of dataset's transaction records with Customer ID, Dates, and the Amount of money spend per transaction.
#   startDate: The start date of transaction; Records occur after the start date will be kept.
#   endDate: The end date of transaction; Records occur after the end date will be removed.  It works with the start date to set a time.
# 
# Scope
#   tIDColName: column that contains Customer IDs in the input data frame.
#   tDateColName: column that contains the transaction dates in the input data frame.
#   tAmountColName: column that contains the amount spend by each customer per transaction in the input data frame.
# 
# Returns
#   A new data frame with 3 new colunmns: "Recency", "Frequency", and "Monetary".
#     "Recency" [num]: The number of days from the most recent transaction of a customer to endDate.
#     "Frequency" [num]: The number of transactions of a customer during the period between startDate and endDate.
#     "Monetary" [num]: The average amount of money spend per transaction by a customer during the period between startDate and endDate.
# 
# ---------------------------------------------------------------------
getDataFrame <- function(df, startDate, endDate, tIDColName = "ID", tDateColName = "Date", tAmountColName = "Amount") {
  # sort data frame by date descendingly
  df <- df[order(df[ , tDateColName], decreasing = TRUE), ]
  
  # remove records outside date range [startDate, endDate]
  # == dplyr POTENTIALS ==
  df <- df[df[ , tDateColName] >= startDate, ]
  df <- df[df[ , tDateColName] <= endDate, ]
  
  # remove rows with duplicate Customer ID
  # == dplyr POTENTIALS ==
  newdf <- df[!duplicated(df[ , tIDColName]), ]
  
  # calc. the Recency [days] to the endDate; smaller the Recency == more recent
  # == dplyr POTENTIALS ==
  Recency <- as.numeric(difftime(endDate, newdf[ , tDateColName], units = "days"))
  # combine Days column to the newdf data frame
  newdf <- cbind(newdf, Recency)
  
  # sort data frame by ID to fit the return order of table() and tapply()
  newdf <- newdf[order(newdf[ , tDateColName]), ]
  
  # calc. the Frequency
  tmp <- as.data.frame(table(df[ , tIDColName]))
  Frequency <- tmp[ , 2]
  # combine Frequency to the newdf data frame
  newdf <- cbind(newdf, Frequency)
  
  # calc. the Monetary
  tmp <- as.data.frame(tapply(df[ , tAmountColName], df[ , tIDColName], sum))
  Monetary <- tmp[ , 1] / Frequency
  # combine Monetary to the newdf data frame
  newdf <- cbind(newdf, Monetary)
  
  return(newdf)
}	
### END Function: getDataFrame() #######################################################################


# ======================================================================================================
# Function
#   getIndepRFMScore(df, r = 5, f = 5, m = 5)
# 
# Description
#   Scoring the Recency, Frequency, and Monetary in r, f, and m into specified amount of bins (5) independently.
# 	Note: This function calls scoring()
# 
# Arguments explained
# 	df: A data frame returned by getDataFrame().
# 	r: The highest point of Recency.
# 	f: The highest point of Frequency.
# 	m: The highest point of Monetary.
# 
# Returns
# 	A new data frame with 4 new columns of "R_score", "F_score", "M_score", and "Total_score".
# 
# ---------------------------------------------------------------------
getIndepScore <- function(df, r = 5, f = 5, m = 5) {
  if (r <= 0 || f <= 0 || m <= 0) return
  
  # sort data set for Recency with Recency (ascending) - Frequency (descending) - Monetary (descending)
  df <- df[order(df$Recency, -df$Frequency, -df$Monetary), ]
  R_score <- scoring(df, "Recency", r)
  # combine R_score to the df data frame
  df <- cbind(df, R_score)
  
  # sort data set for Frequency with Recency (descending) - Frequency (ascending) - Monetary (descending)
  df <- df[order(-df$Recency, df$Frequency, -df$Monetary), ]
  F_score <- scoring(df, "Frequency", f)
  # combine R_score to the df data frame
  df <- cbind(df, F_score)
  
  # sort data set for Monetary with Recency (descending) - Frequency (descending) - Monetary (ascending)
  df <- df[order(-df$Recency, -df$Frequency, df$Monetary), ]
  M_score <- scoring(df, "Monetary", f)
  # combine R_score to the df data frame
  df <- cbind(df, M_score)
  
  # sort data frame by R_score, F_score, and M_score (all in descending order)
  df <- df[order(-df$R_score, -df$F_score, -df$M_score), ]
  
  # calc. the Total Score
  Total_score <- c((100 * df$R_score) + (10 * df$F_score) + df$M_score)
  
  # combine Total_score to the df data frame
  df <- cbind(df, Total_score)
  
  return(df)
}
### END Function: getIndepRFMScore() ###################################################################


# ======================================================================================================
# Function
#   scoring(df, column, r = 5)
# 
# Description
# 	This function invoke by getIndepScore()
# 
# Recency: Lower recency value = More recent = Higher R_Score
# Frequency: Lower freq. value = Lower return visits = Higher F_Score
# Monetary: Lower monetary value = Lower spent / visit / customer = Higher M_Score
# 
# ---------------------------------------------------------------------
scoring <- function(df, column, r = 5) {
  # Determined number of measures of data frame
  len <- dim(df)[1]
  # create the number of rows found in passed-in data frame with "0" as value
  score <- rep(0, times = len)
  
  # Determine the quantity of rows per bin (default: 5 bins)
  nr <- round(len / r)
  if (nr > 0) {
    # seperate the rows by r aliquots
    rStart <- 0
    rEnd <- 0
    # interate through each bin (5)
    for (i in 1:r) {
      # initialize start row number and end row number
      rStart = rEnd + 1
      
      # skip one "i" if the rStart is already in the i+1 or i+2 or ... scope.
      if (rStart > i * nr) next
      
      # i is at last bin
      if (i == r) {
        if (rStart <= len) rEnd <- len else next
      } else {
        rEnd <- i * nr
      }
      
      # set Recency score | since data set is sorted (ascending), 1st-bin has scores=5, 2nd-bin has score=4, ..., 5th-bin has score=1
      score[rStart : rEnd] <- r - i + 1
      
      # ensure customer with same Recency have the same score
      s <- rEnd + 1
      if ((i < r) & (s <= len)) {
        for (u in s:len) {
          if (df[rEnd, column] == df[u, column]) {
            score[u] <- r - i + 1
            rEnd <- u
          } else {
            break;
          }
        }
      }
    }
  }
  
  return(score)
}
### END Function: scoring() ############################################################################


# ======================================================================================================
# Function
#   getScoreWithBreaks(df, r, f, m)
# 
# Description
# 	Scoring the Recency, Frequency, and Monetary in r, f, and m into certain bin, where number of bins are calculated based on series of breaks specified by user.
# 
# Arguments
# 	df: A data frame returned by getDataFrame().
# 	r: A vector of Recency breaks
# 	f: A vector of Frequency breaks
# 	m: A vector of Monetary breaks
# 
# Returns
# 	A new data frame with 4 new columns of "R_score", "F_score", "M_score", and "Total_score".
# 
# ---------------------------------------------------------------------
getScoreWithBreaks <- function(df, r, f, m) {
  
  # scoring the Recency
  len = length(r)
  R_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,R_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=r[i-1]
    }
    p2=r[i]
    
    if(dim(df[p1<df$Recency & df$Recency<=p2,])[1]>0) df[p1<df$Recency & df$Recency<=p2,]$R_Score = len - i+ 2
  }
  
  # scoring the Frequency	
  len = length(f)
  F_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,F_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=f[i-1]
    }
    p2=f[i]
    
    if(dim(df[p1<df$Frequency & df$Frequency<=p2,])[1]>0) df[p1<df$Frequency & df$Frequency<=p2,]$F_Score = i
  }
  if(dim(df[f[len]<df$Frequency,])[1]>0) df[f[len]<df$Frequency,]$F_Score = len+1
  
  # scoring the Monetary	
  len = length(m)
  M_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,M_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=m[i-1]
    }
    p2=m[i]
    
    if(dim(df[p1<df$Monetary & df$Monetary<=p2,])[1]>0) df[p1<df$Monetary & df$Monetary<=p2,]$M_Score = i
  }
  if(dim(df[m[len]<df$Monetary,])[1]>0) df[m[len]<df$Monetary,]$M_Score = len+1
  
  #order the dataframe by R_Score, F_Score, and M_Score desc
  df <- df[order(-df$R_Score,-df$F_Score,-df$M_Score),]
  
  # calc. the total score
  Total_Score <- c(100*df$R_Score + 10*df$F_Score+df$M_Score)
  
  df <- cbind(df,Total_Score)
  
  return(df)
}
### END Function: getScoreWithBreaks() #################################################################


# ======================================================================================================
# Function
# 	getPercentages <- function(df, colNames)
# 
# Description
# 	Calculate the probabilities of "Buy"/Repurchase grouped by R, F, M values respectively or in combination.
# 
# Arguments
# 	df: A data frame returned by getDataFrame() w/ calculated Recency, Frequency, and Montetary along with its scores.
#   colNames: a vector of column names to be grouped by, such as c("Requency") or c("Requency","Frequency")
# 
# Returns
#   Data frame with the variables being used to grouped by and the percentages of customers who buy accordingly
# 
# ---------------------------------------------------------------------
require(plyr)
getPercentages <- function(df, colNames){
  
  Var <- c(colNames, "Buy")
  
  df <- df[ ,names(df) %in% Var, drop = F]
  
  a <- ddply(df, Var, summarize, Number = length(Buy))
  b <- ddply(
    a, 
    .(), 
    .fun = function(x) {
      transform(x, Percentage=with(x,round(ave(Number,a[,names(a) %in% Var,drop=F],FUN=sum)/ave(Number,a[,names(a) %in% colNames,drop=F],FUN=sum),2)))
    }
  )
  
  b <- b[b$Buy == 1, -1]
  
  return(b)
}
### END Function: getPercentages() #####################################################################


# ======================================================================================================
# Function
# 	getCLTV <- function(r, f, rev, cost, n, periods, dr, pModel)
# 
# Description
# 	Calculate CLTV (visualize decision tree) based on Recency and Frequency
# 
# Arguments
# 	r: 			Recency value (e.g., r = 0).
# 	f: 			Frequency value (e.g., f = 1).
# 	rev: 		Anticipated/Expected revenue from customer.
# 	n: 			Num. of customers with the same Recency and Frequency value.
# 	cost: 		Associated cost per period for each potential customer (Buy or No-Buy).
# 	periods: 	Num. of period(s) customer will stay before churning.
# 	dr: 		Discount Rate.
# 	pModel:		Regression model used to predict the "Buy" rate based on Recency, Frequency, or Monetary.
# 
# Returns
# 	Customer's value after n period(s)
# 
# ---------------------------------------------------------------------
getCLTV <- function(r, f, rev, cost, n, periods, dr, pModel) {
  df <- data.frame(period = c(0), r = c(r), f = c(f), n = c(n), value = c(0))
  
  for (i in 1:periods) {
    backstep <- df[df$period == i-1, ]
    nrow <- nrow(backstep)
    
    for (j in 1:nrow) {
      r <- backstep[j, ]$r
      f <- backstep[j, ]$f
      n <- backstep[j, ]$n
      p <- predict(pModel, data.frame(Recency = r, Frequency = f), type = "response")[1]
      buyers <- n * p
      
      # Predict odds of a "Buy" for this period
      df <- rbind( df, c(i, 0, f+1, buyers, buyers*(rev-cost) / (1+dr)^i ))
      
      # Predict odds of a "No-Buy" for this period
      df <- rbind( df, c(i, r+1, f, n-buyers, (n-buyers)*(0-cost) / (1+dr)^i ))
    }
  }
  
  return(sum(df$value))
}
# END Function: getCLTV() ##############################################################################
