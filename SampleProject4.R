# The following R code fetches data from Compustat and CRSP at WRDS

library(RPostgres)
library(tidyverse)
library(datasets)
library(psych)
library(ExPanDaR)
library(data.table)
library(pROC)		# this is needed for calculating the AUC

#initialize WRDS data server connection
wrds<-dbConnect(Postgres(),  
                host='wrds-pgdata.wharton.upenn.edu',
                port=9737,
                user='winstonzhong1',	            # replace $$$$ with your WRDS user name			
                password='Zhongjiajun21',            # replace $$$$ with your WRDS password
                sslmode='require',
                dbname='wrds')

# read the crsp permno and compustat gvkey
crsp_daily_comp_link <- read.table(file="crsp_daily_comp_link.csv",sep=",",colClasses=c("integer","character"),header=TRUE)

#Fetch COMPUSTAT data and merge with he linktable above
res <- dbSendQuery(wrds, "
      select gvkey, datadate, cusip, fyear, wcap, at,
             lt, ebit, re, mkvalt, sale, ni, act, lct
      
      from compa.funda

      where datadate between '1962-01-01' and '2020-12-31';")

comp <- dbFetch(res, n=-1)

dbClearResult(res)

crsp_comp<- merge(crsp_daily_comp_link, comp, by.x="gvkey", by.y="gvkey", sort = TRUE)

crsp_comp$na_count <- apply(is.na(crsp_comp), 1, sum)

crsp_comp <- crsp_comp[order(crsp_comp$gvkey,crsp_comp$datadate, crsp_comp$na_count),]

crsp_comp <- crsp_comp %>% distinct(gvkey, datadate, .keep_all = TRUE)  #There can be more than one line of data for a firm-year, leave only the one with least NAs 


# Fetch monthly CRSP data to construct indep. var.
res <- dbSendQuery(wrds, "
      select permno, date, prc, ret, shrout
      
      from crsp_a_stock.msf

      where date between '1961-01-01' and '2020-12-31';")

crsp_ms <- dbFetch(res, n=-1)
dbClearResult(res)

# Fetch monthly index data to construct indep. var.
res <- dbSendQuery(wrds, "
      select caldt, vwretd, vwretx, totval
      
      from crsp_a_indexes.msic

      where caldt between '1961-01-01' and '2020-12-31';")

crsp_msi <- dbFetch(res, n=-1)
dbClearResult(res)
names(crsp_msi)[names(crsp_msi) == "caldt"] <- "date"

# Fetch begin trading date to drop those begin trading outside 1962-2020 and construct age var.
res <- dbSendQuery(wrds, "
      select distinct permno, begexchdate
      
      from crsp_a_stock.mseexchdates;")

crsp_beg <- dbFetch(res, n=-1)
dbClearResult(res)

crsp_beg <- crsp_beg[order(crsp_beg$permno),]

crsp_comp <- merge(crsp_comp, crsp_beg, by.x="permno", by.y="permno", sort = TRUE)

 crsp_comp<- crsp_comp[ which(crsp_comp$begexchdate>='1962-01-01'
                         & crsp_comp$begexchdate<='2020-12-31'), ] #drop those begin trading before 1962 and after 2020

crsp_comp$begyear<- format(crsp_comp$begexchdate, "%Y") #extract the year a firm begin trading to compute age of a firm

crsp_ms <- merge(crsp_ms, crsp_msi, by.x = "date", by.y = "date", sort = TRUE)

#firm_permno = unique(crsp_ms$permno)
crsp_ms$year <- format(crsp_ms$date,"%Y")
#year_list = unique(crsp_ms$year)

# run regressions on monthly returns of each firm in each year to obtain the standard deviation of the residuals
#
# the following code works, but takes a long time.
#
# system.time({
# for (i in firm_permno){
#    for (j in year_list){
#       if(sum(crsp_ms$year == j & crsp_ms$permno == i & !is.na(crsp_ms$ret), na.rm=TRUE)==12){
#          tmpdata <- crsp_ms[ which(crsp_ms$permno==i& crsp_ms$year==j), ]
#          tmpmodel <- lm(tmpdata$ret ~ tmpdata$vwretd)
#          tmpsd <- sd(residuals(tmpmodel))
#          crsp_ms$sd[crsp_ms$permno ==i&crsp_ms$year==j] <- tmpsd 
#       }
#    }
# }
# })
# 
#     user   system  elapsed 
# 19261.59  2573.35 21873.11
# this code works, but took 6 hours to run on a PC !!!

sdreg <- function(x,y) {
   df <- data.frame(x=x, y=y)
   return( sd(summary(lm(y~x,data=df))$residuals) )
}
system.time({
   tmp <- data.table(crsp_ms)
   setorderv(tmp,c("permno","year"))
   tmp <- tmp[!is.na(tmp$ret),]                                     # remove NAs
   tmp1 <- tmp[, .N, by=c("permno","year")]                         # count number of monthly returns in each firm-year
   tmp1 <- tmp1[N==12,]                                             # keep only the firm-years that have 12 monthly returns
   tmp2 <- tmp[tmp1,,on=c("permno","year")]                         # extract monthly returns for firm-years with 12 monthly returns
   tmp3 <- tmp2[, .(sd=sdreg(vwretd,ret)), by=c("permno","year")]   # run regression of monthly return on market return by firm-year and keep the std dev of the residuals -- this is the sigma variable
   tmp4 <- tmp3[tmp, , on=c("permno","year")]                       # put the resulting sigma variable back into the full data set
})
#   user  system elapsed 
# 310.90    0.89  309.78 
# this code gives the same answers, but took only 310 seconds to run on the same PC !!!


crsp_sd <- tmp4[,c("permno","year","sd")]%>% distinct(permno, year, .keep_all = TRUE)
crsp_ms<-merge(crsp_ms, crsp_sd, by = c("permno","year"), sort = TRUE)
rm(tmp, tmp1, tmp2, tmp3, tmp4)

crsp_ms$ret[is.na(crsp_ms$ret)] <- crsp_ms$vwretd[is.na(crsp_ms$ret)] #replace missing firm monthly returns with the market return of that month

crsp_msi$year <- format(crsp_msi$date,"%Y")
crsp_msi$fyear <- as.numeric(crsp_msi$year)-1  #one year lag to merge with fyear in comp
crsp_msi$month<- format(crsp_msi$date,"%m")
crsp_msi$vwretd <- log(1+crsp_msi$vwretd) #convert to log monthly return
crsp_msi <- crsp_msi[order(crsp_msi$fyear,crsp_msi$month),]
crsp_msi$vwretda <- ave(crsp_msi$vwretd, crsp_msi$fyear, FUN=cumsum) #cumulate monthly return to annual return
crsp_msi$vwretda <- exp(crsp_msi$vwretda)-1
crsp_msi <- crsp_msi[ which(crsp_msi$month == 12),] #keep only end of year market return and cap.
crsp_msi <- crsp_msi[ c("fyear", "vwretda", "totval")] #keep only year, return and market cap.

crsp_comp <- merge(crsp_comp, crsp_msi, by.x="fyear", by.y="fyear", sort = TRUE)

crsp_ms$fyear <- as.numeric(crsp_ms$year)-1  #one year lag to merge with fyear in comp
crsp_ms$month<- format(crsp_ms$date,"%m")
crsp_ms <- crsp_ms[order(crsp_ms$permno, crsp_ms$fyear,crsp_ms$month),]
crsp_ms$reta <- ave(crsp_ms$ret, crsp_ms$permno, crsp_ms$fyear, FUN=cumsum) #cumulate monthly return of firm into annual return
crsp_ms$reta <- exp(crsp_ms$reta)-1
crsp_ms <- crsp_ms[ which(crsp_ms$month == 12),] #only end of year firm level market cap.
crsp_ms_formerge <- crsp_ms[ c("fyear","permno", "reta", "sd")]

crsp_comp <- merge(crsp_comp, crsp_ms_formerge, by=c("fyear", "permno"), sort = TRUE)


crsp_me  <- crsp_ms["permno"]
crsp_me$me <- abs(crsp_ms$prc)*crsp_ms$shrout #compute firm market value 
crsp_me$fyear <- format(crsp_ms$date,"%Y")
crsp_comp <- merge(crsp_comp, crsp_me, by=c("fyear", "permno"), sort = TRUE)

# create Altman's variables

crsp_comp$wc_ta <- crsp_comp$wcap/crsp_comp$at

crsp_comp$re_ta <- crsp_comp$re/crsp_comp$at

crsp_comp$ebit_ta <- crsp_comp$ebit/crsp_comp$at

crsp_comp$me_tl <- crsp_comp$me/crsp_comp$lt/1000

crsp_comp$s_ta <- crsp_comp$sale/crsp_comp$at

# create Zmijewski's variables

crsp_comp$ni_ta <- crsp_comp$ni/crsp_comp$at

crsp_comp$tl_ta <- crsp_comp$lt/crsp_comp$at

crsp_comp$ca_cl <- crsp_comp$act/crsp_comp$lct

# create Shumway's variables

crsp_comp$ri_rm <- crsp_comp$reta - crsp_comp$vwretda

crsp_comp$size <- log(crsp_comp$me/crsp_comp$totval) 

crsp_comp$age <- log(as.numeric(format(crsp_comp$datadate,"%Y"))-as.numeric(crsp_comp$begyear)+1)

# truncate 1st and 99th percentile 

crsp_comp_trun <- treat_outliers(data.frame(crsp_comp[24:34],crsp_comp[22]), percentile = 0.01, truncate = TRUE) # truncate the first and last percentile of outliers

describe(crsp_comp_trun, skew=FALSE)

# create data for firm-years
firmdata <- crsp_comp[, c("permno","fyear","wc_ta","re_ta",      
                      "ebit_ta","me_tl","s_ta","ni_ta","tl_ta",
                      "ca_cl","sd","ri_rm","size","age")]
colnames(firmdata)[colnames(firmdata)=="sd"] <- "sigma"
colnames(firmdata)[colnames(firmdata)=="age"] <- "lage"
# reset 0.01 and 0.99 percentiles
for (i in 3:ncol(firmdata)) {
  tmp <- firmdata[,i]
  p01 <- quantile(tmp,0.01,na.rm=TRUE)
  p99 <- quantile(tmp,0.99,na.rm=TRUE)
  reset <- ifelse(tmp<p01,p01,ifelse(tmp>p99,p99,tmp))
  firmdata[,i] <- reset
}

firmdata[, numna := apply(is.na(firmdata),1,sum)]
setorder(firmdata,permno,fyear,numna)
firmdata = firmdata[,.SD[1],by=.(permno,fyear)]
firmdata[, numna:=NULL]

summary(firmdata)
#Step 1: create the firmdata dataset
firmdata <- data.table(firmdata)
setorderv(firmdata, c("permno","fyear"))


#Step 2: read in the failed firms and the bankruptcy filing years
failfirm <- fread("fail_firms.csv")
failfirm
head(failfirm)

#Step 3: create the status variable
firmdata$status = '0'
firmdata <- failfirm[firmdata, on =.(permno)][fyear <= fail.year -1] 
setorderv(firmdata, c("permno","fyear"))
firmdata[, i.status := replace(i.status, .N, "1"), by = 'permno']
View(firmdata)

#Step 4: Create training and test datasets
training.data <- firmdata[between(fyear, 1962,2002),]
View(training.data)
testing.data <- firmdata[between(fyear, 2003, 2020),]

#start question 1
#Question 1

#Q1. a # of Failed firm
#How many firm-year observations are in the training set? How many firms failed in the training set?
nrow(training.data)
#[1] 7820
nrow(training.data[i.status=="1"])
#[1] 568
#How many firm-year observations are in the testing set? How many firms failed in the testing set?

nrow(testing.data)
#[1] 2292
nrow(testing.data[i.status=="1"])
#[1] 347

#Q1.b Altman’s variables
training.N <- training.data[,.SD[order(fyear)][.N], by = permno]
View(training.N)
#Estimate the one-period model for Altman’s variables as follows:
Alt.1 <- glm( as.numeric(i.status) ~ wc_ta + re_ta + ebit_ta + me_tl + s_ta + lage, family = binomial, data = training.N)

#Predict the observation of each firm in the testing set:
pred <- predict(Alt.1, newdata=testing.data, type="response")

#Calculate the AUC (area under the curve) using the pROC package:
Alt.1.auc <- roc(testing.data$i.status,pred,algorithm=2)$auc
Alt.1.auc
#Area under the curve: 0.5852

#Estimate the multi-period model for Altman’s variables as follows:regressor = wc_ta + re_ta + ebit_ta + me_tl + s_ta + lage
Alt.m <- glm( as.numeric(i.status) ~ wc_ta + re_ta + ebit_ta + me_tl + s_ta + lage, family = binomial, data = training.data)

# predict the last observation of each firm in the testing set
pred <- predict(Alt.m, newdata=testing.data, type="response")

#Calculate the AUC (area under the curve):
Alt.m.auc <- roc(testing.data$i.status,pred,algorithm=2)$auc
Alt.m.auc
#Area under the curve: 0.6681

#Which model has better accuracy (i.e. higher AUC)?
#model with multiple-period has a higehr AUC

#Q1.c Zmijewski’s variables
#Single period
Alt.2 <- glm( as.numeric(i.status) ~ ni_ta + tl_ta + ca_cl + lage, family = binomial, data = training.N)
pred <- predict(Alt.2, newdata=testing.data, type="response")
Alt.2.auc <- roc(testing.data$i.status,pred,algorithm=2)$auc
Alt.2.auc
#Area under the curve: 0.6676

#Multi-Period
Alt.3 <- glm( as.numeric(i.status) ~ ni_ta + tl_ta + ca_cl + lage, family = binomial, data = training.data)
pred <- predict(Alt.3, newdata=testing.data, type="response")
Alt.3.auc <- roc(testing.data$i.status,pred,algorithm=2)$auc
Alt.3.auc
#Area under the curve: 0.7109
#Which model is more accurate, the one-period logit or the multi-period logit?
# Multi-period is more accurate

#Q1.d Shumay’s variables

#Single period
Alt.4 <- glm( as.numeric(i.status) ~ ni_ta + tl_ta + size + ri_rm + sigma + lage, family = binomial, data = training.N)
pred <- predict(Alt.4, newdata=testing.data, type="response")
Alt.4.auc <- roc(testing.data$i.status,pred,algorithm=2)$auc
Alt.4.auc
#Area under the curve: 0.8673
#Multi-Period
Alt.5 <- glm( as.numeric(i.status) ~ ni_ta + tl_ta + size + ri_rm + sigma + lage, family = binomial, data = training.data)
pred <- predict(Alt.5, newdata=testing.data, type="response")
Alt.5.auc <- roc(testing.data$i.status,pred,algorithm=2)$auc
Alt.5.auc 
#Area under the curve: 0.8668

#Which model is more accurate, the one-period logit or the multi-period logit?
# Single period logit is more accurate 
s.table <- c(Alt.1.auc, Alt.m.auc, Alt.2.auc, Alt.3.auc, Alt.4.auc, Alt.5.auc)

s.table1 <- c("Alt.1.auc", "Alt.m.auc", "Alt.2.auc", "Alt.3.auc", "Alt.4.auc", "Alt.5.auc")

s.table2 <- rbind(s.table1, s.table)
View(s.table2)

#Q1.e Comparing across all 6 logit models in 1b-1d, which one is the most accurate in terms of AUC?
#Shumay's variables with one period 
#end question 1

#start question 2
#Question 2

#Q2 Use optimx to find the two parameters, (𝜆, 𝜎), to calibrate the Rendleman-Bartter model
# define function to print tree
prt.tree <- function(tree,digit=2) {
  nt <- nrow(tree)
  # transpose tree
  trantree <- t(tree)
  nt1 <- 2*nt-1
  bintree  <- matrix(rep("",nt1*nt),nrow=nt1,ncol=nt)
  # convert to bin tree
  i1 <- nt
  for (j in 1:nt) {
    i1 <- nt-j+1
    for (i in 1:j) {
      bintree[i1,j] <- as.character(round(trantree[i,j],digit))
      i1 <- i1 + 2
    }
  }
  rownames(bintree) <- rep("",nt1)
  colnames(bintree) <- rep("",nt)
  return(noquote(bintree))
}


Rendleman.Bartter.model.qtree <- function(lambda, sigma, years, delt){
 #up probability 
  N <- floor(years/delt)+1
  u <- exp(sigma*sqrt(delt))
  d <- exp(-sigma*sqrt(delt))
  # create up probability tree
  # assume all up probabilities from function
  qtree  = matrix(0, nrow=N+1, ncol=N+1)
  for (i in 1:(N+1)) {
    for (j in 1:i) {
      qtree[i,j] = (exp(lambda*delt)-d)/(u-d)
    }
  }

  return(qtree) 
  
}
#set up z tree
Rendleman.Bartter.model.ztree <- function(r0, lambda, sigma, years, delt){
  N    <- floor(years/delt)+1   # number of steps
  ztree  <- matrix(0, nrow=N+1, ncol=N+1)
  ztree[1,1] <- r0
  u = exp(sigma * sqrt(delt))
  d = exp(-sigma * sqrt(delt))
  for (i in 2:N) {
    i1 <- i-1
    ztree[i,1:i1] <- ztree[i-1,1:i1]*u
    ztree[i,i] <- ztree[i-1,i-1]*d
  }
  return(ztree)
}

#set up ptree
BM_ZBP <- function(T,qtree,ztree,delt) {
  # Create price tree for zero-coupon bond paying $1 in year T
  # using a given instantaneous spot rate tree and up probability tree
  #
  # Args: 
  #   T:     The maturity of the zero coupon bond (in years) 
  #   qtree: The up probability tree
  #   ztree: The instantaneous spot rate tree
  #   delt:  length of a time step (in years)
  #
  # Returns:
  #   Price tree of the zero coupon bond
  
  N <- floor(T/delt)+1
  
  # make sure N < nrow(ztree)
  if (N>nrow(ztree)) return(matrix(NA,nrow=N,ncol=N))
  if (N>nrow(qtree)) return(matrix(NA,nrow=N,ncol=N))
  
  ptree <- matrix(0,nrow=N,ncol=N)
  ptree[N,1:N] <- 1    
  for (i in (N-1):1) {
    i1 <- i+1
    ptree[i,1:i] <- exp(-delt*ztree[i,1:i])*
      ( qtree[i,1:i]*ptree[i+1,1:i]+ (1-qtree[i,1:i])*ptree[i+1,2:i1])
  }
  return(ptree)
}
#Step 1: Start with a hypothetical set of values for (𝜆, 𝜎), something like 𝜆 = 0 and 𝜎 = 0.01
lambda <- 0
sigma <- 0.01
#Step 2: Build the one-period spot rate tree, with 𝑟0,0(1) = 0.03, and nodes (i,j) are given in the 
#diagram above. The up probability, q, is the same at all nodes. 
r0 <- 0.03
delt <- 0.5
years <- 5


#Step 3: Use this spot rate tree to price 1-, 2-, …, 10- period zero coupon bonds.
#Zero coupon bond spot rate 
zcbs <- c(0.03, 0.035, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.05125, 0.0525, 0.0525)
time <- seq(0.5, 5, 0.5)
data <- data.frame(z = zcbs, t = time)
names(data) <- c("z", "t")

data

#Step 4: From the zero coupon bonds, calculate the ten corresponding spot rates – these are the 
#“model” spot rates.

z <- data[,1]
t <- data[,2]
N <- length(z)
qtree <- Rendleman.Bartter.model.qtree(lambda, sigma, years, delt)
prt.tree(qtree)
ztree <- Rendleman.Bartter.model.ztree(r0, lambda, sigma, years, delt)
prt.tree(ztree)
#create a vector to store the model result

zmod <- rep(r0, N)
for (imat in 1:N) {
  yrs <- t[imat]
  ZBP   <- BM_ZBP(yrs,qtree,ztree,delt)
  zmod[imat] <- 2*((1/ZBP[1,1])^(1/(t[imat]*2))-1)
}
tmp <- cbind(data[,c(2,1)],zmod)

#Step 5: Find the sum of squared errors of the difference between the model spot rates and the 
#observed spot rates, given in the table above
tmp$err <- round(tmp$z - tmp$zmod,8)
tmp
round(sum(tmp$err^2),8)
#Step 6: Use optimx() to minimize the sum of squared errors over the two parameters, (𝜆, 𝜎).
sse <- function(parm,data) {
  z = data[,1]
  t = data[,2]
  N = length(z)
  delt = 0.5
  years= N*delt
  r0=0.03
  lambda = parm[1]
  sigma = parm[2]
  zmod = rep(r0,N)
  qtree <- Rendleman.Bartter.model.qtree(lambda, sigma, years, delt)
  ztree <- Rendleman.Bartter.model.ztree(r0, lambda, sigma, years, delt) 
  for (imat in 1:N) {
    yrs <- t[imat]
    ZBP   <- BM_ZBP(yrs,qtree,ztree, delt)
    zmod[imat] <- 2*((1/ZBP[1,1])^(1/(t[imat]*2))-1)
  }
  
  sse <- sum( (z-zmod)^2 )*1000000    # scale sse up by 1000000 for use in optimization
  if(sigma < 0| qtree[1,1]<0 |qtree[1,1]>1) sse = 9e20
  return(sse)
  
}
# run optimizer

library(optimx)
parm <- c(lambda, sigma)
nparm<- 2
#lower bound for lambda and sigma is - infinity 
lb <- c(-Inf,-Inf)
#optimx method used "Nelder-Mead"
opt2 <- optimx(parm,sse,method="Nelder-Mead",lower=lb,
               control=list(maxit=5000),data=data)
print(opt2)
parm <- c(opt2$p1,opt2$p2)
round(parm,4)
#[1] 0.4373 1.1580
#lambda of 0.4373 and sigma of 1.1580 gives the minimum sum of squared error
sse(parm,data)
#sum square error is the difference between the observed spot rate and optmixed spot rate  
#[1] 8.658699
#end question 2



#start question 3
#Question 3

# define DATE() function
DATE <- function(yyyy,mm,dd) {
  dte  <- as.Date(sprintf("%i-%i-%i",yyyy,mm,dd),format="%Y-%m-%d")
  return(dte)
}
as.Date2 <- function(x) {
  tryfmt <- c("%Y-%m-%d","%m/%d/%Y","%Y/%m/%d","%b %d,%Y")
  return(as.Date(x,tryFormats=tryfmt))
}
zcb.price <- function(zcb.yield,ttm,freq=2) {
  return( 100/(1+zcb.yield/freq)^(freq*ttm) )
}
zcb.yield <- function(zcb.price,ttm,freq=2) {
  return( freq * ( (100/zcb.price)^(1/(freq*ttm)) - 1 ) )
}

#Q3 Calculate the fair value of this security, using the LIBOR spot rates and Eurodollar futures prices 
# on settlement 2023-02-01

# step 1: Set up libor data.table
libor <- fread("TeamAssignment4_libor.csv")  
settle.yyyy <- 2023
settle.mm <- 2
settle.dd <- 1
settle <- DATE(settle.yyyy,settle.mm,settle.dd)

names(libor) <- c("term","spot")
libor[, start.date := settle ]
seq.month <- seq(from=settle,to=DATE(settle.yyyy+1,settle.mm,settle.dd),by="month")
libor[, end.date := c(settle+1,seq.month[c(2,4,7,13)])]
libor[, ndays := as.numeric(end.date-start.date)]
libor
# first floating rate is the current 3m LIBOR spot
swap <- data.table(period = c(0:8),
                   pay.date = seq(DATE(settle.yyyy, settle.mm,settle.dd),
                                  DATE(settle.yyyy+2, settle.mm,settle.dd), by = "3 month"))
swap[, ndays := c(0,as.numeric(diff(swap$pay.date))) ]
swap[, L3 := NA]
swap$L3[1] <- libor[term=="3 Month",]$spot[1]

swap

# step 2: interpolate remaining L3 
# read eurodollar futures
edf <- fread("TeamAssignment4_edfut.csv")
settle.edf <- DATE(2023,2,1)

names(edf) <- c("FutMatDt","FutPr")
nedf <- nrow(edf)
edf[, FutMatDt := as.Date2(FutMatDt)]
edf[, start.date := as.Date2(FutMatDt)+2 ]          # T+2 settlement
edf[, forw    := 1-FutPr/100 ]

# include today's 3 month libor (spot) rate
tmp <- data.table(start.date=settle,
                  forw=libor[term=="3 Month",]$spot[1])
tmp <- rbind(tmp,edf[,.(start.date,forw)])
setorderv(tmp,c("start.date"))
tmp

# interpolate 3 month forward rates
L3 <- as.data.frame(spline(x=tmp$start.date,y=tmp$forw,xout=swap$pay.date,method="natural"))$y
swap$L3[2:nrow(swap)] <- L3[2:length(L3)]
swap


notional   <-1000000
swap$cf.float <- 0
for (i in 2:nrow(swap)) {
  swap$cf.float[i] <- swap$L3[i-1] * swap$L3[i-1]/0.03*notional
}

# discount factors
swap$disfac <- 1
for (i in 2:nrow(swap)) {
  swap$disfac[i] <- swap$disfac[i-1] / (1+swap$L3[i-1]*swap$ndays[i]/360)
}

swap[.N, cf.float := cf.float + notional]

swap[, pv.cf := disfac*cf.float]

round(sum(swap$pv.cf),3)
#[1] 1402703
View(swap)
#end question 3