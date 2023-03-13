# Fixed Income: Bond Valuation - Partial Period
library(data.table)
library(ggplot2)
library(jrvFinance)

# 
# define DATE() function
DATE <- function(yyyy,mm,dd) {
  dte  <- as.Date(sprintf("%i-%i-%i",yyyy,mm,dd),format="%Y-%m-%d")
  return(dte)
}
as.Date2 <- function(x) {
  tryfmt <- c("%Y-%m-%d","%m/%d/%Y","%Y/%m/%d","%b %d,%Y")
  return(as.Date(x,tryFormats=tryfmt))
}

#----------------------------------------------------

# Question 1
#1a. Find the yield-to-maturity of each bond.
ts <- fread("TeamAssignment1_Q1.csv")

settle <- DATE(2021,12,30)
mature <- DATE(2023,05,15)
coupon <- 0.0175
freq <- 2
price <- 101.66              #This is clean price
conv <- "ACT/ACT"
comp.freq <- 2 
round(bond.yield(settle, mature,coupon,freq = 2, price, conv, comp.freq),6)

settle <- DATE(2021,12,30)
mature <- DATE(2026,05,15)
coupon <- 0.02125
freq <- 2
price <- 103.4355              #This is clean price
conv <- "ACT/ACT"
comp.freq <- 2 
round(bond.yield(settle, mature,coupon,freq = 2, price, conv, comp.freq),6)

settle <- DATE(2021,12,30)
mature <- DATE(2030,05,15)
coupon <- 0.00625
freq <- 2
price <- 93.2175              #This is clean price
conv <- "ACT/ACT"
comp.freq <- 2 
round(bond.yield(settle, mature,coupon,freq = 2, price, conv, comp.freq),6)

settle <- DATE(2021,12,30)
mature <- DATE(2050,05,15)
coupon <- 0.01250
freq <- 2
price <- 83.9550              #This is clean price
conv <- "ACT/ACT"
comp.freq <- 2 
round(bond.yield(settle, mature,coupon,freq = 2, price, conv, comp.freq),6)



#1b. Find the market value of this portfolio on the settlement date of 12/30/2021.
settle <- DATE(2021,12,30)
mature <- DATE(2023,05,15)
coupon <- 0.0175
freq <- 2
price1 <- 101.66              #This is clean price
conv <- "ACT/ACT"
comp.freq <- 2 
accrued_interest1 <- bond.TCF(settle,mature,coupon,freq,conv)$accrued
accrued_interest1
# Full Price
full_price1 <- price1 + accrued_interest1
full_price1

settle <- DATE(2021,12,30)
mature <- DATE(2026,05,15)
coupon <- 0.02125
freq <- 2
price2 <- 103.4355              #This is clean price
conv <- "ACT/ACT"
comp.freq <- 2 
accrued_interest2 <- bond.TCF(settle,mature,coupon,freq,conv)$accrued
accrued_interest2
# Full Price
full_price2 <- price2 + accrued_interest2
full_price2

settle <- DATE(2021,12,30)
mature <- DATE(2030,05,15)
coupon <- 0.00625
freq <- 2
price3 <- 93.2175              #This is clean price
conv <- "ACT/ACT"
comp.freq <- 2 
accrued_interest3 <- bond.TCF(settle,mature,coupon,freq,conv)$accrued
accrued_interest3
# Full Price
full_price3 <- price3 + accrued_interest3
full_price3

settle <- DATE(2021,12,30)
mature <- DATE(2050,05,15)
coupon <- 0.01250
freq <- 2
price4 <- 83.9550              #This is clean price
conv <- "ACT/ACT"
comp.freq <- 2 
accrued_interest4 <- bond.TCF(settle,mature,coupon,freq,conv)$accrued
accrued_interest4

# Full Price
full_price4 <- price4 + accrued_interest4
full_price4

# Market Value
View(ts)
ts$face.value
full_price <- rbind(full_price1/100,full_price2/100,full_price3/100,full_price4/100)

portfolio_value <- sum(ts$face.value*full_price)

portfolio_value

ts1<- data.table(ts)
ts1[,settle := rep(DATE(2021,12,30),4)]
ts1[,Maturity := as.Date2(maturity)]
ts1[, freq := rep(2,4)]


str(ts1)
View(ts1)

settle <- DATE(2021,12,30)

#1c. Find the cash flows of this bond portfolio at each payment date.

New_dt <- ts1[, coupons.dates(settle, Maturity, freq), by = Maturity]
New_dt
setnames(New_dt,"V1","c.dates")

New_dt <- cbind(New_dt, ts1[, bond.TCF(settle, Maturity, coupon, freq, conv)$cf * face.value / 100, by = Maturity])
setnames(New_dt,"V1","cash.flow")
New_dt
New_dt2 <- New_dt[, sum(cash.flow), by = c.dates]
setnames(New_dt2,"V1","cash.flow")

New_dt2

#1d.Find the yield--to-maturity (YTM) of this bond portfolio?
New_dt2
cf <- New_dt2$cash.flow
cf
cash.flow.sum <- sum(New_dt$cash.flow)
cash.flow.sum-sum(ts$face.value)
year <- nrow(New_dt2)/2

coupon <- (cash.flow.sum-sum(ts$face.value))/year/sum(ts$face.value)
coupon

cft <- bond.TCF(settle,mature,coupon,freq,conv)$t
cft

pcf <- c(-portfolio_value, cf)
pcf
pcft <- c(0, cft)
pcft
ytm <- irr(cf=pcf,comp.freq=2,cf.t=pcft,r.guess=0.01)
round(ytm,6)

###Notes
# last.coupon.date = DATE(2021,11,15)
# next.coupon.date = DATE(2022,5,15)
# settle = DATE(2021,12,30)
# 
# E <- as.numeric(next.coupon.date - last.coupon.date)    # the number of days in the current coupon period
# E
# DSC <- as.numeric(next.coupon.date - settle)             # the number of days from settlement to next coupon payment
# DSC
# frac<- DSC/365
# frac


# end   question 1
#----------------------------------------------------

# start question 2
DATE <- function(yyyy,mm,dd) {
  dte  <- as.Date(sprintf("%i-%i-%i",yyyy,mm,dd),format="%Y-%m-%d")
  return(dte)
}
as.Date2 <- function(x) {
  tryfmt <- c("%Y-%m-%d","%m/%d/%Y","%Y/%m/%d","%b %d,%Y")
  return(as.Date(x,tryFormats=tryfmt))
}
# fread csv
bond = fread("TeamAssignment1_Q2_bonds.csv")
strip = fread("TeamAssignment1_Q2_strips.csv")

strip
settle = DATE(2021,12,30)
freq = 2
#Add column to bond
bond[, maturity := as.Date2(maturity)]
bond[, settle := rep(settle,nrow(bond))]
bond[, freq := rep(2,nrow(bond))]
bond[, accint := bond.TCF(settle,maturity,coupon,freq,conv)$accrued, by = maturity]
bond[, fullbid := accint + pbid]
bond[, fullask := accint + pask]
View(bond)
#Split the strip to ci and sp
strip[, maturity := as.Date2(maturity)]
ci = strip[type == "ci"]
sp = strip[type == "sp"]
#For loop to calculate arbitrage bid and ask price
dt_Q2 = data.frame()
for (i in 1:nrow(bond)) {
  maturity = bond[i,maturity]
  coupon = bond[i,coupon]
  b.pbid = bond[i,pbid]
  b.pask = bond[i,pask]
  df = data.table(date = coupons.dates(settle,maturity,freq))
  nper = nrow(df)
  df[, coupons := rep(coupon*100/2,nper)]
  df[, principal := c(rep(0,(nper-1)),100) ]
  df.ci <- merge(df,ci,by.x="date",by.y="maturity",all.x=TRUE)
  df.sp <- merge(df,sp,by.x="date",by.y="maturity",all.x=TRUE)
  nlast <- nrow(df.ci)     # this finds the number of rows in df.ci
  arb.bid <- sum(df.ci$coupons*df.ci$pbid/100)+
    df.sp$principal[nlast]*df.sp$pbid[nlast]/100
  nlast <- nrow(df.sp)     # this finds the number of rows in df.sp
  arb.ask <- sum(df.ci$coupons*df.ci$pask/100)+
    df.sp$principal[nlast]*df.sp$pask[nlast]/100
  df_arb = cbind(bond[i],arb.bid,arb.ask)
  dt_Q2 = rbind(dt_Q2,df_arb)
}
# add true and false column to the table for arbitrage opp
dt_Q2 = data.table(dt_Q2)
View(dt_Q2)
dt_Q2[, Buy_STRIPS_Sell_Security := arb.ask < fullbid]
dt_Q2[, Buy_Security_Sell_STRIPS := fullask < arb.bid]
View(dt_Q2)

# end   question 2
#----------------------------------------------------

# start question 3
#3a Which payout option, A or B, has the highest value?

payout <- fread("TeamAssignment1_Q3.csv")
payout[, maturity := as.Date2(Date)]
payout
ci <- strip[strip$type == "ci",]
ci
# Calculate the mid price:
ci[, pmid := 0.5*(pbid+pask)]
# Convert maturity date of STRIPS to a date object
ci[, maturity := as.Date2(maturity)]
# Find the time to maturity (ttm) of each STRIPS:
settle <- DATE(2021,12,30)
ci[, ttm := as.numeric(maturity-settle)/365]
# Find the spot rates (with semi-annual compounding):
comp.freq <- 2
ci[, spot := zcb.yield(pmid,ttm,comp.freq)]
# Find the time to the payment on 06/30/2022 in years:
date.new <- DATE(2022,06,30)
ttm.new <- as.numeric(date.new-settle)/365
ttm.new
# Use the spline function to interpolate the spot rate for 6/30/2022:
spot.new <- spline(x=ci$ttm,y=ci$spot,xout=ttm.new,method="natural")$y
View(ci)
# Calculate the discount factor for 6/30/2022:
disfac.new <- zcb.price(spot.new,ttm.new,comp.freq)/100
disfac.new

# Steps to find the present value of the 30 payments:
payout[, ttm := as.numeric(maturity-settle)/365]
payout[, spot:= spline(x=ci$ttm,y=ci$spot,xout=payout$ttm,method="natural")$y]
payout[, disfac := zcb.price(spot,ttm)/100 ]
payout[, pv := disfac * Amount]
View(payout)
OptionA <- round(sum(payout$pv),0)
OptionA

## 113950816, therefore, select option A

## 3b) Suppose Option A is the only payout option. Discuss how the lottery winner can convert the thirty payments into a single payment using coupon STRIPS for settlement on 12/30/2021.

###use bid price
ci[, spot_3b := zcb.yield(pbid,ttm,comp.freq)]
ci
payout
payout[, spot_3b:= spline(x=ci$ttm,y=ci$spot_3b,xout=payout$ttm,method="natural")$y]
payout[, disfac_3b := zcb.price(spot_3b,ttm)/100 ]
payout[, pv_3b := disfac_3b * Amount]

pv_A_3b = round(sum(payout$pv_3b),0)
pv_A_3b #113520913


## 3c) ) Suppose Option A is the only payout option. Discuss how the lottery’s payment department can use coupon STRIPS to fund the thirty payments.
###use ask price
ci[, spot_3c := zcb.yield(pask,ttm,comp.freq)]

payout[, spot_3c := spline(x=ci$ttm,y=ci$spot_3c,xout=payout$ttm,method="natural")$y]
payout[, disfac_3c := zcb.price(spot_3c,ttm)/100 ]
payout[, pv_3c := disfac_3c * Amount]

pv_A_3c = round(sum(payout$pv_3c),0)
pv_A_3c#114380714
## 3c) Suppose Option A is the only payout option. Discuss how the lottery’s payment department can use coupon STRIPS to fund the thirty payments.

# end   question 3
#----------------------------------------------------





