
library(data.table)
library(ggplot2)
library(jrvFinance)
library(optimx)
library(quantmod)

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
#----------------------------------------------------

# start question 1
libor <- fread("TeamAssignment3_libor.csv")  
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
edf <- fread("TeamAssignment3_edfut.csv")
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


notional   <-100000000
swap$cf.float <- 0
swap$cf.fixed <- 0
for (i in 2:nrow(swap)) {
  swap$cf.float[i] <- notional * swap$L3[i-1] * swap$ndays[i]/360
}

# discount factors
swap$disfac <- 1
for (i in 2:nrow(swap)) {
  swap$disfac[i] <- swap$disfac[i-1] / (1+swap$L3[i-1]*swap$ndays[i]/360)
}


pv_float <- sum(swap$disfac * swap$cf.float)+notional
round(pv_float,3)

# fixed payments 
fixed.rate <- 0.0431      # given in the question 1
swap[2:nrow(swap), cf.fixed := notional * fixed.rate * ndays/360]

#pv of fixed cash flow plus notional payment 
pv_fixed <- sum(swap$disfac*swap$cf.fixed)+notional
round(pv_fixed,3)

initial.swap.rate <- (pv_float/pv_fixed) * fixed.rate
#the swap rate is calculated using PV + notional at the end 
round(initial.swap.rate,4)


#Calculate ttm 
end.date <- DATE(2025,02,01)
swap[, ttm.float := ndays/360]
swap[, ttm.fixed := as.numeric(pay.date-settle)/360]

#Calculate cash flow
#the fixed rate payments plus the notional principal is a coupon bond
#the floating rate payments plus the notional principal is a floating rate note
swap[pay.date == "2025-02-01", cf.float := cf.float + notional]
swap[pay.date == "2025-02-01", cf.fixed := cf.fixed + notional]

#Calculate weight 
swap[, wt.float := cf.float * disfac/pv_float]
swap[, wt.fixed := cf.fixed * disfac/pv_fixed]

swap[, wtmat.float := wt.float * ttm.float]
swap[, wtmat.fixed := wt.fixed * ttm.fixed]

# Use L3 rate as the spot rate to calculate Mod Duration for floating rate payer
#
swap[, Dmac.float := swap$wtmat.float]
#Divided by 4, because it is quarterly 
swap[, Dod.float := Dmac.float/(1+ L3/4)]
swap
Dod.float <- sum(swap$Dod.float)
Dod.float

# Use fixed rate as the spot rate to calculate Mod Duration for Fixed rate receiver
#0.0431
Dmac.fixed <- sum(swap$wtmat.fixed)
Dod.fixed <- Dmac.fixed/(1+ fixed.rate/4)

#Duration of a swap position 
#Duration of a receive-fixed swap = duration of the underlying coupon bond - duration of the underlying floating-rate bond > 0
swap.Dmod <- Dod.fixed - Dod.float
round(swap.Dmod,4)
#[1] 1.5519
# end   question 1
#----------------------------------------------------

# start question 2

# read libor spot rates
libor <- fread("TeamAssignment3_libor.csv")
#settle 2023,02,01
settle.yyyy <- 2023
settle.mm <- 02
settle.dd <- 01
settle <- DATE(settle.yyyy,settle.mm,settle.dd)
names(libor) <- c("term","spot")
libor[, start.date := settle ]
seq.month <- seq(from=settle,to=DATE(settle.yyyy+1,settle.mm,settle.dd),by="month")
libor[, end.date := c(settle+1,seq.month[c(2,4,7,13)])]
libor[, ndays := as.numeric(end.date-start.date)]
libor

#Step 1: set up payment dates
# first floating rate is the current 3m LIBOR spot
settle.libor <- DATE(2022,12,14)

swap <- data.table(period = c(0:8),
                   pay.date = seq(settle.libor,
                                  DATE(settle.yyyy+2, settle.mm,settle.dd), by = "3 month"))
swap[, ndays := c(0,as.numeric(diff(swap$pay.date))) ]
swap[, L3 := NA]
swap$L3[1] <- 0.04376 #Given by the question2
swap

# step 2: Find the 3-month LIBOR forward rates for payment dates
# read eurodollar futures
edf <- fread("TeamAssignment3_edfut.csv")
settle.edf <- DATE(2023,2,1)
names(edf) <- c("FutMatDt","FutPr")
nedf <- nrow(edf)
edf[, FutMatDt := as.Date2(FutMatDt)]
edf[, start.date := as.Date2(FutMatDt)+2 ]          # T+2 settlement
edf[, forw    := 1-FutPr/100 ]

# include today's 3 month libor (spot) rate at 12,14,2022
#### I used 0.04376 to interpolate starting the 1st payment date  
tmp <- data.table(start.date=DATE(2022,12,14),
                  forw=0.04376)
tmp <- rbind(tmp,edf[,.(start.date,forw)])
setorderv(tmp,c("start.date"))
tmp

# interpolate 3 month forward rates
# used tmp table to interpolate L3
L3 <- as.data.frame(spline(x=tmp$start.date,y=tmp$forw,xout=swap$pay.date,method="natural"))$y
swap$L3[2:nrow(swap)] <- L3[2:length(L3)]
swap

# Step 3.5: Interpolate the stub rate* from 2023-02-01 to the first payment date(using libor to interpolate)
first.pay.date <- DATE(2023,03,14)
start.date <- DATE(2023,02,01)

newdata <- data.table(start.date=start.date,
                      first.pay.date=first.pay.date)
newdata[, ndays := as.numeric(first.pay.date-start.date)]
newdata[, spot := spline(x=libor$ndays,y=libor$spot,xout=newdata$ndays,method='natural')$y]
stub.rate <- round(newdata$spot,10)
stub.rate
#0.04661448

#Step 3: Forecast floating cash flows
notional   <-20000000
swap$cf.float <- 0
swap$cf.fixed <- 0
for (i in 2:nrow(swap)) {
  swap$cf.float[i] <- notional * swap$L3[i-1] * swap$ndays[i]/360
}

###Calculate on going swap, insert 2023-02-01 to the swap data table
On.Going.swap <- data.table(pay.date=DATE(2023,02,01), L3 = stub.rate, cf.float = 0, cf.fixed=0)
On.Going.swap <- rbind(On.Going.swap, swap[2:nrow(swap), .(pay.date, L3, cf.float, cf.fixed)])
On.Going.swap[, ndays := c(0,as.numeric(diff(On.Going.swap$pay.date)))]
On.Going.swap

# Step 4: Calculate discount factors for payment dates
On.Going.swap$disfac <- 1
for (i in 2:nrow(On.Going.swap)) {
  On.Going.swap$disfac[i] <- On.Going.swap$disfac[i-1] / (1+On.Going.swap$L3[i-1]*On.Going.swap$ndays[i]/360)
}
On.Going.swap

#Step 5: Find PV of floating cash flows 

pv_float <- sum(On.Going.swap$disfac * On.Going.swap$cf.float)
round(pv_float,3)
#[1] 1700907


# fixed payments 
swap.rate.01 <- 0.049      # given in the question 2

On.Going.swap[2:nrow(On.Going.swap), cf.fixed := notional * swap.rate.01 * ndays/360]
On.Going.swap

# Step 6: Find PV of fixed cash flows

pv_fixed <- sum(On.Going.swap$disfac*On.Going.swap$cf.fixed)
round(pv_fixed,3)
#[1] 1766816

swap_value = pv_fixed - pv_float
round(swap_value,4)


#  65909.08
# end   question 2
#----------------------------------------------------

# start question 3
m = 2 # number of compounding periods in a year
delt = 0.5 # length of a time step (in years)
N = 10 # number of time steps

#Assume all up probabilities are 0.5 and create the qtree matrix:
  qtree = matrix(0, nrow=N+1, ncol=N+1)
for (i in 1:(N+1)) {
  for (j in 1:i) {
    qtree[i,j] = 0.5
  }
}
#b. Create an interest rate tree for the 1 period spot rate (ztree):
  ztree = matrix(0, nrow=N+1, ncol=N+1)
ztree[1,1] = 0.1
for (i in 2:(N+1)) {
  ztree[i,1] = ztree[i-1,1]+0.01
  for (j in 2:i) {
    ztree[i,j] = ztree[i,j-1]-0.02
  }
}

#c. Modify the R code to find the price tree for the (non-callable) bond, 
# paying 10% coupon semi-annually, with 5-years to maturity
N=10
citree = matrix(0,nrow=N+1, ncol=N+1)
C = 5
for (i in 2:(N+1)) {
  citree[i,c(1:i)] <- rep(C,i)
}
citree
prt.tree(citree)

deltm <- delt * m
ptree = matrix(0,nrow=N+1, ncol=N+1)   # price tree, including coupon
ptree[N+1,c(1:(N+1))] = rep(100,(N+1))
for (i in N:1) {
  i1 = i+1
  ptree[i,1:i] = (qtree[i,1:i]*(ptree[i+1,1:i]+citree[i+1,1:i])+
                    (1-qtree[i,1:i])*(ptree[i+1,2:i1]+citree[i+1,2:i1]))/(1+ztree[i,1:i]/m)^deltm
}
prt.tree(ptree)

#d.Modify the R code to find the price tree for the European call option on this bond, with the 
#strike price of 100 and maturity of 3 years.
K <- 100                                # strike price
Nopt <- 6                              # option maturity is 6 periods
ECall = matrix(0,nrow=Nopt+1, ncol=Nopt+1)   # option price tree
for (j in (1:(Nopt+1))) {
  ECall[Nopt+1,j] <- max( 0,ptree[Nopt+1,j]-K )
}
for (i in Nopt:1) {
  i1 = i+1
  ECall[i,1:i] = (qtree[i,1:i]*ECall[i+1,1:i]+(1-qtree[i,1:i])*ECall[i+1,2:i1])/(1+ztree[i,1:i]/m)^deltm
}
prt.tree(ECall,4)

#e.Write the R code to find the price tree for the callable bond, which is the non-callable bond 
# minus the European call option. 
callable.bond <- ptree[1:7,1:7] - ECall
prt.tree(callable.bond,4)


# end   question 3
#----------------------------------------------------

# start question 4

# end   question 4
#----------------------------------------------------

# start question 5

# end   question 5
#----------------------------------------------------



