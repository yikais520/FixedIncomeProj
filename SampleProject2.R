
library(data.table)
library(ggplot2)
library(jrvFinance)
library(optimx)

#Date and zero coupon bond function 
DATE <- function(yyyy,mm,dd) {
  dte  <- as.Date(sprintf("%i-%i-%i",yyyy,mm,dd),format="%Y-%m-%d")
  return(dte)
}
as.Date2 <- function(x) {
  tryfmt <- c("%Y-%m-%d","%m/%d/%Y","%Y/%m/%d","%b %d,%Y")
  return(as.Date(x,tryFormats=tryfmt))
}
zcb.price <- function(zcb.yield,ttm,comp.freq=2) {
  return( 100/(1+zcb.yield/comp.freq)^(comp.freq*ttm) )
  
}
zcb.yield <- function(zcb.price,ttm,comp.freq=2) {
  return( comp.freq * ( (100/zcb.price)^(1/(comp.freq*ttm)) - 1 ) )
  
}

# create bond.convexity function
bond.convexity <- function(settle,mature,coupon,freq=2,yield,convention,comp.freq=freq) {
  library(jrvFinance)
  z  <- as.data.frame(bond.TCF(settle,mature,coupon,freq,convention))
  cf <- z$cf
  t  <- z$t
  r  <- yield
  m  <- comp.freq
  return ( 1/sum( cf / (1+r/m)^(t*m) ) * sum( t * (t+1/m) * cf / (1 + r/m)^(t*m+2) ) )
}

#----------------------------------------------------

# start question 1
#1a. Find the modified duration and convexity of each bond.

ts <- fread("TeamAssignment2_Q1.csv")

ts
settle <- DATE(2021,12,30)
comp.freq <- 2
freq <- 2
conv <- "ACT/ACT"

ts[, maturity := as.Date2(maturity)]
ts[, yield:= bond.yield(settle, maturity,coupon,freq = 2, clean.price, conv, comp.freq), by = maturity]
ts[, dmod := bond.duration(settle,maturity,coupon,freq,yield,conv,modified=TRUE,comp.freq),by = maturity]
ts[, Conv := bond.convexity(settle,maturity,coupon,comp.freq,yield,conv),by = maturity]
View(ts)
 

#1b. Find the modified duration and convexity of the bond portfolio.
ts[, accint := bond.TCF(settle,maturity,coupon,freq,conv)$accrued, by = maturity]
ts[, pfull := ts$clean.price + accint]
ts[, marketvau := face.value * pfull/100]
ts[, weight := marketvau/(sum(ts$marketvau))]
ts[, wtdmod := weight*dmod]
ts[, wtCov := weight * Conv]
Portdmod <- sum(ts$wtdmod)
PortCov <- sum(ts$wtCov)
#weighted average of modified duration and convexity 
Portdmod
PortCov

#1c. Suppose all yields increase by 10 bp, what is the first order approximation of the percentage 
#change of the market value of this portfolio.
#10 bp = 0.001, use first order approximation 
R <- 0.001
dp.p.1st <- -Portdmod * R *100
dp.p.1st

#1d. Suppose all yields increase by 10 bp, what is the second order approximation of the 
#percentage change of the market value of this portfolio

dR <- 0.001
dP.P.2nd <- -Portdmod * dR + 0.5 * PortCov * dR^2
round(dP.P.2nd*100,4)

#1e.  Suppose all yields increase by 10 bp, what is the actual percentage change of the market 
#value of this portfolio.
# assuming the value is market value 
price.old <- sum(ts$marketvau)
price.old
dR = 0.001
ts[, yield.new := yield + dR]
ts[, price.new := bond.price(settle,maturity,coupon,freq,yield.new,conv,comp.freq) +
  accint,by = maturity]
View(ts)
ts[, marketvau1 := price.new*face.value/100]
price.new<- sum(ts$marketvau1)
price.new
dP.P.actual <- (price.new-price.old)/price.old
round(dP.P.actual*100,4)

View(ts)

#1f.  Compare this approximation to the actual percentage change of the 
#market value of this portfolio.

q1.f = copy(ts)
q1.f
yield.change = c(0.0015,0.0013,0.0011,0.0009)
q1.f[,ad.yield:=yield+yield.change]
q1.f[,ad.clean.price:=bond.prices(settle,maturity,coupon,freq,ad.yield,conv)]
new.full.price = q1.f$ad.clean.price+q1.f$accint
new.mktvalue =sum(new.full.price*q1.f$face.value/100)
percentage.f = (new.mktvalue - sum(q1.f$marketvau))/sum(q1.f$marketvau)
percentage.f

q1.f[,new.full.price:=ad.clean.price+accint]
q1.f[,new.mkt.value:=new.full.price*marketvau/100]
q1.f[,new.wt:=new.mkt.value/sum(new.mkt.value)]
q1.f.2 = q1.f[,.(maturity,coupon,face.value,ad.yield,ad.clean.price,new.full.price,new.mkt.value,new.wt)]
q1.f.2[,mod.duration:=bond.durations(settle,maturity,coupon,freq,ad.yield,conv,comp.freq,modified=TRUE)]
convexity.f=c()
for (i in 1:nrow(ts)) {
  temp = bond.convexity(settle,q1.f.2$maturity[i],q1.f.2$coupon[i],freq,q1.f.2$ad.yield[i],conv)
  convexity.f[i] = temp
}

q1.f.2[,convexity:=convexity.f]
q1.f.2

new.wt.mod.duration = sum(q1.f.2$new.wt*q1.f.2$mod.duration)
new.wt.convexity = sum(q1.f.2$new.wt*q1.f.2$convexity)
first.order.approx = sum(yield.change*q1.f.2$new.wt) * (-new.wt.mod.duration)
first.order.approx
second.order.approx = sum(yield.change*q1.f.2$new.wt) * (-new.wt.mod.duration) + new.wt.convexity * 
  (sum(yield.change*q1.f.2$new.wt)^2) /2
second.order.approx
percentage.f

# end   question 1
#----------------------------------------------------

# start question 2
library(optimx)
methods.fast <- c('BFGS','Nelder-Mead','L-BFGS-B','nlm','nlminb','Rvmmin')

data <- fread("TeamAssignment2_Q2_bond.csv")
settle <- DATE(2021,12,30)
freq <-2
conv <- "ACT/ACT"
data[, maturity := as.Date2(maturity)]
data[, accint := bond.TCF(settle,maturity,coupon,freq,conv)$accrued, by = maturity]
data[, pfull := pclean + accint]
data[, ttm := as.numeric(maturity-settle)/365]
View(data)

#2a.What are the estimated parameters? [
parm <- c(0.01,0,0,0)
nparm<- 4

# define g function from assignment page 2:  
gfun4 <- function(ttm,parm) {
  tmp <- parm[1] + parm[2]*ttm + parm[3]^2*log(1+ttm) + parm[4]*((1/(1+ttm))-1)
  return(tmp)
}

#starting value (0.01, 0, 0, 0)
start <- c(0.01,0,0,0)


system.time({
  
  cf.mat <- matrix(0,nrow=nrow(data),ncol=61)
  
  # ttm.mat is the matrix of ttms; each row is a bond from cf.mat; the ttms are in the same columns as cf.mat
  ttm.mat<- cf.mat
  
  # ncf.mat is a matrix that keeps track of how many cash flows there are in each bond.
  # so when we want retrieve the cf and ttm of a bond, we know how many to get.
  ncf.mat<- matrix(0,nrow=nrow(data),ncol=1)
  
  for (i in 1:nrow(data)) {
    cfi  <- bond.TCF(settle,data$maturity[i],data$coupon[i],2,"ACT/ACT")$cf    # for a coupon security, use bond.TCF to retrieve future cash flows
    cf.d <- coupons.dates(settle,data$maturity[i],2)                           #                        use coupons.dates to retrive dates of these cash flows
    ttmi <- as.numeric(cf.d-settle)/365                                        #                        use bond.TCF to retrieve ttms of these cash flows
    ncf.mat[i] <- length(cfi)                                                  # store the number of cash flows is the length of the cfi vector in ncf.mat
    cf.mat[i,c(1:length(cfi))] <- cfi                                          # store the cash flows in the i-th row of cf.mat
    ttm.mat[i,c(1:length(ttmi))] <- ttmi                                       # store the ttms in the i-th row of ttm.mat
  }
  
  ssr <- function(parm) {
    for (i in 1:nrow(data)) {
      ncf  <- ncf.mat[i]                       # retrieve the number of cash flows of this bond
      cfi  <- cf.mat[i,c(1:ncf)]               # retrieve the cash flows of this bond
      ttmi <- ttm.mat[i,c(1:ncf)]              # retrieve the ttms of these cash flows
      rates<- sapply(ttmi,gfun4,parm=parm)     # calculate the spot rates for these ttms using gfun()
      disfac<- 1/(1+rates/2)^(ttmi*2)          # calculate the discount factors for these ttms
      data$phat[i] <- sum(disfac * cfi)        # calculate the PVs for cash flows, and sum them to get the model price of this bond
    }
    ztest <- sum((data$pfull-data$phat)^2)
    if (is.nan(ztest)) ztest <- 9e20
    return(ztest)
  }
  
  opt4 <- optimx(start,ssr,method=methods.fast,
                 control = list(maxit=10000))
}
)

opt4

# find model with lowest mse 
opt4[which.min(opt4$value),]

parm4 <- unlist(opt4[which.min(opt4$value),][1:nparm])
parm4

#2b.Compare the actual prices and predicted prices of the bonds based on estimated parameters 

for (i in 1:nrow(data)) {
  # CALCULATE data$phat[i]
  cfi  <- bond.TCF(settle,data$maturity[i],data$coupon[i],2,"ACT/ACT")$cf
  cf.d <- coupons.dates(settle,data$maturity[i],2)
  ttmi <- as.numeric(cf.d-settle)/365   
  # Parm from previous question are used to determine spot rate 
  rates<- sapply(ttmi,gfun4,parm=parm4)
  disfac<- 1/(1+rates/2)^(ttmi*2)
  data$phat[i] <- sum(disfac * cfi)
}

# plot fitted and actual bond prices 
ggplot(data,aes(x=ttm,y=pfull,color="actual")) +
  geom_point(alpha=0.2) +
  geom_line(aes(y=phat,color="fit4"),linetype="longdash",lwd=0.6) +
  scale_colour_manual("",
                      breaks=c("actual","fit4"),
                      values=c("green","blue")) +
  xlab("Time to maturity (years)") +
  ylab("Price")

#2c. Compare the actual prices and predicted prices of the coupon strips using the nonlinear yield 

data1 <- fread("TeamAssignment2_Q2_ci.csv")
settle <- DATE(2021,12,30)
freq <-2
conv <- "ACT/ACT"
data1[, maturity := as.Date2(maturity)]
data1[, ttm := as.numeric(maturity-settle)/365]
data1[, face_value := rep(100, nrow(data1))]
data1[, rates:= sapply(ttm,gfun4,parm=parm4)]
data1[, disfac:= 1/(1+rates/2)^(ttm*2)]
data1[, predicted := disfac * face_value]

View(data1)

# plot fitted and predicted bond prices 
ggplot(data1,aes(x=ttm,y=price,color="actual")) +
  geom_point(alpha=0.2) +
  geom_line(aes(y=predicted,color="predicted"),linetype="longdash",lwd=0.6) +
  scale_colour_manual("",
                      breaks=c("actual","predicted"),
                      values=c("red","black")) +
  xlab("Time to maturity (years)") +
  ylab("Price")

# end   question 2
#----------------------------------------------------

# start question 3
#3a. Find the discount factors corresponding to the above ten dates. 
#
q3 = fread("TeamAssignment2_Q3.csv")
str(q3)
q3[,maturity:=as.Date2(maturity)]
q3[,fprice:=100]
q3[,ttm:=as.numeric(maturity-settle)/365,by = "maturity"]
q3[,disfac:=1]
q3$disfac[1] = q3$fprice[1]/(100+q3$par.rate[1]*100)

#df1 calc:[100 + 100 * C(1)] * D(1) = 100
#df2 calc: 100 * C(2) * D(1) + [100 + 100 * C(2)] * D(2) = 100
#-----recursive stripping-----
for (i in 2:nrow(q3)) {
  cumdf = sum(q3$disfac[1:i-1])
  q3$disfac[i] = (q3$fprice[i]-q3$par.rate[i]*100*cumdf) / (100+q3$par.rate[i]*100)
}

q3[,spot:=zcb.yield(disfac*100,ttm)]
q3

#3b. Use this information to find the present value of an annuity that pays $100,000 on Jun 30 of 
# each year, starting 6/30/2022 and ending 6/30/2031.

payout <- data.table(date = c("2022-06-30","2023-06-30","2024-06-30","2025-06-30","2026-06-30","2027-06-30","2028-06-30","2029-06-30","2030-06-30","2031-06-30" ),
                     amount = rep(100000,10))
payout[, date := as.Date2(date)]
payout[,ttm:=as.numeric(date-settle)/365]
payout[, spot_3b := spline(x=q3$ttm,y=q3$spot,xout=payout$ttm,method="natural")$y]
payout[, disfac_3b := zcb.price(spot_3b,ttm)/100 ]
payout[, pv_3b := disfac_3b * amount]

View(payout)
sum(payout$pv_3b)
#933264.7
# end   question 3
