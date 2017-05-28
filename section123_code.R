

# monthly plot ---------------------------------
a=ts(crashes.mon.mi,freq=12,start=c(2004,1))
decomp = stl(a[,5], "periodic")
decomp1 = stl(a[,6], "periodic")
c1=decomp$time.series[,2]
c2=decomp1$time.series[,2]

decomp = stl(a[,7], "periodic")
decomp1 = stl(a[,8], "periodic")
c3=decomp$time.series[,2]
c4=decomp1$time.series[,2]

par(mfrow=c(2,2))
plot(a[,5],col=4,ylab='',xlab='',main='total crash rate');lines(c1,lty=3)
plot(a[,6],col=2,ylab='',xlab='',main='fatal crash rate');lines(c2,lty=3)
plot(a[,7],col=1,ylab='',xlab='',main='average temperature');lines(c3,lty=3)
plot(a[,8],col=1,ylab='',xlab='F',main='number of events');lines(c4,lty=3)

# Multivariate model: VARX -----------------------------------

library(dse)
da_2016=data.frame(crash=rep(0,12),fatal=rep(0,12),temp=c(27.32258065,30.5862069,41.67741935,44.83333333,60.12903226,70.46666667,38.27934186,40.34278085,38.74741445,35.99715328,32.73267552,28.7376),event=c(19,13,16,15,10,6,11,14,10,15,14,19))
da2=rbind(da,da_2016)
CNA2 <- TSdata(input= da2[,3:4],output= da2[,1:2])
CNA2 <- tframed(CNA2,list(start=c(2004,1), frequency=12))
seriesNamesInput(CNA2) <- c("temp","event")
seriesNamesOutput(CNA2) <- c("crash","fatal")
CNA.ls <- estVARXls(window(CNA2,end=c(2015,12)),max.lag=2)
S.p=forecast(CNA.ls, conditioning.inputs=CNA2$input)
S.p$forecast
tfplot(S.p,start=c(2014,1))

print(CNA.ls)
tfplot(CNA.ls4)
stability(CNA.ls4)
rr=checkResiduals(CNA.ls4,plot=T)
plot(rr$residuals,type='p')


o=c(sqrt(mean((pre[,1]-da[,1])^2)),sqrt(mean((pre[,2]-da[,2])^2)))
pre=CNA.ls4$estimates$pred
o=rbind(o,c(sqrt(mean((pre[,1]-da[,1])^2)),sqrt(mean((pre[,2]-da[,2])^2))));o

CNA.ls2 <- estVARXar(window(CNA2,end=c(2015,12)),max.lag=2)
CNA.ls3 <- estSSfromVARX(window(CNA2,end=c(2015,12)),max.lag=2)
CNA.ls4 <- estMaxLik(CNA.ls,max.lag=1)

z <- featherForecasts(TSmodel(CNA.ls4), CNA2)
tfplot(z)

z2 <- horizonForecasts(TSmodel(CNA.ls4), CNA2,horizons = c(1,3,6))
tfplot(z2)

# spectum -----------------------------------
ap1=spec.pgram(ts(scale(crashes.mon.mi$crash_rate_100k),start=c(2004,1),freq=12),log='no',span=c(3,5))
ap2=spec.pgram(ts(scale(crashes.mon.mi$temp),start=c(2004,1),freq=12),log='no',span=c(3,5))
ap3=spec.pgram(ts(scale(crashes.mon.mi$fatal_rate_100k),start=c(2004,1),freq=12),log='no',span=c(3,5))
ap4=spec.pgram(ts(scale(crashes.mon.mi$event),start=c(2004,1),freq=12),log='no',span=c(3,5))
par(mfrow=c(1,2))
spec.pgram(ts(scale(crashes.mon.mi$crash_rate_100k),start=c(2004,1),freq=12),log='no',span=c(3,5),main="Smoothed periodogram")
lines(ap2$freq,ap2$spec,lty=2,col=2)
lines(ap3$freq,ap3$spec,lty=2,col=3)
lines(ap4$freq,ap4$spec,lty=2,col=4)
legend("right",c('crash','fatal','temperature','event'),lty=c(1,2,2,2),col=c(1,3,2,4),cex = 0.75)
acf(crashes.mon.mi$crash_rate_100k,lag.max = 60,main='ACF plot for total crash rate')

aic_table <- function(data,P,Q){
  table <- matrix(NA,(P+1),(Q+1))
  for(p in 0:P) {
    for(q in 0:Q) {
      table[p+1,q+1] <- arima(data,order=c(p,0,q))$aic
    }
  }
  dimnames(table) <- list(paste("AR",0:P,sep=""),paste("MA",0:Q,sep=""))
  table
}
d_aic_table <- aic_table(crashes.mon.mi$crash_rate_100k,4,5)

sarma=arima(crashes.mon.mi$crash_rate_100k, order=c(3,0,2),
            seasonal=list(order=c(1,0,0),period=12))
sarma
sqrt(mean(sarma$residuals^2))

d_aic_table <- aic_table(crashes.mon.mi$fatal_rate_100k,4,5)

sarma2=arima(crashes.mon.mi$fatal_rate_100k, order=c(2,0,2),
            seasonal=list(order=c(1,0,0),period=12)
)
sarma2
sqrt(mean(sarma2$residuals^2))

predict=predict(sarma,n.ahead=12)
plot(crashes.mon.mi$crash_rate_100k,type='l',col=4,xlim=c(121,157),ylab="total crash rate",xlab='')
lines(predict$pred,type="l",col=1)
lines(predict$pred-1.98*predict$se,col=3,pch=22, lty=2)
lines(predict$pred+1.98*predict$se,col=3,pch=22, lty=2)

predict2=predict(sarma2,n.ahead=12)
plot(crashes.mon.mi$fatal_rate_100k,type='l',col=2,ylim=c(-0.02,0.08),xlim=c(121,157),ylab="fatal crash rate",xlab='')
lines(predict2$pred,type="l",col=1)
lines(predict2$pred-1.98*predict2$se,col=3,pch=22, lty=2)
lines(predict2$pred+1.98*predict2$se,col=3,pch=22, lty=2)

# residual diagnostics ------------------------
par(mfrow=c(2,3))
qqnorm(e,xlab='',tlab='');qqline(e,col=4)
acf(e,main='ACF plot',xlab='')
plot(e,type='l',xlab='',ylab='',main='Residual plot')
library(portes)
plot(gvtest(e,1:60)[,4],main="Gneralized variance tests",
     ylab="p-value", xlab="lag",pch=16);abline(h=0.05,lty=2,col=4)
plot(LjungBox(e,1:60)[,4],main="Ljung-Box tests",
     ylab="p-value", xlab="lag",pch=16,ylim=c(0,1));abline(h=0.05,lty=2,col=4)
plot(Hosking(e,1:60)[,4],main="Hosking tests",
     ylab="p-value", xlab="lag",pch=16,ylim=c(0,1));abline(h=0.05,lty=2,col=4)


# non-linear model-----------------------------
library(tsDyn);library(sm)
par(mfrow=c(2,4))
autopairs(da[,1], lag=1, type="lines")
autopairs(da[,2], lag=1, type="lines")
autopairs(da[,3], lag=1, type="lines")
autopairs(da[,4], lag=1, type="lines")
par(mfrow=c(1,4))
lag.plot(v1.new,labels=FALSE,lag=1)
lag.plot(v2.new,labels=FALSE,lag=1)
lag.plot(v3.new,labels=FALSE,lag=1)
lag.plot(v4.new,labels=FALSE,lag=1)
v1.new=llar.predict(da[,1],n.ahead=40,m=3,eps=2,onvoid="enlarge",r=5)
v2.new=llar.predict(da[,2],n.ahead=40,m=3,eps=2,onvoid="enlarge",r=5)
v3.new=llar.predict(da[,3],n.ahead=40,m=3,eps=2,onvoid="enlarge",r=5)
v4.new=llar.predict(da[,4],n.ahead=40,m=3,eps=2,onvoid="enlarge",r=5)

#AR
d=ts(crashes.mon.mi$crash_rate_100k,freq=12,start=c(2004,1))
d=ts(crashes.mon.mi$fatal_rate_100k,freq=12,start=c(2004,1))
x.ar=linear(d, m=2);summary(x.ar)

##SETAR

selectSETAR(d,m=2)
x.setar=setar(d, m=2,mL=2)
summary(x.setar)
x.setar.p=predict(x.setar, n.ahead=12)

x.setar3=setar(d, m=2,mL=2,nthresh=2)
summary(x.setar3)
x.setar.p3=predict(x.setar3, n.ahead=12)
Hansen.x=setarTest(d, m=1,nboot=1000)
summary(Hansen.x)
plot(Hansen.x)#is linear

#logistic
selectLSTAR(d, m=1)
x.lstar=lstar(d, m=2, mL=2,mH=1,thDelay = 1 )
summary(x.lstar)
x.lstar.p=predict(x.lstar, n.ahead=12)

#NN
set.seed(8);selectNNET(d, m=3, size=1:10)
x.nnet=nnetTs(d, m=2,size=9);summary(x.nnet)
x.nnet.p=predict(x.nnet, n.ahead=10)

#AAM
d=ts(crashes.mon.mi$crash_rate_100k,freq=12,start=c(2004,1))
d=ts(crashes.mon.mi$fatal_rate_100k,freq=12,start=c(2004,1))
x.aar=aar(d,m=1);summary(x.aar)
x.aar.p=predict(x.aar,n.ahead=12)

par(mfrow=c(1,3))
autopairs(da[,1], lag=1, type="lines")
lag.plot(v1.new,labels=FALSE,lag=1)
plot(d[121:144],type='l',col=2,xlim=c(0,36),ylab="total crash rate",xlab='')
lines(25:36,x.aar.p,type="l",col=1)
lines(25:36,x.aar.p-1.98*0.2221178,col=3,pch=22, lty=2)
lines(25:36,x.aar.p+1.98*0.2221178,col=3,pch=22, lty=2)

#Compare
mod <- list()
mod[['linear']] =x.ar
mod[['setar']] =x.setar
mod[['setar3']] =x.setar
mod[['lstar']] =x.lstar
mod[['nnetTs']]=x.nnet
mod[['aar']]=x.aar
u2=rbind(sapply(mod, AIC),sapply(mod, MAPE))
u=rbind(u,u2)
rownames(u)=c('AIC','MAPE','AIC','MAPE')


# Garch ----------------------------------
library(TSA);
(b=McLeod.Li.test(y=residuals(sarma)))
library(rugarch)
r.x=a[,5]
spec = ugarchspec(variance.model=list(model="eGARCH",
       garchOrder=c(1,1)), mean.model=list(armaOrder=c(3,2),
       arfima=F,include.mean=TRUE),distribution.model="sged")
(fit = ugarchfit(data = r.x, spec = spec))
sqrt(mean(fit@fit$residuals^2))
e=fit@fit$residuals
fo=ugarchforecast(fit,n.ahead=12,data=r.x)


# unit root and cointegration test--------------------------

library(tseries)
adf.test(dfy, alt='stationary')
adf.test(dfm, alt='stationary')

pp.test(dfy, alt='stationary')
pp.test(dfm, alt='stationary')

kpss.test(dfy,null='Level')
kpss.test(dfy,null='Trend')
kpss.test(dfm,null='Level')
kpss.test(dfm,null='Trend')

library(urca)
da=data.frame(crash=crashes.mon.mi$crash_rate_100k,fatal=crashes.mon.mi$fatal_rate_100k,temp=crashes.mon.mi$temp,event=crashes.mon.mi$event)

for(i in 1:4) print(kpss.test(da[,i]))
z=list();z1=list()
for(i in 1:4)z[[i]]=lm(da[,i]~.,da[,-i])
for(i in 1:4)print(summary(z[[i]]))
for(i in 1:4)
{z1[[i]]=ur.df(z[[i]]$res);
if (i==1) print(z1[[i]]@cval); print(z1[[i]]@teststat)}#stationary

zz=ca.po(da, demean = "constant",type ="Pu")
zz@cval;zz@teststat
zz=ca.po(da, demean = "constant",type ="Pz")
zz@cval;zz@teststat

at=ca.jo(da,type="trace",ecdet="const");at@cval;at@teststat
ae=ca.jo(da,type="eigen",ecdet="const");ae@cval;ae@teststat

library(MSBVAR)
granger.test(da,p = 12)
