library(forecast)
library(FinTS)
library(tseries)
library(TSA)
library(lmtest)
library(fGarch)
library(rugarch)

DATA_EMONEY = read.csv('C:/Users/user/Documents/DATA_EMONEY.csv', sep=';')
tsData = ts(DATA_EMONEY$Jumlah.Transaksi, start=c(2009,1),end=c(2020,2),
            frequency=12)
summary(tsData)

plot(tsData, main='Grafik Jumlah Transaksi E-Money di Indonesia ',ylab="Jumlah Transaksi")
abline(h=mean(tsData),col='blue')

tsDiff = diff(tsData, differences = 1)
plot(tsDiff, main = 'Plot Differensi 2 Jumlah Transaksi E-Money di Indonesia')
abline(h=mean(tsDiff), col='blue')
adf.test(tsDiff)

Acf(tsData, lag.max = 48,main="ACF Data Asli")
Pacf(tsData, lag.max = 48,main="PACF Data Asli")

Acf(tsDiff, lag.max = 48,main="ACF Diferensi 1")
Pacf(tsDiff, lag.max = 48,main="PACF Diferensi 1")

model = auto.arima(tsData, seasonal=F)
model = Arima(tsData, order = c(4,2,0))
model
coeftest(model)

summary(model)

plot(tsData,ylab="Jumlah Transaksi",main="Fitting Model ARIMA(4,2,0)")
lines(fitted(model), col = "blue")

#cek residual model manual
checkresiduals(model)

#cek efek heteroskedastis #tolak H0 saat pval < alpha=...%
ArchTest(residuals(model))

#definisikan barisan nt
nt <- residuals(model)
nt2 <- nt^2
plot(nt2,ylab="et^2",main="et^2 model ARIMA(4,2,0)")

Acf(nt2,main="ACF et^2")
Pacf(nt2,main="PACF et^2")


garch11<-(garch(nt2,order=c(1,1)))
checkresiduals(garch11)
plot(mt)

(garchFit(nt2=~garch(p,q))) #DAPET MODEL TERBAIK GARCH(1,1), ibaratnya auto.arima

#fitting garch
mod.res<-garch(nt,order=c(1,1))
mod.res
summary(mod.res)
plot(nt,main="Fitting model GARCH(1,1)")
lines.default(fitted(mod.res), col = "blue")
lines.default(-fitted(mod.res), col = "blue")

#LOOPING UNTUK VAR
Var = data.frame(k=c(1:length(nt2)),variansi=NA)
Var[1,2] <- 0
for ( k in 2:length(nt2)) {
  Var[k,2] <- 0.0107614 + (0.1531341)*nt2[k] +(0.8059737)*Var[k-1,2]
}
plot(Var[,2],type='l')

#==============================================================================
set.seed(293713846)
h = 12
Data <- DATA_EMONEY$Jumlah.Transaksi
g <- as.numeric(nt2)
n <- data.frame(k=c(1:h),error=NA)
atas <- data.frame(k=c(1:h),error=NA)
bawah <- data.frame(k=c(1:h),error=NA)
for (k in 1:h) {
  Var[134+k,2] <- 0.0107614 + (0.1531341+0.8059737)*Var[134+k-1,2]
  n[k,2] <- as.numeric(rnorm(1,mean=0,sd=1))
  Data[134+k] <- 2*Data[134+k-1] - Data[134+k-2] +
    (-1.0874)*(Data[134+k-1] -2*Data[134+k-2] + Data[134+k-3]) + 
    (-1.1272)*(Data[134+k-2] -2*Data[134+k-3] + Data[134+k-4]) + 
    (-0.8684)*(Data[134+k-3] -2*Data[134+k-4] + Data[134+k-5]) + 
    (-0.4669)*(Data[134+k-4] -2*Data[134+k-5] + Data[134+k-6]) + 
    as.numeric((sqrt(Var[134+k,2]))*n[k,2])
  atas[k,2]<-Data[134+k]+1.96*sqrt(Var[134+k,2])
  bawah[k,2]<-Data[134+k]-1.96*sqrt(Var[134+k,2])
}
newtsData<-ts(Data, start=c(2009,1),end=c(2021,2),
              frequency=12)
tsAtas<- ts(atas[,2], start=c(2020,3),end=c(2021,2),frequency=12)
tsBawah<- ts(bawah[,2], start=c(2020,3),end=c(2021,2),frequency=12)
tsA<- ts((atas[,2]), start=c(2020,3),end=c(2021,2),frequency=12)
tsB<- ts((bawah[,2]), start=c(2020,3),end=c(2021,2),frequency=12)
plot(newtsData,type='l',ylab="Jumlah Transaksi",col='blue',main = expression(paste("Forecast 12 bulan ke depan (", alpha, "=5%) ")))
for (k in 1:h) {
  rect((2020+(k+1)/12), tsBawah[k], (2020+(k+2)/12), tsAtas[k], density = NULL,
       col = "gray87",border="gray87")
}
lines(newtsData,type='l',col='blue')
lines(tsData,type='l',col='black')
#lines(tsA,type='l',col='gray87',lwd=6)
#lines(tsB,type='l',col='gray87',lwd=6)
lines(tsAtas,type='l',col='red')
lines(tsBawah,type='l',col='red')
for (k in 1:h) {
  rect((2020+(k+1)/12), tsBawah[k], (2020+(k+2)/12), tsAtas[k], density = NULL,
       col = "grey",border="grey")
}
rect(2020.1, tsBawah[2], 2020.9, tsAtas[2], density = NULL,
     col = "green",border="green")
#=================================================================================
set.seed(54321) 
h = 5
Data <- DATA_EMONEY$Jumlah.Transaksi
g <- as.numeric(nt2)
n <- data.frame(k=c(1:h),error=NA)
up <- data.frame(k=c(1:h),error=NA)
down <- data.frame(k=c(1:h),error=NA)
for (k in 1:h) {
  Var[129+k,2] <- 0.0107614 + (0.1531341+0.8059737)*Var[129+k-1,2]
  n[k,2] <- as.numeric(rnorm(1,mean=0,sd=1))
  Data[129+k] <- 2*Data[129+k-1] - Data[129+k-2] +
    (-1.0874)*(Data[129+k-1] -2*Data[129+k-2] + Data[129+k-3]) + 
    (-1.1272)*(Data[129+k-2] -2*Data[129+k-3] + Data[129+k-4]) + 
    (-0.8684)*(Data[129+k-3] -2*Data[129+k-4] + Data[129+k-5]) + 
    (-0.4669)*(Data[129+k-4] -2*Data[129+k-5] + Data[129+k-6]) + 
    as.numeric((sqrt(Var[129+k,2])*n[k,2]))
  up[k,2]<-Data[129+k]+1.96*sqrt(Var[129+k,2])
  down[k,2]<-Data[129+k]-1.96*sqrt(Var[129+k,2])
}
verData <-ts(tsData[129:134],start=c(2019,9),end=c(2020,2),frequency=12)
verCast <-ts(Data[129:(134)],start=c(2019,9),end=c(2020,2),frequency=12)
tsUp<-ts((up[,2]-9000000), start=c(2019,10),end=c(2020,3),frequency=12)
tsDown<-ts((down[,2]-9000000), start=c(2019,10),end=c(2020,3),frequency=12)

Date<-c("9/1/2019","10/1/2019","11/1/2019","12/1/2019","1/1/2020","2/1/2020")
testing<-c(1,2,3,1,2,3)
Time <- as.Date(Date, "%m/%d/%Y")
plot(testing~tanggal,type)

dates <- seq(as.Date("01/09/2019", format = "%d/%m/%Y"),
             by = "month", length = length(verData))

plot(tsData[129:134]~Time,ylab="Jumlah Transaksi",ylim=c(350000000,550000000),type='l'
     ,col='black',main = expression(paste("Verifikasi 5 Pengamatan Terakhir (", alpha, "=5%) ")))
par(new=T)
plot(verData,xaxt='n',ylab="Jumlah Transaksi",ylim=c(350000000,550000000),type='l'
     ,col='black',main = expression(paste("Verifikasi 5 Pengamatan Terakhir (", alpha, "=5%) ")))
for (k in 1:(h)) {
  rect((2019-(0.5/12)+(k+8)/12), tsDown[k], (2019-(0.5/12)+(k+9)/12), tsUp[k], density = NULL,
       col = "gray87",border="gray87")
}
rect((2019-(0.5/12)+(9)/12), tsDown[1], (2019-(1/12)+(10)/12), tsUp[1], density = NULL,
     col = "white",border="white")
lines(verData,type="l",col="black")
lines(verCast,type='l',col='blue')
lines(tsUp,type='l',col='red')
lines(tsDown,type='l',col='red')
axis.POSIXct(side = 1, at=dates, format = "%d/%m/%Y")

#========================SUDAH SELESAI===================================
#Dibawah hanya untuk oprek oprek

mt<-residuals(garchFit(nt2=~garch(p,q)))
plot(mt,type='l')


Var = data.frame(k=c(1:length(nt2)),variansi=NA)
Var[1,2] <- 0
for ( k in 2:length(nt2)) {
  Var[k,2] <- 3.674e+14 + (1.049e+00)*nt2[k] +(1.538e-02)*Var[k-1,2]
}
plot(Var[,2],type='l')

set.seed(1)
h = 5
Data <- DATA_EMONEY$Jumlah.Transaksi
g <- as.numeric(nt2)
n <- data.frame(k=c(1:h),error=NA)
for (k in 1:h) {
  Var[134+k,2] <- 3.674e+14 + (1.049e+00+1.538e-02)*Var[134+k-1,2]
  n[k,2] <- as.numeric(rnorm(1,mean=0,sd=1))
  Data[134+k] <- 2*Data[134+k-1] - Data[134+k-2] +
    (-1.0874)*(Data[134+k-1] -2*Data[134+k-2] + Data[134+k-3]) + 
    (-1.1272)*(Data[134+k-2] -2*Data[134+k-3] + Data[134+k-4]) + 
    (-0.8684)*(Data[134+k-3] -2*Data[134+k-4] + Data[134+k-5]) + 
    (-0.4669)*(Data[134+k-4] -2*Data[134+k-5] + Data[134+k-6]) + as.numeric((sqrt(Var[134+k,2])))#*n[k,2]))
}
plot(Data[1:(134+h)],type='l',col='blue')
lines(Data[1:134],type='l',col='black')

set.seed(NULL) 
h = 5
Data <- DATA_EMONEY$Jumlah.Transaksi
g <- as.numeric(nt2)
n <- data.frame(k=c(1:h),error=NA)
for (k in 1:h) {
  Var[129+k,2] <- 3.674e+14 + (1.049e+00+1.538e-02)*Var[129+k-1,2]
  n[k,2] <- as.numeric(rnorm(1,mean=0,sd=1))
  Data[129+k] <- 2*Data[129+k-1] - Data[129+k-2] +
    (-1.0874)*(Data[129+k-1] -2*Data[129+k-2] + Data[129+k-3]) + 
    (-1.1272)*(Data[129+k-2] -2*Data[129+k-3] + Data[129+k-4]) + 
    (-0.8684)*(Data[129+k-3] -2*Data[129+k-4] + Data[129+k-5]) + 
    (-0.4669)*(Data[129+k-4] -2*Data[129+k-5] + Data[129+k-6]) + 
    as.numeric((sqrt(Var[129+k,2])*n[k,2]))
}
plot(tsData[120:134],type='l',col='black')
lines(Data[120:(134)],type='l',col='blue')







m1=garchFit(formula=~arma(4,0)+garch(1,1),data=Datadiff)
summary(m1)

predict(m1,5)



for (k in -12:0) {
  Var[134+k,2] <- 0.0107614 + (0.1531341+0.8059737)*Var[134+k-1,2]
  n[k,2] <- as.numeric(rnorm(1,mean=0,sd=1))
  Data[134+k] <- 2*Data[134+k-1] - Data[134+k-2] +
    (-1.0874)*(Data[134+k-1] -2*Data[134+k-2] + Data[134+k-3]) + 
    (-1.1272)*(Data[134+k-2] -2*Data[134+k-3] + Data[134+k-4]) + 
    (-0.8684)*(Data[134+k-3] -2*Data[134+k-4] + Data[134+k-5]) + 
    (-0.4669)*(Data[134+k-4] -2*Data[134+k-5] + Data[134+k-6]) + as.numeric((sqrt(Var[134+k,2])))#*n[k,2]))
}


plot(Data[1:134],type='l',col='blue')
lines(as.numeric(tsData))

mod.res<-garch(nt,order=c(1,1))
mod.res
summary(mod.res)
shapiro.test(resid(mod.res))

plot(nt)
lines.default(fitted(mod.res), col = "blue")
lines.default(-fitted(mod.res), col = "blue")
lines(nt)
auto.arima(abs(),seasonal=F)
Pacf(abs(diffres))
Acf(abs(diffres))
Arima(abs(nt),order=c(3,1,0))

diffres = diff(nt, differences = 1)
adf.test(diffres)
plot(diffres)
auto.arima(nt2,seasonal=F)
Acf(diffres)
Pacf(diffres)
arima(nt2, order=c(3,1,0))

res.mod <- garchFit(tsData = ~ garch(0,2))
#res.mod <- garch(nt, order = c(1,1))
plot(residuals(res.mod), type='l')
ArchTest(residuals(res.mod))



#1,1 = -1106.608



h = 134
Data <- as.numeric(DATA_EMONEY$Jumlah.Transaksi)
Data[8:134] <- NA
g <- as.numeric(nt2)
et <- as.numeric(nt)
n <- data.frame(k=c(1:h),error=NA)
for (k in 7:h) {
  Var[k,2] <- 0.0107614 + (0.1531341+0.8059737)*Var[k-1,2]
  n[k,2] <- as.numeric(rnorm(1,mean=0,sd=1))
  Data[k] <- 2*Data[k-1] - Data[k-2] +
    (-1.0874)*(Data[k-1] -2*Data[k-2] + Data[k-3]) + 
    (-1.1272)*(Data[k-2] -2*Data[k-3] + Data[k-4]) + 
    (-0.8684)*(Data[k-3] -2*Data[k-4] + Data[k-5]) + 
    (-0.4669)*(Data[k-4] -2*Data[k-5] + Data[k-6]) #+ as.numeric((sqrt(Var[k,2])*n[k,2]))
}
plot(tsData)
plot(Data[1:(134)],type='l',col='blue')



Var = data.frame(k=c(1:length(nt2)),variansi=NA)
Var[1,2] <- 0
for ( k in 2:length(nt2)) {
  Var[k,2] <- 0.0107614 + (0.1531341)*nt2[k] +(0.8059737)*Var[k-1,2]
}
plot(Var[,2])
#==============================================================================
set.seed(1234567890) #4,21,123456,1234567890
h = 12
Data <- DATA_EMONEY$Jumlah.Transaksi
g <- as.numeric(nt2)
n <- data.frame(k=c(1:h),error=NA)
for (k in 1:h) {
  Var[134+k,2] <- 0.0107614 + (0.1531341+0.8059737)*Var[134+k-1,2]
  n[k,2] <- as.numeric(rnorm(1,mean=0,sd=1))
  Data[134+k] <- 2*Data[134+k-1] - Data[134+k-2] +
    (-1.0874)*(Data[134+k-1] -2*Data[134+k-2] + Data[134+k-3]) + 
    (-1.1272)*(Data[134+k-2] -2*Data[134+k-3] + Data[134+k-4]) + 
    (-0.8684)*(Data[134+k-3] -2*Data[134+k-4] + Data[134+k-5]) + 
    (-0.4669)*(Data[134+k-4] -2*Data[134+k-5] + Data[134+k-6]) + 
    as.numeric((sqrt(Var[134+k,2])*n[k,2]))
}
plot(Data[1:(134+h)],type='l',col='blue')
lines(Data[1:134],type='l',col='black')
Data[135]
#=================================================================================
set.seed(54321) 
h = 5
Data <- DATA_EMONEY$Jumlah.Transaksi
g <- as.numeric(nt2)
n <- data.frame(k=c(1:h),error=NA)
for (k in 1:h) {
  Var[129+k,2] <- 0.0107614 + (0.1531341+0.8059737)*Var[129+k-1,2]
  n[k,2] <- as.numeric(rnorm(1,mean=0,sd=1))
  Data[129+k] <- 2*Data[129+k-1] - Data[129+k-2] +
    (-1.0874)*(Data[129+k-1] -2*Data[129+k-2] + Data[129+k-3]) + 
    (-1.1272)*(Data[129+k-2] -2*Data[129+k-3] + Data[129+k-4]) + 
    (-0.8684)*(Data[129+k-3] -2*Data[129+k-4] + Data[129+k-5]) + 
    (-0.4669)*(Data[129+k-4] -2*Data[129+k-5] + Data[129+k-6]) + 
    as.numeric((sqrt(Var[129+k,2])*n[k,2]))
}
plot(tsData[129:134],type='l',col='black')
lines(Data[129:(134)],type='l',col='blue')





library(AnalyzeTS)
fit2 <- garch(nt2,order=c(1,1))
mod <- arima(tsData,order=c(4,2,0))
fit2
forecastGARCH(mod,fit2,r=6,trace=TRUE)


#====================================================
fc = forecast(model, h = 12)
plot(fc)

plot(tsData)
lines(fitted(model), col = "blue") 

checkresiduals(model,plot=T)
ArchTest(residuals(model))

nt = residuals(model)
nt2 = nt^2

Acf(nt2,lag.max = 48)
Pacf(nt2)

plot(nt2)

model.res = garch(nt2, order=c(0,3))
auto.arima(nt2)
nt2[1]
nt
