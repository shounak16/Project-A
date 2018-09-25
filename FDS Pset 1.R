rm(list = ls())
snp500 = read.csv("snp500.csv")
library(quantmod)

#1a
t=0
Date=snp500$Dates
OpenPrice=snp500$PX_OPEN
ClosePrice=snp500$PX_LAST
HighPrice=snp500$PX_HIGH
LowPrice=snp500$PX_LOW
for (i in 1:22780){
  if(OpenPrice[i]==" "|OpenPrice[i]=="NA"|OpenPrice[i]=="N/A"){
    t=t+1
  }
  else if(ClosePrice[i]==" "|ClosePrice[i]=="NA"|ClosePrice[i]=="N/A"){
    t=t+1
  }
  else if(HighPrice[i]==" "|HighPrice[i]=="NA"|HighPrice[i]=="N/A"){
    t=t+1
  }
  else if(LowPrice[i]==" "|LowPrice[i]=="NA"|LowPrice[i]=="N/A"){
    t=t+1
  }
  else{NULL}
}

#1b
EqualPrice=0
NegPrice=0
for (i in 1:22780){
  if(OpenPrice[i]==ClosePrice[i]&&HighPrice[i]==LowPrice[i]){
    EqualPrice=EqualPrice+1
  }
  else if(OpenPrice[i]<0 |ClosePrice[i]<0|LowPrice[i]<0|HighPrice[i]<0){
    NegPrice=NegPrice+1
  }
}


#1c
clean <- index(snp500)[LowPrice != HighPrice]
ProbHighOpen=mean(OpenPrice[clean]==HighPrice[clean])
ProbHighClose=mean(ClosePrice[clean]==HighPrice[clean])
ProbLowOpen=mean(OpenPrice[clean]==LowPrice[clean])
ProbLowClose=mean(ClosePrice[clean]==LowPrice[clean])

#1d
Intraday=0
for(i in 1:length(ClosePrice))
{
  Intraday[i]=(HighPrice[i]-LowPrice[i])/LowPrice[i]  
}
snp500$Intraday<-Intraday
snp500V2<-snp500[13023:21014,]
IntradaySort=sort(snp500V2$Intraday, decreasing=TRUE)
IntradaySort[0:19]

Intradaydata<-snp500V2[order(-snp500V2$Intraday),]
print(Intradaydata[c(1:20),c(1,6)])

#1e
Overnight=0
for(i in 2:length(ClosePrice))
{
  Overnight[i]=(OpenPrice[i]-ClosePrice[i-1])/ClosePrice[i-1]  
}
snp500$Overnight<-Overnight
snp500V3<-snp500[13024:21014,]
OvernightSort=sort(snp500V3$Overnight, decreasing=TRUE)
head(OvernightSort, n=20)
tail(OvernightSort, n=20)

OvernightData<-snp500V3[order(-snp500V3$Overnight),]
print(OvernightData[c(1:20),c(1,7)])
print(OvernightData[c((7991-20):7991),c(1,7)])

#1f
returns=0
for(i in 2:length(ClosePrice))
{
  returns[i]=log(ClosePrice[i]/ClosePrice[i-1])
}

lagtime=0
jump=0
StDev=0

for(i in 65:length(ClosePrice)){
  
  lagtime=i-63
  LaggedReturns=returns[lagtime:(i-1)]
  StDev[i]=sd(LaggedReturns)
  jump[i]=returns[i]/StDev[i]
}
snp500$jump<-jump
snp500V4<-snp500[64:22780,]
jumpSort=sort(abs(jump), decreasing=TRUE)
jumpSort[0:19]

JumpData<-snp500V4[order(-abs(snp500V4$jump)),]
print(JumpData[c(1:20),c(1,8)])

#2
symbol="^GSPC"
startday=as.Date("1982-10-05")
endday=as.Date("1982-10-08")
getSymbols(symbol, src = "yahoo",from = startday, to = endday)
#Snapshot of the Data
head(GSPC)[2]
x=c(OpenPrice[13721],HighPrice[13721],LowPrice[13721],ClosePrice[13721])

#3
Dow<-read.csv("AnData.csv")
Dow1<-Dow[-3,]
Dow2<-Dow[-30,]
Dow3<-Dow[c(-3,-30),]
Divisor=(sum(Dow1$Close))/17856.78
ATTWeight=Dow1$Close[29]/(sum(Dow$Close))

BeforeWeights= 0
for(i in 1:30){
  BeforeWeights[i]=Dow1$Close[i]/sum(Dow1$Close)
}
Dow1$BeforeWeights=BeforeWeights
print(Dow1[c(1:30),c(1,6)])

AfterWeights= 0
for(i in 1:30){
  AfterWeights[i]=Dow2$Close[i]/sum(Dow2$Close)
}
Dow2$AfterWeights=AfterWeights
print(Dow2[c(1:30),c(1,6)])

AmazonDivisor=(sum(Dow3$Close)+380.09)/17856.78
BerkshireDivisor=(sum(Dow3$Close)+221353)/17856.78


  



