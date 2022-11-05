nollat <- rep(0,6)
unif1 <- floor(runif(4,0,150))
unif2 <- floor(runif(4,100,150))
unif3 <- floor(runif(4,100,150))
unif4 <- floor(runif(4,0,150))
nollat2 <- rep(0,2)
daily1 <- c(nollat,sort(unif1),sort(unif2,decreasing=T),sort(unif3,decreasing=T),sort(unif4,decreasing=T),nollat2)
plot.ts(daily1)

nollat <- rep(0,6)
unif11<- floor(runif(4,0,150))
unif21<- floor(runif(4,100,150))
unif31<- floor(runif(4,100,150))
unif41<- floor(runif(4,0,150))
nollat2 <- rep(0,2)
daily2 <- c(nollat,sort(unif11),sort(unif21,decreasing=T),sort(unif31,decreasing=T),sort(unif41,decreasing=T),nollat2)
plot.ts(daily2)

nollat <- rep(0,6)
unif12<- floor(runif(4,0,150))
unif22<- floor(runif(4,100,150))
unif32<- floor(runif(4,100,150))
unif42<- floor(runif(4,0,150))
nollat2 <- rep(0,2)
daily3 <- c(nollat,sort(unif12),sort(unif22,decreasing=T),sort(unif32,decreasing=T),sort(unif42,decreasing=T),nollat2)
plot.ts(daily3)

nollat <- rep(0,7)
unif13<- floor(runif(3,0,150))
unif23<- floor(runif(4,100,150))
unif33<- floor(runif(4,100,150))
unif43<- floor(runif(3,0,150))
nollat2 <- rep(0,3)
daily4 <- c(nollat,sort(unif13),sort(unif23,decreasing=T),sort(unif33,decreasing=T),sort(unif43,decreasing=T),nollat2)
plot.ts(daily4)

nollat <- rep(0,7)
unif14<- floor(runif(3,0,50))
unif24<- floor(runif(4,100,150))
unif34<- floor(runif(4,100,150))
unif44<- floor(runif(3,0,50))
nollat2 <- rep(0,3)
daily5 <- c(nollat,sort(unif14),sort(unif24,decreasing=T),sort(unif34,decreasing=T),sort(unif44,decreasing=T),nollat2)
plot.ts(daily5)

nollat <- rep(0,8)
unif15 <- runif(4,0,10)
unif25 <- runif(4,0,10)
nollat2 <- rep(0,8)
daily6 <- c(nollat,sort(unif15),sort(unif25,decreasing=T),nollat2)
plot.ts(daily6)

nollat <- rep(0,8)
unif35 <- runif(4,0,20)
unif35 <- runif(4,0,20)
nollat2 <- rep(0,8)
daily7 <- c(nollat,sort(unif15),sort(unif25,decreasing=T),nollat2)
plot.ts(daily7)

plot.ts(c(daily1,daily2,daily3,daily4,daily5,daily6,daily7))

viikko <- c(daily1,daily2,daily3,daily4,daily5,daily6,daily7)
class(viikko)

malli <- arima(ts(viikko),order=c(3,0,0),seasonal=list(order=c(1,0,1),period=24))

enne <- predict(malli,n.ahead=200)
plot.ts(enne$pred)

hoo <- bats(ts(viikko),seasonal.periods=c(24,168))
fc1 <- forecast(hoo)

bcg <- read.table("bcg_data.csv",sep=";",header=T)
joku <- floor(c(bcg[,2],bcg[,3],bcg[,4],bcg[,5],bcg[,6],bcg[,7],bcg[,8]))
class(joku)

pl <- c(viikko,joku)

hoo1 <- bats(ts(pl),seasonal.periods=c(24,168))
fc2 <- forecast(hoo1,h=7*168)
plot(fc2$mean)

jokurandom <- fc2$mean

jokurandom[jokurandom < 0] <- 0
plot(jokurandom)
jokurandom
kevat <- jokurandom

for (i in 1:length(kevat)){
  kevat[i] <- kevat[i] * runif(1,0.85,1.15)
}
kevat1 <- kevat
plot.ts(kevat1,col="red")

kevat <- jokurandom
for (i in 1:length(kevat)){
  kevat[i] <- kevat[i] * runif(1,0.9,1.1)
}
kevat2 <- kevat
plot.ts(kevat2,col="red")

kevat <- jokurandom
for (i in 1:length(kevat)){
  kevat[i] <- kevat[i] * runif(1,0.75,1.2)
}
kevat3 <- kevat
plot.ts(kevat3, col="red")



fc3 <- forecast(hoo1,h=4*168)
plot(fc3$mean)
jokurandom1 <- fc3$mean

jokurandom1[jokurandom1 < 0] <- 0
plot(jokurandom1)
kesa <- jokurandom1

for (i in 1:length(kesa)){
  kesa[i] <- kesa[i] * runif(1,0.6,0.7)
}
plot.ts(kesa, col="red")

heina <- jokurandom1
for (i in 1:length(heina)){
  heina[i] <- heina[i] * runif(1,0.35,0.55)
}
plot.ts(heina,col="red")

elo <- jokurandom1
for (i in 1:length(elo)){
  elo[i] <- elo[i] * runif(1,0.6,0.7)
}
plot.ts(elo,col="red")




fc4 <- forecast(hoo1,h=9*168)
plot(fc4$mean)
jokurandom2 <- fc4$mean

jokurandom2[jokurandom2 < 0] <- 0
plot(jokurandom2)
syksy1 <- jokurandom2

for (i in 1:length(syksy1)){
  syksy1[i] <- syksy1[i] * runif(1,0.85,1.15)
}
plot.ts(syksy1,col="red")

fc5 <- forecast(hoo1,h=9*168)
jokurandom3 <- fc5$mean
jokurandom3[jokurandom3 < 0] <- 0
syksy2 <- jokurandom3
for (i in 1:length(syksy2)){
  syksy2[i] <- syksy2[i] * runif(1,0.85,1.15)
}
plot.ts(syksy2,col="red")


fc6 <- forecast(hoo1,h=1.143*168)
plot(fc6$mean)

jokurandom3 <- fc6$mean

jokurandom3[jokurandom3 < 0] <- 0
plot(jokurandom3)
joulu <- jokurandom3

for (i in 1:length(joulu)){
  joulu[i] <- joulu[i] * runif(1,0.03,0.06)
}
plot.ts(joulu,col="red")


vuosi <- c(kevat1,kevat2,kevat3,kesa,heina,elo,syksy1,syksy2,joulu)
vuosi <- floor(vuosi)
plot.ts(vuosi,col="blue")

plot.ts(vuosi,col="blue",ylab="# of employees at office",xlab="\n")
axis(1,at=1:length(vuosi),LETTERS[1:length(vuosi)])

plot.ts(vuosi[1:168])
finalprediction <- forecast(hoo1,h=24)
plot(finalprediction$mean)

jokuenne <- predict(hoo1,n.ahead=20)
plot.ts(jokuenne$fitted)

diff168 <- diff(vuosi,lag=168,differences=1)
diff24 <- diff(diff168, lag=24, differences=1)
plot.ts(diff168)
plot.ts(diff24)

model <- arima(diff168,order=c(0,1,2),seasonal=list(order=c(1,1,1),period=24))
enne <- predict(model,n.ahead=168)
plot.ts(enne$pred)

L1 <- 168

ennuste <- c(1:168)
clyla = c(1:168)
clala = c(1:168)
for(x in 1:length(ennuste)) {
  ennuste[x] <- vuosi[168-L1+x]+enne$pred[x]
  clyla[x] = ennuste[x] + 1.96*enne$se[x]
  clala[x] = ennuste[x] - 1.96*enne$se[x]
}
ennuste = ts(ennuste, start = c(35,1), frequency = 24)
clyla = ts(clyla, start = c(35,1), frequency = 24)
clala = ts(clala, start = c(35,1), frequency = 24)
ts.plot(ennuste, col="blue")#clyla, clala, col = c("blue", "blue", "blue"))


