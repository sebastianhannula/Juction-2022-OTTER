df <- read.csv("people_flow.csv", sep=",")
df <- df[-c(1)]

df_series <- ts(df, start = 1, frequency = 24)
plot.ts(df_series)

L1 <- 168
L2 <- 24

diff168 <- diff(df_series,lag=168,differences=1)
diff24 <- diff(diff168, lag=24, differences=1)
plot.ts(diff168)
plot.ts(diff24)

model <- arima(diff168,order=c(0,1,2),seasonal=list(order=c(1,1,1),period=24))
enne <- predict(model,n.ahead=168)
plot.ts(enne$pred)

ennuste <- c(1:168)
clyla = c(1:168)
clala = c(1:168)
for(x in 1:length(ennuste)) {
  ennuste[x] <- df_series[168-L1+x]+enne$pred[x]
  clyla[x] = ennuste[x] + 1.96*enne$se[x]
  clala[x] = ennuste[x] - 1.96*enne$se[x]
}
ennuste = ts(ennuste, start = c(35,1), frequency = 24)
clyla = ts(clyla, start = c(35,1), frequency = 24)
clala = ts(clala, start = c(35,1), frequency = 24)
ts.plot(ennuste, col = c("blue"))

write.csv(ennuste, "people_flow_pred.csv")
