df <- read.csv("annual_people_flow_nov.csv", sep=",")
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
ennuste[ennuste <0] <- 0

write.csv(ennuste, file="people_flow_pred_nov.csv")

#Create binary vector and constant factor normalized to maximum employee count

df_binary <- df
df_binary[df_binary > 0] <- 1
write.csv(df_binary, "people_flow_binary.csv")

ennuste_binary <- ennuste
as.data.frame(ennuste_binary)
ennuste_binary[ennuste_binary > 0] <- 1
write.csv(ennuste_binary, "people_flow_pred_binary.csv")

df_constant <- df
empl <- max(df_constant)
df_final <- df_constant / empl
write.csv(df_final, "people_constant_factor.csv")

df_pred_constant <- ennuste
df_pred_final <- df_pred_constant / empl
write.csv(df_pred_final, "people_pred_constant_factor.csv")
