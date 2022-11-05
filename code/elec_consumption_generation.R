eletemp = read.table(file = "sahko.csv", sep = ";", dec = ",", skip = 1)
ele = ts(eletemp[[1]][1:816], start = 1, frequency = 24)


plot(ele)
ele_month <- ele*1.05
plot(ele_month)

ele_week <- ele_month[c((2*24+1):(9*24))]
plot.ts(ele_week)

week1 <- ele_week

res <- rep(NA,365)

n <- 1
T <- 60
t.<-seq(0,2*pi,l=53) #Set for trigonometric multipliers
#t3.<-seq(1,1,l=T) #Set for constant functions
y2<-matrix(NA,n,53) #Set matrix to save values
for(i in 1:n)  y2[i,]<-runif(1,1,1.1)*sin(1*t.-0.75)+runif(1,1,1.1)*cos(1*t.)
min(y2[1,])
y2 <- (y2+5)/5
plot(y2[1,])
weights <- as.array(y2[1,])


for (i in 1:52) {
  for (j in 1:length(ele_week)){
    res[(i-1)*168+j] <- ele_week[j] * runif(1,weights[i]-0.05,weights[i])* (1+i*0.002)
  }
}

plot.ts(res)
write.csv(res, "elec_consumption.csv")
