hw <- read.table("Helsinki_weather.csv",header=T,sep=",")
a <- hw[,6]
plot.ts(a)
as.matrix(c(hw[,6]),nrow=24,ncol=365)
matriisi <- matrix(hw[,6],nrow=24,ncol=365)
matriisi
str(matriisi)
t(matriisi)

for (i in 1:n){
  which(matriisi[i]=="NA")
}





