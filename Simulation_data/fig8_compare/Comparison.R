fs= function(c){
  ff= 1
  h= 0.03
  tr=24
  cmax= 10
  x= 1-ff*(1-c/cmax)
  pp= (1+c*x/ff)
  tr/12*pp*(1-h)^tr*pp/(1+pp*(1-h)^tr)
}
ft= function(c){
  ff= 1
  h= 0.03
  tr=24
  cmax= 10
  s1= 0 ## switch for compensating foraging
  x= 1-ff*(1-c/cmax)*s1
  T= 10000
  pp= (1+c*x/ff)*(1-h)^tr
  tr/2/(1+pp)+ pp*tr*(1+c*x/ff)/2/(1+pp)
}

fun<- matrix(NA,2,10000)
fx<- seq(0,10,length.out=10000)
colnames(fun)<- fx
for (i in 1:10000){
  fun[1,i]= ft(fx[i])
  fun[2,i]= fs(fx[i])
}

aa<-read.csv("ns_10000.txt",sep="\t",skip=1)
data<- matrix(NA,10000,10,dimnames=list(rownames(aa),c(1:10)))
for(i in 1:10){
  data[,i]= aa[,i+1]
}
ana<- matrix(NA,2,10)
colnames(ana)=c(1:10)
for(i in 1:10){
  ana[1,i]= mean(data[,i])
  ana[2,i]= sd(data[,i])
}

# plot
sam_mean <- ana0[1,]
sam_sd <- ana0[2,]
variable <- c(1:10)
par(mai=c(0,1.5,0.5,0.5))
dev.new()
plot(1:length(sam_mean), 
     sam_mean, type = "p", 
     pch= 16, 
     lty = 1,
     ylim = c(0,700),
     xlab="clutch size",
     ylab="wasted time",
     cex.lab=2,
     cex.axis=1,
     )
segments(variable, sam_mean - sam_sd, variable, sam_mean + sam_sd)
segments(variable - .1, sam_mean + sam_sd, variable + .1, sam_mean + sam_sd)
segments(variable - .1, sam_mean - sam_sd, variable + .1, sam_mean - sam_sd)
sam_mean <- fun[1,]
sam_sd <- fun[2,]
variable <- fx
segments(variable, sam_mean - sam_sd, variable, sam_mean + sam_sd, col="lightblue")
