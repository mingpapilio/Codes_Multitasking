length<- 10
d1<- 1500
d2<- 1500
d3<- 1500
d4<- 1500
d5<- 1500

raw<- read.csv("c=1.txt",sep="\t",skip=1)
aa<- matrix(NA,2,length)
for(i in 1:length){
  aa[1,i]= mean(raw[,i+1])
  aa[2,i]= sd(raw[,i+1])
}

raw<- read.csv("c=3.txt",sep="\t",skip=1)
bb<- matrix(NA,2,length)
for(i in 1:length){
  bb[1,i]= mean(raw[,i+1])
  bb[2,i]= sd(raw[,i+1])
}

raw<- read.csv("c=5.txt",sep="\t",skip=1)
cc<- matrix(NA,2,length)
for(i in 1:length){
  cc[1,i]= mean(raw[,i+1])
  cc[2,i]= sd(raw[,i+1])
}

raw<- read.csv("c=7.txt",sep="\t",skip=1)
dd<- matrix(NA,2,length)
for(i in 1:length){
  dd[1,i]= mean(raw[,i+1])
  dd[2,i]= sd(raw[,i+1])
}

raw<- read.csv("c=9.txt",sep="\t",skip=1)
ee<- matrix(NA,2,length)
for(i in 1:length){
  ee[1,i]= mean(raw[,i+1])
  ee[2,i]= sd(raw[,i+1])
}

mean_duration<-matrix(NA,5,length)
## row.names(mean_duration)<-c("without size_cost","with size_cost")
mean_duration[1,]<- aa[1,]/d1*100
mean_duration[2,]<- bb[1,]/d2*100
mean_duration[3,]<- cc[1,]/d3*100
mean_duration[4,]<- dd[1,]/d4*100
mean_duration[5,]<- ee[1,]/d5*100

sd_duration<-matrix(NA,5,length)
## row.names(sd_duration)<-c("without size_cost","with size_cost")
sd_duration[1,]<- aa[2,]/sqrt(d1)
sd_duration[2,]<- bb[2,]/sqrt(d2)
sd_duration[3,]<- cc[2,]/sqrt(d3)
sd_duration[4,]<- dd[2,]/sqrt(d4)
sd_duration[5,]<- ee[2,]/sqrt(d5)

dev.new()
par(mai=c(0.8,0.8,0.2,0.2))
plot(mean_duration[1,],type="l",ylim=c(1.5,2.7),
     xlab="",
     ylab="",	
     main="",
     cex.lab=1.75,
     cex.axis=1.25
     )
points(mean_duration[2,],type="l")
points(mean_duration[3,],type="l")
points(mean_duration[4,],type="l")
points(mean_duration[5,],type="l")
title(xlab="Level of Clutch Size Dependent Predation", line=2.5, cex.lab=1.75)
title(ylab="Standardized mean offspring number", line=2.25, cex.lab=1.75)

variable<- c(1:10)
sd_scale<- 0.25
sd_width<- 0.05
segments(variable, mean_duration[1,] - sd_duration[1,]*sd_scale, variable, mean_duration[1,] + sd_duration[1,]*sd_scale, col="lightgrey")
segments(variable, mean_duration[2,] - sd_duration[2,]*sd_scale, variable, mean_duration[2,] + sd_duration[2,]*sd_scale, col="lightgrey")
segments(variable, mean_duration[3,] - sd_duration[3,]*sd_scale, variable, mean_duration[3,] + sd_duration[3,]*sd_scale, col="lightgrey")
segments(variable, mean_duration[4,] - sd_duration[4,]*sd_scale, variable, mean_duration[4,] + sd_duration[4,]*sd_scale, col="lightgrey")
segments(variable, mean_duration[5,] - sd_duration[5,]*sd_scale, variable, mean_duration[5,] + sd_duration[5,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[1,] - sd_duration[1,]*sd_scale, variable + sd_width, mean_duration[1,] - sd_duration[1,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[2,] - sd_duration[2,]*sd_scale, variable + sd_width, mean_duration[2,] - sd_duration[2,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[3,] - sd_duration[3,]*sd_scale, variable + sd_width, mean_duration[3,] - sd_duration[3,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[4,] - sd_duration[4,]*sd_scale, variable + sd_width, mean_duration[4,] - sd_duration[4,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[5,] - sd_duration[5,]*sd_scale, variable + sd_width, mean_duration[5,] - sd_duration[5,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[1,] + sd_duration[1,]*sd_scale, variable + sd_width, mean_duration[1,] + sd_duration[1,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[2,] + sd_duration[2,]*sd_scale, variable + sd_width, mean_duration[2,] + sd_duration[2,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[3,] + sd_duration[3,]*sd_scale, variable + sd_width, mean_duration[3,] + sd_duration[3,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[4,] + sd_duration[4,]*sd_scale, variable + sd_width, mean_duration[4,] + sd_duration[4,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[5,] + sd_duration[5,]*sd_scale, variable + sd_width, mean_duration[5,] + sd_duration[5,]*sd_scale, col="lightgrey")

points(mean_duration[1,],type="p",pch=1,cex=2)
points(mean_duration[2,],type="p",pch=2,cex=2)
points(mean_duration[3,],type="p",pch=15,cex=2)
points(mean_duration[4,],type="p",pch=17,cex=2)
points(mean_duration[5,],type="p",pch=19,cex=2)

legend("bottomright",c("c=1","c=3","c=5","c=7","c=9"),pch=c(1,2,15,17,19),cex=1.25)


# Only for season length, use thes codes to stansardize the data
for(i in 1:5){
  for(j in 1:length){
    if (i==1){ 
      mean_duration[i,j]= aa[1,j]/(500*j)*100 
      sd_duration[i,j]= aa[2,j]/sqrt(500*j)
    }
    if (i==2){ 
      mean_duration[i,j]= bb[1,j]/(500*j)*100 
      sd_duration[i,j]= bb[2,j]/sqrt(500*j)
    }
    if (i==3){ 
      mean_duration[i,j]= cc[1,j]/(500*j)*100 
      sd_duration[i,j]= cc[2,j]/sqrt(500*j)
    }
    if (i==4){ 
      mean_duration[i,j]= dd[1,j]/(500*j)*100 
      sd_duration[i,j]= dd[2,j]/sqrt(500*j)
    }
    if (i==5){ 
      mean_duration[i,j]= ee[1,j]/(500*j)*100 
      sd_duration[i,j]= ee[2,j]/sqrt(500*j)
    }
}}
