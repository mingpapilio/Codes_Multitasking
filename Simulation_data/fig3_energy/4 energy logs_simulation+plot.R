####Parental Investment model, Aug29, 2016, by Ming Liu

d1 <- 500
d2 <- 1000
d3 <- 1500
d4 <- 10000

fs1 <- 1
fs2 <- 1
fs3 <- 1
fs4 <- 1

## Basic parameters
n  <- d1 		#time limit
rt <- 1000 	 	#repetition times
e  <- rep(0,n)	#energy state
ee <- rep(0,n)	#invested energy (egg number)
r  <- 0  		#state parameter (reproducing/foraging)
t1 <- 0  		#time recorder for reproduction
up <- 500		#upper bound of energy reserve
lw <- 250		#lower bound of energy reserve

## Cost of reproduction and foraging
nc = 0			#basic cost per attempt (nest building cost)
bc = 0			#basic cost per unit of offspring
rp1 <- 24		#standard reproductive period
rp2 <- 0		#penalty parameter (clutch size dependent)
rc1 <- 1		#standard maintainance cost per unit of offspring
xx = c(1:10)	#value of vectors
ll = length(xx)
fs <- fs1  		#mean of foraging success in each time step
s1 = 1			#switch for compensatng feeding (during reproduction)
si = 1.1		#index of conpensating feeding
eth = 750		#energy threshold for quitting
s2 = 0			#switch for quitting in the middle

## Environmental change
flc = 1			#switch for the presence of environmental change
thr = 0			#environment output value, variable
ot  = 0			#temp for remaining offspring
m  = 0.97		#mean of survival rate
vv = 0.01		#variance coefficient (environmental fluctuation)
a <- m/vv		#a (parameter) for beta distribution
b <- (1-m)/vv	#b (parameter) for beta distribution

## Recording
acq = matrix(0,rt,ll)  								#offspring number
	rownames(acq) = c(1:rt)
	colnames(acq) = xx
inv = matrix(0,rt,ll)  								#offspring number
	rownames(inv) = c(1:rt)
	colnames(inv) = xx
	
## Main loop
for (k in 1:rt){
for (j in 1:ll){

## Reset parameters
p <- xx[j]											#unit of offspring planning to reproduce
r = 0
e[1] <- lw
ee[1]<- 0
t1 = 0
ss = 0
p1 = 0												#temp for offspring number
p2 = 0
rp <- rp1+ p*rp2*rp1/10  							#reproductive period
rc <- rc1*rp1/rp

## Core loop
for (i in 1:n){
	if (r < 1){
	if (e[i] >= (lw+ nc+ p*bc+ p*rc*rp) | e[i] >= up){  #Is the organism ready to reproduce?
	e[i+1]  <- e[i]-nc -p*bc 							#reproduce investment (on eggs)
	ee[i+1] <- p
	r  = 1
	t1 = i											#starting period of reproduction
	rr = e[i]										#initial energy state
	}
	else{                    						#foraging
	e[i+1]  <- e[i]+ fs
	p1 <- p1+ fs
	ee[i+1] <- ee[i]
	}}

	if (r == 1 & i > t1){								#specify parental caring period
	e[i+1]  = e[i]- p*rc+ fs*(1-(p/10)^si)*s1			#breeding maintainance cost and foraging ####Important change 
	ee[i+1] = ee[i]
	p1 <- p1+ fs*(1-(p/10)^si)*s1
	p2 <- p2+ p*rc
	}

	if (r == 1 & i > t1 & flc == 1 ){					#Environmental change
	thr <- rbeta(1,a,b)
	ot  <- ee[i] 
	pp <- runif(1,0,1)
	if (pp > thr){
	r  = 0
	t1 = 0
	ee[i+1] = 0
	}}

	if (r == 1 & i >= t1+rp){							#breeding period, wihout penalty *(1+p)
	p1 = p1+ ee[i+1]
	t1 = 0
	r  = 0
	ee[i+1] = 0}

}
	acq[k,j] <- p1
	inv[k,j] <- p2
}}

data <- acq
variable <- xx
colnames(data) <- variable
mean_a1 <- apply(data, 2, mean) 	# average
sd_a1 <- apply(data, 2, sd) 		# standard deviation

data <- inv
variable <- xx
colnames(data) <- variable
mean_i1 <- apply(data, 2, mean) 	# average
sd_i1 <- apply(data, 2, sd) 		# standard deviation

##### Reset
n  <- d2 		#time limit
fs <- fs2
## Recording
acq = matrix(0,rt,ll)  								#offspring number
	rownames(acq) = c(1:rt)
	colnames(acq) = xx
inv = matrix(0,rt,ll)  								#offspring number
	rownames(inv) = c(1:rt)
	colnames(inv) = xx
	
## Main loop
for (k in 1:rt){
for (j in 1:ll){

## Reset parameters
p <- xx[j]											#unit of offspring planning to reproduce
r = 0
e[1] <- lw
ee[1]<- 0
t1 = 0
ss = 0
p1 = 0												#temp for offspring number
p2 = 0
rp <- rp1+ p*rp2*rp1/10  							#reproductive period
rc <- rc1*rp1/rp

## Core loop
for (i in 1:n){
	if (r < 1){
	if (e[i] >= (lw+ nc+ p*bc+ p*rc*rp) | e[i] >= up){  #Is the organism ready to reproduce?
	e[i+1]  <- e[i]-nc -p*bc 							#reproduce investment (on eggs)
	ee[i+1] <- p
	r  = 1
	t1 = i											#starting period of reproduction
	rr = e[i]										#initial energy state
	}
	else{                    						#foraging
	e[i+1]  <- e[i]+ fs
	p1 <- p1+ fs
	ee[i+1] <- ee[i]
	}}

	if (r == 1 & i > t1){								#specify parental caring period
	e[i+1]  = e[i]- p*rc+ fs*(1-(p/10)^si)*s1			#breeding maintainance cost and foraging ####Important change 
	ee[i+1] = ee[i]
	p1 <- p1+ fs*(1-(p/10)^si)*s1
	p2 <- p2+ p*rc
	}

	if (r == 1 & i > t1 & flc == 1 ){					#Environmental change
	thr <- rbeta(1,a,b)
	ot  <- ee[i] 
	pp <- runif(1,0,1)
	if (pp > thr){
	r  = 0
	t1 = 0
	ee[i+1] = 0
	}}

	if (r == 1 & i >= t1+rp){							#breeding period, wihout penalty *(1+p)
	p1 = p1+ ee[i+1]
	t1 = 0
	r  = 0
	ee[i+1] = 0}

}
	acq[k,j] <- p1
	inv[k,j] <- p2
}}

data <- acq
variable <- xx
colnames(data) <- variable
mean_a2 <- apply(data, 2, mean) 	# average
sd_a2 <- apply(data, 2, sd) 		# standard deviation

data <- inv
variable <- xx
colnames(data) <- variable
mean_i2 <- apply(data, 2, mean) 	# average
sd_i2 <- apply(data, 2, sd) 		# standard deviation

##### Reset
n  <- d3 		#time limit
fs <- fs3
## Recording
acq = matrix(0,rt,ll)  								#offspring number
	rownames(acq) = c(1:rt)
	colnames(acq) = xx
inv = matrix(0,rt,ll)  								#offspring number
	rownames(inv) = c(1:rt)
	colnames(inv) = xx
	
## Main loop
for (k in 1:rt){
for (j in 1:ll){

## Reset parameters
p <- xx[j]											#unit of offspring planning to reproduce
r = 0
e[1] <- lw
ee[1]<- 0
t1 = 0
ss = 0
p1 = 0												#temp for offspring number
p2 = 0
rp <- rp1+ p*rp2*rp1/10  							#reproductive period
rc <- rc1*rp1/rp

## Core loop
for (i in 1:n){
	if (r < 1){
	if (e[i] >= (lw+ nc+ p*bc+ p*rc*rp) | e[i] >= up){  #Is the organism ready to reproduce?
	e[i+1]  <- e[i]-nc -p*bc 							#reproduce investment (on eggs)
	ee[i+1] <- p
	r  = 1
	t1 = i											#starting period of reproduction
	rr = e[i]										#initial energy state
	}
	else{                    						#foraging
	e[i+1]  <- e[i]+ fs
	p1 <- p1+ fs
	ee[i+1] <- ee[i]
	}}

	if (r == 1 & i > t1){								#specify parental caring period
	e[i+1]  = e[i]- p*rc+ fs*(1-(p/10)^si)*s1			#breeding maintainance cost and foraging ####Important change 
	ee[i+1] = ee[i]
	p1 <- p1+ fs*(1-(p/10)^si)*s1
	p2 <- p2+ p*rc
	}

	if (r == 1 & i > t1 & flc == 1 ){					#Environmental change
	thr <- rbeta(1,a,b)
	ot  <- ee[i] 
	pp <- runif(1,0,1)
	if (pp > thr){
	r  = 0
	t1 = 0
	ee[i+1] = 0
	}}

	if (r == 1 & i >= t1+rp){							#breeding period, wihout penalty *(1+p)
	p1 = p1+ ee[i+1]
	t1 = 0
	r  = 0
	ee[i+1] = 0}

}
	acq[k,j] <- p1
	inv[k,j] <- p2
}}

data <- acq
variable <- xx
colnames(data) <- variable
mean_a3 <- apply(data, 2, mean) 	# average
sd_a3 <- apply(data, 2, sd) 		# standard deviation

data <- inv
variable <- xx
colnames(data) <- variable
mean_i3 <- apply(data, 2, mean) 	# average
sd_i3 <- apply(data, 2, sd) 		# standard deviation

##### Reset
n  <- d4 		#time limit
fs <- fs4
## Recording
acq = matrix(0,rt,ll)  								#offspring number
	rownames(acq) = c(1:rt)
	colnames(acq) = xx
inv = matrix(0,rt,ll)  								#offspring number
	rownames(inv) = c(1:rt)
	colnames(inv) = xx
	
## Main loop
for (k in 1:rt){
for (j in 1:ll){

## Reset parameters
p <- xx[j]											#unit of offspring planning to reproduce
r = 0
e[1] <- lw
ee[1]<- 0
t1 = 0
ss = 0
p1 = 0												#temp for offspring number
p2 = 0
rp <- rp1+ p*rp2*rp1/10  							#reproductive period
rc <- rc1*rp1/rp

## Core loop
for (i in 1:n){
	if (r < 1){
	if (e[i] >= (lw+ nc+ p*bc+ p*rc*rp) | e[i] >= up){  #Is the organism ready to reproduce?
	e[i+1]  <- e[i]-nc -p*bc 							#reproduce investment (on eggs)
	ee[i+1] <- p
	r  = 1
	t1 = i											#starting period of reproduction
	rr = e[i]										#initial energy state
	}
	else{                    						#foraging
	e[i+1]  <- e[i]+ fs
	p1 <- p1+ fs
	ee[i+1] <- ee[i]
	}}

	if (r == 1 & i > t1){								#specify parental caring period
	e[i+1]  = e[i]- p*rc+ fs*(1-(p/10)^si)*s1			#breeding maintainance cost and foraging ####Important change 
	ee[i+1] = ee[i]
	p1 <- p1+ fs*(1-(p/10)^si)*s1
	p2 <- p2+ p*rc
	}

	if (r == 1 & i > t1 & flc == 1 ){					#Environmental change
	thr <- rbeta(1,a,b)
	ot  <- ee[i] 
	pp <- runif(1,0,1)
	if (pp > thr){
	r  = 0
	t1 = 0
	ee[i+1] = 0
	}}

	if (r == 1 & i >= t1+rp){							#breeding period, wihout penalty *(1+p)
	p1 = p1+ ee[i+1]
	t1 = 0
	r  = 0
	ee[i+1] = 0}

}
	acq[k,j] <- p1
	inv[k,j] <- p2
}}

data <- acq
variable <- xx
colnames(data) <- variable
mean_a4 <- apply(data, 2, mean) 	# average
sd_a4 <- apply(data, 2, sd) 		# standard deviation

data <- inv
variable <- xx
colnames(data) <- variable
mean_i4 <- apply(data, 2, mean) 	# average
sd_i4 <- apply(data, 2, sd) 		# standard deviation

#####plot

mean_duration<-matrix(NA,4,10)
row.names(mean_duration)<-c("500","1000","1500","10000")
mean_duration[1,]<- mean_a1/d1*100
mean_duration[2,]<- mean_a2/d2*100
mean_duration[3,]<- mean_a3/d3*100
mean_duration[4,]<- mean_a4/d4*100

sd_duration<-matrix(NA,4,10)
row.names(sd_duration)<-c("500","1000","1500","10000")
sd_duration[1,]<- sd_a1
sd_duration[2,]<- sd_a2
sd_duration[3,]<- sd_a3
sd_duration[4,]<- sd_a4

dev.new()
par(mai=c(1,1,0.5,0.5))
plot(mean_duration[1,],type="l",ylim=c(85,95),
     xlab="brood size",
     ylab="standardized mean offspring number",	
     main="",
     cex.lab=2,
     cex.axis=1)
points(mean_duration[2,],type="l",lty=2)
points(mean_duration[3,],type="l",lty=3)
points(mean_duration[4,],type="l",lty=5)
legend("topleft",c("500","1000","1500","10000"),lty=c(1,2,3,5))
variable <- xx
sam_mean <- mean_duration[1,] 	# average
sam_sd   <- sd_duration[1,]		# standard deviation
segments(variable, sam_mean - sam_sd, variable, sam_mean + sam_sd)
segments(variable - .1, sam_mean + sam_sd, variable + .1, sam_mean + sam_sd)
segments(variable - .1, sam_mean - sam_sd, variable + .1, sam_mean - sam_sd)
sam_mean <- mean_duration[2,] 	# average
sam_sd   <- sd_duration[2,]		# standard deviation
segments(variable, sam_mean - sam_sd, variable, sam_mean + sam_sd)
segments(variable - .1, sam_mean + sam_sd, variable + .1, sam_mean + sam_sd)
segments(variable - .1, sam_mean - sam_sd, variable + .1, sam_mean - sam_sd)
sam_mean <- mean_duration[3,] 	# average
sam_sd   <- sd_duration[3,]		# standard deviation
segments(variable, sam_mean - sam_sd, variable, sam_mean + sam_sd)
segments(variable - .1, sam_mean + sam_sd, variable + .1, sam_mean + sam_sd)
segments(variable - .1, sam_mean - sam_sd, variable + .1, sam_mean - sam_sd)
sam_mean <- mean_duration[4,] 	# average
sam_sd   <- sd_duration[4,]		# standard deviation
segments(variable, sam_mean - sam_sd, variable, sam_mean + sam_sd)
segments(variable - .1, sam_mean + sam_sd, variable + .1, sam_mean + sam_sd)
segments(variable - .1, sam_mean - sam_sd, variable + .1, sam_mean - sam_sd)

mean_duration<-matrix(NA,4,10)
row.names(mean_duration)<-c("500","1000","1500","10000")
mean_duration[1,]<- mean_i1/d1*100
mean_duration[2,]<- mean_i2/d2*100
mean_duration[3,]<- mean_i3/d3*100
mean_duration[4,]<- mean_i4/d4*100

sd_duration<-matrix(NA,4,10)
row.names(sd_duration)<-c("500","1000","1500","10000")
sd_duration[1,]<- sd_i1
sd_duration[2,]<- sd_i2
sd_duration[3,]<- sd_i3
sd_duration[4,]<- sd_i4

dev.new()
par(mai=c(1,1,0.5,0.5))
plot(mean_duration[1,],type="l",ylim=c(75,95),
     xlab="brood size",
     ylab="standardized mean offspring number",	
     main="",
     cex.lab=2,
     cex.axis=1)
points(mean_duration[2,],type="l",lty=2)
points(mean_duration[3,],type="l",lty=3)
points(mean_duration[4,],type="l",lty=5)
legend("topleft",c("500","1000","1500","10000"),lty=c(1,2,3,5))
variable <- xx
sam_mean <- mean_duration[1,] 	# average
sam_sd   <- sd_duration[1,]		# standard deviation
segments(variable, sam_mean - sam_sd, variable, sam_mean + sam_sd)
segments(variable - .1, sam_mean + sam_sd, variable + .1, sam_mean + sam_sd)
segments(variable - .1, sam_mean - sam_sd, variable + .1, sam_mean - sam_sd)
sam_mean <- mean_duration[2,] 	# average
sam_sd   <- sd_duration[2,]		# standard deviation
segments(variable, sam_mean - sam_sd, variable, sam_mean + sam_sd)
segments(variable - .1, sam_mean + sam_sd, variable + .1, sam_mean + sam_sd)
segments(variable - .1, sam_mean - sam_sd, variable + .1, sam_mean - sam_sd)
sam_mean <- mean_duration[3,] 	# average
sam_sd   <- sd_duration[3,]		# standard deviation
segments(variable, sam_mean - sam_sd, variable, sam_mean + sam_sd)
segments(variable - .1, sam_mean + sam_sd, variable + .1, sam_mean + sam_sd)
segments(variable - .1, sam_mean - sam_sd, variable + .1, sam_mean - sam_sd)
sam_mean <- mean_duration[4,] 	# average
sam_sd   <- sd_duration[4,]		# standard deviation
segments(variable, sam_mean - sam_sd, variable, sam_mean + sam_sd)
segments(variable - .1, sam_mean + sam_sd, variable + .1, sam_mean + sam_sd)
segments(variable - .1, sam_mean - sam_sd, variable + .1, sam_mean - sam_sd)


library (beepr)
beep("ping")
beep("ping")
beep("ping")
#############################

mean_duration<-matrix(NA,3,10)
row.names(mean_duration)<-c("500","1500","10000")
mean_duration[1,]<- mean_i1/d1*100
mean_duration[2,]<- mean_i3/d3*100
mean_duration[3,]<- mean_i4/d4*100

sd_duration<-matrix(NA,3,10)
row.names(sd_duration)<-c("500","1500","10000")
sd_duration[1,]<- sd_i1/sqrt(d1)
sd_duration[2,]<- sd_i3/sqrt(d3)
sd_duration[3,]<- sd_i4/sqrt(d4)

dev.new()
par(mai=c(0.8,0.8,0.2,0.2))
plot(mean_duration[1,],type="l",ylim=c(55,90),
     xlab="",
     ylab="",	
     main="",
     cex.lab=1.75,
     cex.axis=1.25
)
points(mean_duration[2,],type="l")
points(mean_duration[3,],type="l")
title(xlab="Clutch size", line=2.5, cex.lab=1.75)
title(ylab="Excessive reserve of time", line=2.25, cex.lab=1.75)

variable<- c(1:10)
sd_scale<- 1
sd_width<- 0.05
segments(variable, mean_duration[1,] - sd_duration[1,]*sd_scale, variable, mean_duration[1,] + sd_duration[1,]*sd_scale, col="lightgrey")
segments(variable, mean_duration[2,] - sd_duration[2,]*sd_scale, variable, mean_duration[2,] + sd_duration[2,]*sd_scale, col="lightgrey")
segments(variable, mean_duration[3,] - sd_duration[3,]*sd_scale, variable, mean_duration[3,] + sd_duration[3,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[1,] - sd_duration[1,]*sd_scale, variable + sd_width, mean_duration[1,] - sd_duration[1,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[2,] - sd_duration[2,]*sd_scale, variable + sd_width, mean_duration[2,] - sd_duration[2,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[3,] - sd_duration[3,]*sd_scale, variable + sd_width, mean_duration[3,] - sd_duration[3,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[1,] + sd_duration[1,]*sd_scale, variable + sd_width, mean_duration[1,] + sd_duration[1,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[2,] + sd_duration[2,]*sd_scale, variable + sd_width, mean_duration[2,] + sd_duration[2,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[3,] + sd_duration[3,]*sd_scale, variable + sd_width, mean_duration[3,] + sd_duration[3,]*sd_scale, col="lightgrey")

points(mean_duration[1,],type="p",pch=1,cex=2)
points(mean_duration[2,],type="p",pch=17,cex=2)
points(mean_duration[3,],type="p",pch=19,cex=2)
legend("bottomleft",c("500 steps","1500 steps","10000 steps"),pch=c(1,17,19),cex=1.25)

#######

mean_duration<-matrix(NA,3,10)
row.names(mean_duration)<-c("500","1500","10000")
mean_duration[1,]<- mean_a1/d1*100
mean_duration[2,]<- mean_a3/d3*100
mean_duration[3,]<- mean_a4/d4*100

sd_duration<-matrix(NA,3,10)
row.names(sd_duration)<-c("500","1500","10000")
sd_duration[1,]<- sd_a1/sqrt(d1)
sd_duration[2,]<- sd_a3/sqrt(d3)
sd_duration[3,]<- sd_a4/sqrt(d4)

dev.new()
par(mai=c(0.8,0.8,0.2,0.2))
plot(mean_duration[1,],type="l",ylim=c(90,96),
     xlab="",
     ylab="",	
     main="",
     cex.lab=1.75,
     cex.axis=1.25
)
points(mean_duration[2,],type="l")
points(mean_duration[3,],type="l")
title(xlab="Clutch size", line=2.5, cex.lab=1.75)
title(ylab="Excessive reserve of time", line=2.25, cex.lab=1.75)

variable<- c(1:10)
sd_scale<- 1
sd_width<- 0.05
segments(variable, mean_duration[1,] - sd_duration[1,]*sd_scale, variable, mean_duration[1,] + sd_duration[1,]*sd_scale, col="lightgrey")
segments(variable, mean_duration[2,] - sd_duration[2,]*sd_scale, variable, mean_duration[2,] + sd_duration[2,]*sd_scale, col="lightgrey")
segments(variable, mean_duration[3,] - sd_duration[3,]*sd_scale, variable, mean_duration[3,] + sd_duration[3,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[1,] - sd_duration[1,]*sd_scale, variable + sd_width, mean_duration[1,] - sd_duration[1,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[2,] - sd_duration[2,]*sd_scale, variable + sd_width, mean_duration[2,] - sd_duration[2,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[3,] - sd_duration[3,]*sd_scale, variable + sd_width, mean_duration[3,] - sd_duration[3,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[1,] + sd_duration[1,]*sd_scale, variable + sd_width, mean_duration[1,] + sd_duration[1,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[2,] + sd_duration[2,]*sd_scale, variable + sd_width, mean_duration[2,] + sd_duration[2,]*sd_scale, col="lightgrey")
segments(variable - sd_width, mean_duration[3,] + sd_duration[3,]*sd_scale, variable + sd_width, mean_duration[3,] + sd_duration[3,]*sd_scale, col="lightgrey")

points(mean_duration[1,],type="p",pch=1,cex=2)
points(mean_duration[2,],type="p",pch=17,cex=2)
points(mean_duration[3,],type="p",pch=19,cex=2)
legend("bottomright",c("500 steps","1500 steps","10000 steps"),pch=c(1,17,19),cex=1.25)

