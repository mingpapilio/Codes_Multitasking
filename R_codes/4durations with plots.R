# Parental Investment model for multitasking and clutch size
# This code contains four repeated simulation, with potentially different values of season lengths, foraging efficiencies and other variables.
# Please check the notes after declaration of each variables for more details
# First day of building: Aug29, 2016, by Ming Liu
# Last modified: Apr16, 2018, by Ming Liu

# Duration of seasons
d1 <- 500
d2 <- 1000
d3 <- 1500
d4 <- 10000

# Foraging efficiencies
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
#nc = 0			#basic cost per attempt (nest building cost)
#bc = 0			#basic cost per unit of offspring
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
m1  = 0.7		#mean of distribution (good/bad environment)
v1  = 0			#variance of distribution
m2  = 0			#mean of environment, variable
v2  = 0  		#environmental variation during parental care
thr = 0			#environment output value, variable
ot  = 0			#temp for remaining offspring
m  = 0.97		#mean of survival rate
vv = 0.01		#variance coefficient (environmental fluctuation)
a <- m/vv		#a (parameter) for beta distribution
b <- (1-m)/vv	#b (parameter) for beta distribution

## Recording
off = matrix(0,rt,ll)  								#offspring number
	rownames(off) = c(1:rt)
	colnames(off) = xx
	
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
	ee[i+1] <- ee[i]
	}}

	if (r == 1 & i > t1){								#specify parental caring period
	e[i+1]  = e[i]- p*rc+ fs*(1-(p/10)^si)*s1			#breeding maintainance cost and foraging ####Important change 
	ee[i+1] = ee[i]
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

if (p1 == 0){										#recording
	off[k,j] = 0
	}else{
	off[k,j] = p1}
}}

data <- off
variable <- xx
colnames(data) <- variable
mean_1 <- apply(data, 2, mean) 	# average
sd_1 <- apply(data, 2, sd) 		# standard deviation
###

## Reset
n  <- d2 		#time limit
fs <- fs2
off = matrix(0,rt,ll)  								#offspring number
	rownames(off) = c(1:rt)
	colnames(off) = xx

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
	ee[i+1] <- ee[i]
	}}

	if (r == 1 & i > t1){								#specify parental caring period
	e[i+1]  = e[i]- p*rc+ fs*(1-(p/10)^si)*s1			#breeding maintainance cost and foraging ####Important change 
	ee[i+1] = ee[i]
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

if (p1 == 0){										#recording
	off[k,j] = 0
	}else{
	off[k,j] = p1}
}}

data <- off
variable <- xx
colnames(data) <- variable
mean_2 <- apply(data, 2, mean) 	# average
sd_2 <- apply(data, 2, sd) 		# standard deviation
###

## Reset
n  <- d3 		#time limit
fs <- fs3
off = matrix(0,rt,ll)  								#offspring number
	rownames(off) = c(1:rt)
	colnames(off) = xx

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
rp <- rp1+ p*rp2*rp1/10  		#reproductive period
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
	ee[i+1] <- ee[i]
	}}

	if (r == 1 & i > t1){								#specify parental caring period
	e[i+1]  = e[i]- p*rc+ fs*(1-(p/10)^si)*s1			#breeding maintainance cost and foraging ####Important change 
	ee[i+1] = ee[i]
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

if (p1 == 0){										#recording
	off[k,j] = 0
	}else{
	off[k,j] = p1}
}}

data <- off
variable <- xx
colnames(data) <- variable
mean_3 <- apply(data, 2, mean) 	# average
sd_3 <- apply(data, 2, sd) 		# standard deviation
###

## Reset
n  <- d4 		#time limit
fs <- fs4
off = matrix(0,rt,ll)  								#offspring number
	rownames(off) = c(1:rt)
	colnames(off) = xx

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
rp <- rp1+ p*rp2*rp1/10  		#reproductive period
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
	ee[i+1] <- ee[i]
	}}

	if (r == 1 & i > t1){								#specify parental caring period
	e[i+1]  = e[i]- p*rc+ fs*(1-(p/10)^si)*s1			#breeding maintainance cost and foraging ####Important change 
	ee[i+1] = ee[i]
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

if (p1 == 0){										#recording
	off[k,j] = 0
	}else{
	off[k,j] = p1}
}}

data <- off
variable <- xx
colnames(data) <- variable
mean_4 <- apply(data, 2, mean) 	# average
sd_4 <- apply(data, 2, sd) 		# standard deviation
###

##plot
mean_duration<-matrix(NA,4,10)
row.names(mean_duration)<-c("500","1000","1500","10000")
mean_duration[1,]<- mean_1/d1*100
mean_duration[2,]<- mean_2/d2*100
mean_duration[3,]<- mean_3/d3*100
mean_duration[4,]<- mean_4/d4*100

dev.new()
par(mai=c(1,1,0.5,0.5))
plot(mean_duration[1,],type="l",ylim=c(0,7),
     xlab="brood size",
     ylab="standardized mean offspring number",	
     main="",
     cex.lab=2,
     cex.axis=1)
points(mean_duration[2,],type="l",lty=2)
points(mean_duration[3,],type="l",lty=3)
points(mean_duration[4,],type="l",lty=5)
legend("topleft",c("fs= 1","fs= 2","fs= 4","fs= 8"),lty=c(1,2,3,5))


sd_duration<-matrix(NA,4,10)
row.names(sd_duration)<-c("500","1000","1500","10000")
sd_duration[1,]<- sd_1
sd_duration[2,]<- sd_2
sd_duration[3,]<- sd_3
sd_duration[4,]<- sd_4

dev.new()
par(mai=c(1,1,0.5,0.5))
plot(sd_duration[1,],type="l",ylim=c(2,10),
     xlab="brood size",
     ylab="standard deviation",	
     main="",
     cex.lab=2,
     cex.axis=1)
points(sd_duration[2,],type="l",lty=2)
points(sd_duration[3,],type="l",lty=3)
points(sd_duration[4,],type="l",lty=5)
legend("topleft",c("fs= 1","fs= 2","fs= 4","fs= 8"),lty=c(1,2,3,5))


library (beepr)
beep("ping")
beep("ping")
beep("ping")
