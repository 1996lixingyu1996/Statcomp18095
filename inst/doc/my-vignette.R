## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

## ------------------------------------------------------------------------
x<-rnorm(10)
y<-rnorm(10)
plot(x,y,xlab="10 random values",ylab="10 other values",xlim=c(-2,2),ylim=c(-2,2),col="red")

## ------------------------------------------------------------------------
square <- function(x)  x^2
square(1:5)

is.min <- function(x)      x == min(x) 

a = runif(10)
is.min(a) 
a[is.min(a)]                                            
which(is.min(a))                                  

## ------------------------------------------------------------------------
x <- c("m", "f", "u", "f", "f", "m", "m")
lookup <- c(m = "Male", f = "Female", u = NA)
lookup[x]

## ------------------------------------------------------------------------
U=runif(1000)#随机数
X = 0*I(U<=0.1) + 1*I( U>0.1 & U<=0.3) + 2*I( U>0.3 & U<=0.5)+3*I( U>0.5 & U<=0.7)+4*I( U>0.7 & U<=1)#逆函数
hist(X, breaks=-1:4,ylim=c(0,0.4) ,main = "c(0.1,0.2,0.2,0.2,0.3)",freq=FALSE)#图 
fy<-function(x)  0.1*I(x<=0&x>-1)+0.2*I(x>0&x<=3)+0.3*I(x<=4&x>3) #理论函数
curve(fy,from=-1,to=4,add=T,col="red")#理论图
table(X)#频率表

## ------------------------------------------------------------------------
f1<-function(x)
{
t<-x^2*(1-x)/beta(3,2)
t
}#cdf of beta(3,2)
rej1<-function(n)
{
   xvec<-runif(n,0,1);
   u<-runif(n,0,1);
   zvec<-xvec[u<(f1(xvec)/2)]
   zvec
}#acceptance-rejection method
NROW(rej1(5000))#efficient
samp1<-rej1(5000)[1:1000]#getting 1000 random number
hist(samp1,freq=FALSE,breaks=seq(-0.5,1.5,0.02),ylim=c(0,2.5),border="yellow")
curve(f1,from=0,to=1,add=T,col="red",lwd=2) #plot

## ------------------------------------------------------------------------
a<-rgamma(1000,4,2)
b=rep(0,1000)
for(i in 1:1000)
{
  b[i]<-rexp(1,a[i])
}
hist(b,freq=FALSE,breaks=seq(min(b)-1,max(b)+1,0.05),ylim=c(0,5),border="green")


## ------------------------------------------------------------------------
n=500
m=1000
v<-2*(1:n)-n-1

t<-numeric(m)
for(i in 1:m)
{
 x<-sort(rlnorm(n))
 meanx<-mean(x)
 t[i]<-(1/(n^2*exp(0.5)))*as.numeric(v%*%x)
}

## ------------------------------------------------------------------------
mean(t)

## ------------------------------------------------------------------------
median(t)

## ------------------------------------------------------------------------
quantile(t, probs = seq(0.1,0.9,0.1))

## ------------------------------------------------------------------------
hist(t,prob=TRUE,ylim = c(0,10))
lines(density(t),col="green",lwd=2)

## ------------------------------------------------------------------------
for(i in 1:m)
{
 x<-sort(runif(n))
 meanx<-mean(x)
 t[i]<-(1/(n^2*exp(meanx)))*as.numeric(v%*%x)
}

## ------------------------------------------------------------------------
mean(t)
median(t)
quantile(t, probs = seq(0.1,0.9,0.1))

## ------------------------------------------------------------------------
hist(t,prob=TRUE,ylim = c(0,10))
lines(density(t),col="green",lwd=2)

## ------------------------------------------------------------------------
set.seed(1234)
n<-1000              #generate 1000 random numbers
m<-100               #generate 100 gini ratio every times             
k=100                #repeat 100 times
Glnorm<-1:m
gamma.hat<-sdgamma.hat<-cpgamma<-1:k
v<-2*(1:n)-n-1

for(j in 1:k)
{
for(i in 1:m)
{
  a=runif(1)#random mean
  b=runif(1)#random sd

  x<-sort(rlnorm(n,a,b))
  meanx<-mean(x)
  Glnorm[i]<-(1/(n^2*exp(meanx)))*as.numeric(v%*%x)
}
  gamma.hat[j]<-mean(Glnorm)
  sdgamma.hat[j]<-sd(Glnorm)
  cpgamma[j]<-(1/m)*sum(I(gamma.hat[j]-qt(0.975,m-2)*sdgamma.hat[j]<Glnorm & Glnorm<gamma.hat[j]+qt(0.975,m-2)*sdgamma.hat[j]))
}

## ------------------------------------------------------------------------
mean(cpgamma)

## ------------------------------------------------------------------------
#(1)
alpha=0.1
m<-2500
n=100
test1<-test2<-test3<-numeric(m)
#estimate
for(j in 1:m)
{
u1=runif(1)
sd1=runif(1,0,5)
u2=runif(2)
sd2=runif(1,0,2)
x<-rnorm(n,u1,sd1)
y<-rnorm(n,u2,sd2)
test1[j]<-as.integer(cor.test(x,y,method="pearson")$p.value<=alpha)#pearson
test2[j]<-as.integer(cor.test(x,y,method="spearman")$p.value<=alpha)#spearman
test3[j]<-as.integer(cor.test(x,y,method="kendall")$p.value<=alpha)#kendall
}

## ------------------------------------------------------------------------
mean(test1)
mean(test2)
mean(test3)

## ------------------------------------------------------------------------
test1<-test2<-test3<-numeric(m)
m=1000
n=100
for(j in 1:m)
{
  x<-y<-numeric(n)
  for(i in 1:n)
  {
    x[i]<-runif(1,-1,1)
    t=x[i]
    y[i]<-runif(1,-sqrt(1-t^2),sqrt(1-t^2))
  }
  test1[j]<-as.integer(cor.test(x,y,method="pearson")$p.value<=alpha)#pearson
  test2[j]<-as.integer(cor.test(x,y,method="spearman")$p.value<=alpha)#spearman
  test3[j]<-as.integer(cor.test(x,y,method="kendall")$p.value<=alpha)#kendall
}

## ------------------------------------------------------------------------
mean(test1)
mean(test2)
mean(test3)

## ------------------------------------------------------------------------
library(bootstrap)
LSAT=law$LSAT
GPA=law$GPA
n = length( LSAT ) #numbeer of data
cor_jack = numeric( n )
cor_hat = cor( LSAT,GPA ) #corelation of LSAT  and GPA

## ------------------------------------------------------------------------
for (i in 1:n) { cor_jack[i] = cor( LSAT[-i],GPA[-i] ) }
bias_jack = (n-1)*( mean(cor_jack) - cor_hat )
cor_bar = mean(cor_jack) # mean of statistic generated from jackknife
se_jack = sqrt( (n-1)*mean( (cor_jack-cor_bar)^2 ) ) 

print(sprintf('estimate of the bias  of the correlation statistic is: %f',bias_jack ))

print(sprintf('estimate of the standard error of the correlation statistic is: %f',se_jack ))


## ------------------------------------------------------------------------
library(boot)
attach(aircondit)
x = hours
B = 5000 
set.seed(1)

## ------------------------------------------------------------------------
gaptime_hat = mean(x) #MLE of 1/lambda
#bootstrap estimate of 1/lambda
frac_1_lambda_boot = boot(data=aircondit,statistic=function(x,i){mean(x[i,])}, R=B )

frac_1_lambda_boot

## ------------------------------------------------------------------------
einf_jack = empinf(frac_1_lambda_boot, type='jack')
boot.ci( frac_1_lambda_boot, type=c('norm','basic','perc','bca'), L=einf_jack ) 

## ------------------------------------------------------------------------
hist(frac_1_lambda_boot$t, main='', xlab=expression(1/lambda), prob=T)
points(frac_1_lambda_boot$t0, 0, pch = 19)

## ------------------------------------------------------------------------
library(bootstrap)
attach( scor )
n = length( scor[,1] )
x = as.matrix(scor)
theta_jack = numeric( n )

## ------------------------------------------------------------------------
lambda_hat = eigen(cov(scor))$values
theta_hat = lambda_hat[1]/sum(lambda_hat) #compute the mean of theta from sample
#according to the formala, we write a function to calculate the theta. 
theta = function(x){
 eigen(cov(x))$values[1]/sum(eigen(cov(x))$values)
}  

for (i in 1:n) { theta_jack[i] = theta( x[-i,] ) }

theta_bar = mean(theta_jack)
bias_jack = (n-1)*( mean(theta_jack) - theta_hat )
se_jack = sqrt( (n-1)*mean( (theta_jack-theta_bar)^2 ) )

print(sprintf('the jackknife estimates of bias of hat of theta is : %f', bias_jack))
print(sprintf('the jackknife estimates of standard error of hat of theta is : %f', se_jack))
detach(scor)

## ------------------------------------------------------------------------
library(DAAG)
attach(ironslag)
a <- seq(10, 40, .1) #sequence for plotting fits

L1 <- lm(magnetic ~ chemical)
plot(chemical, magnetic, main="Linear", pch=16)
yhat1 <- L1$coef[1] + L1$coef[2] * a
lines(a, yhat1, lwd=2)


L2 <- lm(magnetic ~ chemical + I(chemical^2))
plot(chemical, magnetic, main="Quadratic", pch=16)
yhat2 <- L2$coef[1] + L2$coef[2] * a + L2$coef[3] * a^2
lines(a, yhat2, lwd=2)


L3 <- lm(log(magnetic) ~ chemical)
plot(chemical, magnetic, main="Exponential", pch=16)
logyhat3 <- L3$coef[1] + L3$coef[2] * a
yhat3 <- exp(logyhat3)
lines(a, yhat3, lwd=2)


L4 <- lm(log(magnetic) ~ log(chemical))
plot(log(chemical), log(magnetic), main="Log-Log", pch=16)
logyhat4 <- L4$coef[1] + L4$coef[2] * log(a)
lines(log(a), logyhat4, lwd=2)

## ------------------------------------------------------------------------
n <- length(magnetic) 
e1 <- e2 <- e3 <- e4 <- numeric(n)
k=1
# fit models on leave-two-out samples

while (k<n) {
    y <- magnetic[c(-k,-(k+1))]
    x <- chemical[c(-k,-(k+1))]

    J1 <- lm(y ~ x)
    yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
    e1[k] <- magnetic[k] - yhat1
    
    J2 <- lm(y ~ x + I(x^2))
    yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +
    J2$coef[3] * chemical[k]^2
    e2[k] <- magnetic[k] - yhat2
    
    J3 <- lm(log(y) ~ x)
    logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
    yhat3 <- exp(logyhat3)
    e3[k] <- magnetic[k] - yhat3
    
    J4 <- lm(log(y) ~ log(x))
    logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[k])
    yhat4 <- exp(logyhat4)
    e4[k] <- magnetic[k] - yhat4
    
    k=k+2
}

## ------------------------------------------------------------------------
c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))

## ------------------------------------------------------------------------
L2

## ------------------------------------------------------------------------
par(mfrow = c(1, 2)) #layout for graphs
plot(L2$fit, L2$res) #residuals vs fitted values
abline(0, 0) #reference line
qqnorm(L2$res) #normal probability plot
qqline(L2$res) #reference line
par(mfrow = c(1, 1)) #restore display

## ------------------------------------------------------------------------
set.seed(1)
x <- c(158, 171 ,193 ,199 ,230 ,243 ,248 ,248 ,250 ,267 ,271 ,316 ,327 ,329)
y <- c(141 ,148 ,169 ,181 ,203 ,213 ,229 ,244 ,257 ,260 ,271 ,309)
#the function cvm.test is used to calculate the Cramer-von Mises statistic
cvm.test <- function(x,y){
  #the empirical distribution function of the x,y
  F <- ecdf(x)
  G <- ecdf(y)
  n <- length(x)
  m <- length(y)
  s <- numeric(n)
  t <- numeric(m)
  for (i  in 1:n) {
    s[i] <- (F(x[i])-G(x[i]))^2
  }
  s <- sum(s)
  for (j  in 1:m) {
    t[j] <- (F(y[j])-G(y[j]))^2
  }
  t <- sum(t)
  #return the Cramer-von Mises statistic
  return (m*n*(s+t)/(m+n)^2)
}
#number of replicates
R <- 999 
#pooled sample
z <- c(x, y)
K <- 1:26
#storage for replicates
reps <- numeric(R) 


## ------------------------------------------------------------------------
t0 <- cvm.test(x, y)
t0
for (i in 1:R) {
  #generate indices k for the first sample
  k <- sample(K, size = 14, replace = FALSE)
  x1 <- z[k]
  y1 <- z[-k] #complement of x1
  reps[i] <- cvm.test(x1, y1)
}
p <- mean(c(t0, reps) >= t0)
p
hist(reps, main = "", freq = FALSE, xlab = "T (p = 0.421)",
breaks = "scott")
points(t0, 0, cex = 1, pch = 16) 

## ------------------------------------------------------------------------

library(RANN)
library(boot)
library(energy)
library(Ball)
m <- 100; k<-3; p<-2; mu <- 0.2; set.seed(12345)
n1 <- n2 <- 20; R<-50; n <- n1+n2; N = c(n1,n2)
Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1) 
  block1 <- NN$nn.idx[1:n1,-1]
  block2 <- NN$nn.idx[(n1+1):n,-1]
  i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5)
  (i1 + i2) / (k * n)
}
#the nn method and return the p.values
eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R,
                   sim = "permutation", sizes = sizes,k=k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  list(statistic=ts[1],p.value=p.value)
}

p.values1 <- matrix(NA,m,3)
p.values2 <- matrix(NA,m,3)
p.values3 <- matrix(NA,m,3)
p.values4 <- matrix(NA,m,3)




## ------------------------------------------------------------------------
#under the situation of unequal variances and equal expectations
for(i in 1:m){
  #the sample x is from the standard normal distribution and the sample y is from the normal diisreibution with mean=0,variance is 2
  x <- matrix(rnorm(n1*p),ncol=p);
  y <- cbind(rnorm(n2,sd=2),rnorm(n2,sd=2));
  z <- rbind(x,y)
  p.values1[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values1[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values1[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.value
}
alpha <- 0.1;
pow1 <- colMeans(p.values1<alpha)
names(pow1) <- c('NN','energy', 'ball ')
#get the powers by the three method
pow1

#under the situation of unequal variances and unequal expectations
for(i in 1:m){
    #the sample x is from the standard normal distribution and the sample y is from the normal diisreibution with mean=0.2 ,variance is 2
  x <- matrix(rnorm(n1*p),ncol=p);
  y <- cbind(rnorm(n2,mean = mu,sd=2),rnorm(n2,mean=mu,sd=2));
  z <- rbind(x,y)
  p.values2[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values2[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values2[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.value
}
alpha <- 0.1;
pow2 <- colMeans(p.values2<alpha)
names(pow2) <- c('NN','energy', 'ball ')
#get the powers by the three method
pow2

#under the situation of non-normal distributions
for(i in 1:m){
      #the sample x is from t distribution with df=1 and the sample y is from the mixture of two normal distributions
  x <- matrix(rt(n1*p,df=1),ncol = p);
  y <- cbind(rnorm(n2),rnorm(n2,mean=mu,sd=4));
  z <- rbind(x,y)
  p.values3[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values3[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values3[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.value
}
alpha <- 0.1;
pow3 <- colMeans(p.values3<alpha)
names(pow3) <- c('NN','energy', 'ball ')
#get the powers by the three method
pow3

#under the situation of Unbalanced samples which the number of x is 200 and y is 20
n1 <- 200
n2 <- 20
n <- n1+n2
N = c(n1,n2)
for(i in 1:m){
  x <- matrix(rnorm(n1*p),ncol=p);
  y <- cbind(rnorm(n2),rnorm(n2));  
  z <- rbind(x,y)
  p.values4[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values4[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values4[i,3] <- bd.test(x=x,y=y,R=99,seed=i*12345)$p.value
}
alpha <- 0.18;
pow4 <- colMeans(p.values4<alpha)
names(pow4) <- c('NN','energy', 'ball ')
#get the powers by the three method
pow4

## ------------------------------------------------------------------------
set.seed(1)
#build a standard Cauchy distribution
f <- function(x, x0=0, gamma=1){
  out<-1/(pi*gamma*(1+((x-x0)/gamma)^2))
  return(out)
  
}
#the times of simulation
m <- 80000
x <- numeric(m)
xt <- x[i-1]
#generat the normal proposal distribution with mean=xt ,sd=1
y <- rnorm(1, mean = xt,sd=4)
x[1] <- rnorm(1,mean=xt,sd=4 )
k <- 0
u <- runif(m)


## ------------------------------------------------------------------------
for (i in 2:m) {
  xt <- x[i-1]
  y <- rnorm(1, mean = xt,sd=1)
  num <- f(y) * dnorm(xt, mean = y,sd=4)
  den <- f(xt) * dnorm(y, mean = xt,sd=4)
  if (u[i] <= num/den) x[i] <- y else {
    x[i] <- xt
    k <- k+1 #y is rejected
  }
}
#discard the burnin sample
b <- 1001 
y <- x[b:m]
a <- ppoints(200)
#quantiles of cauchy distribution
Qcauchy <- qcauchy(a)
#quantiles of sample distribution
Q <- quantile(x, a)
qqplot(Qcauchy, Q,xlim=c(0,2),ylim=c(0,2),xlab="Cauchy Quantiles", ylab="Sample Quantiles",main = expression("Q-Q plot for Cauchy distribution"))
hist(y, breaks=50, main="", xlab="", freq=FALSE)
lines(Qcauchy, f(Qcauchy))


## ------------------------------------------------------------------------
size <- c(125,18,20,34)
#The following function prob computes the target density
prob <- function(y, size) {
  if (y < 0 || y >1)
    return (0)
  return((1/2+y/4)^size[1] *
           ((1-y)/4)^size[2] * ((1-y)/4)^size[3] *(y/4)^size[4])
}
#length of the chain
m <- 5000
#width of the uniform support set
w <- .25 
#burn-in time
burn <- 1000
#for accept/reject step
u <- runif(m) 
#proposal distribution
v <- runif(m, -w, w)



## ------------------------------------------------------------------------
x[1] <- .25
for (i in 2:m) {
  y <- x[i-1] + v[i]
  if (u[i] <= prob(y, size) / prob(x[i-1], size))
    x[i] <- y else
      x[i] <- x[i-1]
}
xb <- x[(burn+1):m]
print(mean(xb))

## ------------------------------------------------------------------------
lden=function(theta)125*log(2+theta)+38*log(1-theta)+34*log(theta)
MCMC=function(x,burn_in=0,N=burn_in+10000,print_acc=F){
  y=1:N#for MCMC generation
  ld=lden#use log-likelihood, things could be a little bit different.
  acc=0
  for(i in 1:N){
    p=runif(1)
    y[i]=x=if(runif(1)<exp(ld(p)-ld(x))){acc=acc+1;p}else x
  }
  if(print_acc)print(acc/N)
  y[(burn_in+1):N]
}
plot(MCMC(0.5,print_acc=T))#accept rate~0.15

## ------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
  # psi[i,j] is the statistic psi(X[i,1:j])
  # for chain in i-th row of X
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi) #row means
  B <- n * var(psi.means) #between variance est.
  psi.w <- apply(psi, 1, "var") #within variances
  W <- mean(psi.w) #within est.
  v.hat <- W*(n-1)/n + (B/(n*k)) #upper variance est.
  r.hat <- v.hat / W #G-R statistic
  return(r.hat)
}

## ------------------------------------------------------------------------
M1=cumsum((MC1=MCMC(0.2)))/1:10000#mean_cumsum
M2=cumsum((MC2=MCMC(0.4)))/1:10000#mean_cumsum
M3=cumsum((MC3=MCMC(0.6)))/1:10000#mean_cumsum
M4=cumsum((MC4=MCMC(0.8)))/1:10000#mean_cumsum
psi=rbind(M1,M2,M3,M4)
plot((R=sapply(1:10000,function(i)Gelman.Rubin(psi[,1:i]))),main="R value of Gelman-Rubin method",ylim=c(1,2))

## ------------------------------------------------------------------------
res=which.max(R<1.2)
c(mean(c(MC1[res:10000],MC2[res:10000],MC3[res:10000],MC4[res:10000])),var(c(MC1[res:10000],MC2[res:10000],MC3[res:10000],MC4[res:10000])))

## ------------------------------------------------------------------------

euclidean_norm<-function(x)
{
	length_x=length(x)
      y=0
	for(i in 1:length_x)
		{
			y=y+(x[i])^2
		}
	y=sqrt(y)
}
factorial<-function(n)
{
	if(n==0) {ans=1}
	if(n!=0) {ans=prod(1:n)}
	ans
}

## ------------------------------------------------------------------------
f1<-function(k,x,d)
{
	m=euclidean_norm(x)
	a=((-1)^k)*(m)^(2*k+2)*gamma((d+1)/2)*gamma(k+1.5)
	b=factorial(k)*2^k*(2*k+1)*(2*k+2)*gamma(k+0.5*d+1)
	ans=a/b
}

## ------------------------------------------------------------------------
a=c(1,2)
a=t(a)
k=0
t2=f1(0,a,1)
t1=0
s=t2
while(abs(t2-t1)>0.01)
{
	k=k+1
	t1=t2
	t2=f1(k,a,1)
	s=s+t2	
}

## ------------------------------------------------------------------------
s

## ------------------------------------------------------------------------
f<-function(y,theta,nita)
{
	1/(theta*pi*(1+((y-nita)/theta)^2))
}

cauchy_cdf<-function(theta1,nita1,x)
{
	integrate(f,lower=-Inf,upper=x,rel.tol=.Machine$double.eps^0.25,theta=theta1,nita=nita1)$value
}


## ------------------------------------------------------------------------
nita1=0;
theta1=1;
x=20;
cauchy_cdf(theta1,nita1,x)
pcauchy(20)

## ------------------------------------------------------------------------
ABOexpect=function(nA,nB,p,q,r) {
    nAA=nA * p / (p + 2*r)
    nAO=nA - nAA
    nBB=nB * q / (q + 2*r)
    nBO=nB - nBB
    N=list(AA=nAA,AO=nAO,BB=nBB,BO=nBO)
  }
  
  ABOmaximize=function(nAA,nAO,nBB,nBO,nAB,nO) {
    p=(2 * nAA + nAO + nAB) / (2 * (nA + nB + nO + nAB))
    q=(2 * nBB + nBO + nAB) / (2 * (nA + nB + nO + nAB))
    r= 1.0 - p - q
    L=list(p=p,q=q,r=r)
  }
  
  #initial guess
  p=0.3
  q=0.2
  r=0.5
  ## Observed data (counts of people with the four blood groups)
  nA =28
  nB=24
  nAB=41
  nO=40
  
  ## Set up iteration
  iter=1
  diff=1
  tol=0.0001 ## tolerance
  
  while (diff > tol) {
    E=ABOexpect(nA,nB,p,q,r)
    M=ABOmaximize(E$AA,E$AO,E$BB,E$BO,nAB,nO)
    diff=abs(M$p - p) + abs(M$q - q) + abs(M$r -r)
    p=M$p
    q=M$q
    r=M$r
    cat(sprintf("iter:%d, diff:%.2f\n",iter,diff))
    iter=iter + 1
  }
  
  cat(sprintf("p=%.4f,q=%.4f, r=%.4f\n",p,q,r))

## ------------------------------------------------------------------------
set.seed(1)
x<- c(3,8,4,9)
y<- c(6,9,3,5)
b<- table(x,y)
summary(b)

## ---- echo=FALSE---------------------------------------------------------
set.seed(1)
library(bootstrap)    #for the law data
a<-matrix(c(round(law$LSAT,digits = 0),law$GPA),nrow=2,byrow = TRUE )
dimnames(a)<-list(c("LSAT","GPA"),1:15)
knitr::kable(a)

## ------------------------------------------------------------------------
library(bootstrap)
x <- law$LSAT; y<-law$GPA
cor <- cor(x,y)
n <- length(x)
cor_jack <- numeric(n)  #storage of the resamples

for (i in 1:n)
  cor_jack[i] <- cor(x[-i],y[-i]) 

bias.jack <- (n-1)*(mean(cor_jack)-cor)

se.jack <- sqrt((n-1)/n*sum((cor_jack-mean(cor_jack)))^2)
print(list(cor= cor ,est=cor-bias.jack, bias = bias.jack,se = se.jack, cv = bias.jack/se.jack))

## ------------------------------------------------------------------------
#Bootstrap
library(boot)
data(aircondit,package = "boot")
air <- aircondit$hours
theta.hat <- mean(air)
#set up the bootstrap
B <- 2000            #number of replicates
n <- length(air)      #sample size
theta.b <- numeric(B)     #storage for replicates

#bootstrap estimate of standard error of R
for (b in 1:B) {
  #randomly select the indices
  i <- sample(1:n, size = n, replace = TRUE)
  dat <- air[i]       #i is a vector of indices
  theta.b[b] <- mean(dat)
}

bias.theta <- mean(theta.b - theta.hat)
se <- sd(theta.b)

print(list(bias.b = bias.theta,se.b = se))

theta.boot <- function(dat,ind) {
  #function to compute the statistic
  mean(dat[ind])
}
boot.obj <- boot(air, statistic = theta.boot, R = 2000)
print(boot.obj)

## ------------------------------------------------------------------------
print(boot.ci(boot.obj, type=c("basic","norm","perc","bca")))

## ------------------------------------------------------------------------
#Jackknife
#compute the jackknife replicates, leave-one-out estimates
library(bootstrap)
data(scor,package = "bootstrap")
theta.jack <- numeric(n)
dat <- cbind(scor$mec, scor$vec, scor$alg, scor$ana, scor$sta)
for (i in 1:n){
  sigma.jack <- cov(dat[-i,])
  theta.jack[i] <- eigen(sigma.jack)$values[1]/sum(eigen(sigma.jack)$values)
}

#jackknife estimate of bias
bias.jack <- (n - 1) * (mean(theta.jack) - theta.hat) 

#Jackknife estimate of standard error
se.j <- sqrt((n-1) * mean((theta.jack - mean(theta.jack))^2))

print(list(bias.jack = bias.jack,se.jack = se.j))

## ------------------------------------------------------------------------
    ptm <- proc.time()

    library(DAAG); attach(ironslag)
    n <- length(magnetic)   #in DAAG ironslag
    e1 <- e2 <- e3 <- e4 <- matrix(0,n-1,n) # error matrix
    #yhat1 <- yhat2 <- yhat3 <- yhat4 <- matrix(0,n-1,n)
    #logyhat3 <- logyhat4 <- matrix(0,n-1,n)
    
    
    # for n-fold cross validation
    # fit models on leave-two-out samples
    for (i in 1:n-1) {
      for (j in 1:n){
        if (j != i){
          y <- magnetic[c(-i,-j)]
          x <- chemical[c(-i,-j)]
          
          J1 <- lm(y ~ x) 
          yhat1 <- J1$coef[1] + J1$coef[2] * chemical[i]
          e1[i,j] <- magnetic[i] - yhat1
          
          J2 <- lm(y ~ x + I(x^2))
          yhat2 <- J2$coef[1] + J2$coef[2] * chemical[i] +
            J2$coef[3] * chemical[i]^2
          e2[i,j] <- magnetic[i] - yhat2
          
          J3 <- lm(log(y) ~ x)
          logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[i]
          yhat3 <- exp(logyhat3)
          e3[i,j] <- magnetic[i] - yhat3
          
          J4 <- lm(log(y) ~ log(x))
          logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[i])
          yhat4 <- exp(logyhat4)
          e4[i,j] <- magnetic[i]- yhat4
        }
      }
    }
    
ltocv <- c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))
ltocv.ptm <- proc.time() - ptm # same function with system.time(exp)
print(list("timeconsuming_of_ltocv"=ltocv.ptm[1:3]))

## ---- echo=FALSE---------------------------------------------------------
### Example 7.18 (Model selection: Cross validation)

    # Example 7.17, cont.
    ptm <- proc.time()
    n <- length(magnetic)   #in DAAG ironslag
    e1 <- e2 <- e3 <- e4 <- numeric(n)

    # for n-fold cross validation
    # fit models on leave-one-out samples
    for (k in 1:n) {
        y <- magnetic[-k]
        x <- chemical[-k]

        J1 <- lm(y ~ x)
        yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
        e1[k] <- magnetic[k] - yhat1

        J2 <- lm(y ~ x + I(x^2))
        yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +
                J2$coef[3] * chemical[k]^2
        e2[k] <- magnetic[k] - yhat2

        J3 <- lm(log(y) ~ x)
        logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
        yhat3 <- exp(logyhat3)
        e3[k] <- magnetic[k] - yhat3

        J4 <- lm(log(y) ~ log(x))
        logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[k])
        yhat4 <- exp(logyhat4)
        e4[k] <- magnetic[k] - yhat4
    }


    loocv <- c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))
    loocv.ptm <- proc.time() - ptm
    print(list("timeconsuming_of_loocv"=loocv.ptm[1:3]))

## ------------------------------------------------------------------------
a <- data.frame(rbind(loocv,ltocv))
row.names(a) <- c("loocv","ltocv")
names(a) <- c("L1","L2","L3","L4")
knitr::kable(a)

