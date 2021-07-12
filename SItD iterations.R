##### Iterating Death, Start with alpha #####
n=200 #n is total number of days
N=66100000 - 1890000#N is population size, N = S+I+D , UK-NI

alpha=rep(1,n)
alpha[1]=1
beta=rep(0,n) #sum(beta)=1

ro=0.203

h=rep(0,n) #hazard function at infected age j

S=rep(0,n)
I=matrix(rep(0,n^2),nrow=n)
I[1,1]=860
S[1]=N-I[1,1]

II=rep(0,n)
  II[1]=860
D=rep(0,n)

lamda=matrix(rep(0,n^2),nrow=n)

mean=5.2 #mean= shape*scale
var=2.96 #var=shape

shape=338/37 
scale=37/65
rate=1/scale

#beta
dgamma(x=5,rate=rate,shape=shape)
pgamma(q=120,rate=rate,shape=shape)
beta[1]=pgamma(q=3/2,rate=rate,shape=shape)

#Hazard ratio
dispersion=0.0546
probability=18.69/(18.69+0.0546) #mean /(mean+dispersion)
inf_fatal_rate=0.00724
dnbinom(x=1,size=dispersion,prob=probability)

P=rep(0,n)
lamsum=rep(0,n)
lamsum2=rep(0,n)
p=rep(0,n)
d=rep(0,n)
dd=rep(0,n)
c=rep(0,n)
P[1]=dnbinom(x=1,mu=18.69,size=(1/0.0546))
h[1]=inf_fatal_rate*dnbinom(x=1,size=dispersion,prob=probability)/(1-inf_fatal_rate*P[1])
p[1]=dnbinom(x=1,mu=18.69,size=(1/0.0546))

dnbinom(size)
dnbinom(x=3,mu=18.69,size=(1/0.0546))
dnbinom(x=20,mu=18.69,size=0.0546)
tstar=31.57

#Find alpha, beta, h and P
for ( i in 2:n) {p[i]=dnbinom(x=i,mu=18.69,size=(1/0.0546))
P[i]=sum(p[1:i])
  h[i]=inf_fatal_rate*dnbinom(x=i,mu=18.69,size=(1/0.0546))/(1-inf_fatal_rate*P[i-1])
  beta[i]=pgamma(q=(i+0.5),rate=rate,shape=shape) - pgamma(q=(i-0.5),rate=rate,shape=shape)
  if (i<floor(tstar)){ alpha[i]= 1}
  else if(i >= ceiling(tstar)){ alpha[i]= 0.2814}
  else {alpha[i] = tstar -floor(tstar) + 0.2814*(ceiling(tstar)-tstar)}
  }

beta1=data.frame(beta)
beta1$Day=c(1:n)
names(beta1)=c("Probability","Day")

##### Plots from Figure 1 #########
barplot(beta,col = "light blue",ylim=c(0,0.25),xlim=c(1,15),xlab="Day,j")
barplot(p,col = "light blue",ylim=c(0,0.07),xlim=c(1,60),xlab="Day,j")

#Find S,I,D, c{appendix}
for (i in 1:n){

  for (j in 1:i) {
    lamda[i,j]=ro*alpha[i]*beta[j]/N
  if (i<n){
    I[i+1,j+1]=I[i,j]-h[j]*I[i,j]}
  }
  lamlam=0
  for (k in 1:i){lamlam=lamlam+lamda[i,k]*I[i,k]}
  lamsum2[i]=lamlam
  lamsum[i]=lamda[i,1:j]%*%I[i,1:j]
  d[i]=h[1:j]%*%I[i,1:j]
  if (i<n){
    I[i+1,1]=S[i]*lamsum[i]

    S[i+1]=S[i]-I[i+1,1]
    D[i+1]=D[i]+d[i]
    c[i]=sum(I[i+1,1:j])/sum(I[i,1:j])
    }
  
}
#### Number Deaths on each Day ############
DD=data.frame(D)
DD$Day=c(1:n)
DD$Date=as.Date()
DD$Death[1]=0
for (i in 2:n){DD$Death[i]=DD$D[i]-DD$D[(i-1)]} #gives number died on each day

DeathByDay=ggplot(DD)
DeathByDay=DeathByDay + geom_point(aes(x=Day,y=Death),color="magenta",size=1)
DeathByDay=DeathByDay+labs(title="Daily Number of Deaths")
DeathByDay=DeathByDay+ylab("Daily Deaths")
DeathByDay
