

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

rm(list=ls())

# design
l1 <- 20      # level1   
l2 <- 2       # level2   
l3 <- 2       # level3   
repz <-2      # replicates
N <- l1*l2*l3*repz

# true SDs
beta  <- 100 # intercept
sd1   <-  20  # top
sd2   <-  2  # middle
sd3   <-  2  # bottom
error <-  2

A <- (l1)
B <- (l1*l2)
C <- (l1*l2*l3)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

level1. <- as.factor(rep (1:A, each=repz*l2*l3)) # ABOVE DAY
level2. <- as.factor(rep (1:B, each=repz*l3)) # DAY
level3. <- as.factor(rep (1:C, each=repz)) # RUN
Data    <- data.frame(level1.,level2.,level3.)

matrix1    <- model.matrix(~ 0 + level1., data=Data)
matrix2    <- model.matrix(~ 0 + level2., data=Data)
matrix3    <- model.matrix(~ 0 + level3., data=Data)

r1  <- matrix1 %*%  rnorm(A, 0, sd1)      
r2  <- matrix2 %*%  rnorm(B, 0, sd2)   
r3  <- matrix3 %*%  rnorm(C, 0, sd3)   

Data$Response  <-   beta + r1 + r2 + r3 + rnorm(N, 0, error)   

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

(f<-lme4::lmer(Response ~   (1|level1./level2./level3.), Data))
(f<- nlme::lme(Response ~ 1, random = ~1 |  level1./level2./level3.  , data=Data) )

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

surv.fake <- function (grand.mean, top.sd, day.sd,  run.sd, residual, top, d,r ,reps){
  
  N <- top*d*r*reps
  A <- top
  B <- top*d
  C <- top*d*r
  
  to <-  rep (1:A, each=d*r*reps)
  day <- rep (1:B, each=r*reps  ) 
  run <- rep (1:C, each=reps    ) 
  
  ### simulate
  mu.top <- rnorm(A, grand.mean, top.sd)
  mu.sim <- rnorm(B, 0         , day.sd)
  mu.run <- rnorm(C, 0         , run.sd)
  
  return ( data.frame( top=as.factor(to),
                       day=as.factor(day), 
                       run=as.factor(run),
                       Response= rnorm( N, mu.top[to] + mu.sim[day] + mu.run[run], residual)
  ))
}

dd <- surv.fake(grand.mean=beta, top.sd=sd1, day.sd=sd2, run.sd=sd3, residual=error, 
                top=l1, d=l2, r=l3, reps=repz)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

(f<-lme4::lmer(Response ~   (1|top/day/run), dd))
(f<- nlme::lme(Response ~ 1, random = ~1 |  top/day/run  , data=dd) )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# pull out some estimates to add to the plot


p4 <- function(x) {formatC(x, format="f", digits=4)}
p1 <- function(x) {formatC(x, format="f", digits=1)}

require(nlme)
modelint<-f

possibleError <-  
  tryCatch(intervals(lme(Response ~ 1, random = ~1 |  top/day/run  , data=dd)), 
           error=function(e) e)

###http://stackoverflow.com/questions/8093914
###/skip-to-next-value-of-loop-upon-error-in-r-trycatch

if(!inherits(possibleError, "error")){
  
  modelint<-possibleError
  
  emu      <-p1(modelint[['fixed']][2][[1]]    )  
  etop     <-p1(modelint[['reStruct']][['top']][2][[1]])
  eday     <-p1(modelint[['reStruct']][['day']][2][[1]])
  erun     <-p1(modelint[['reStruct']][['run']][2][[1]])
  esigma   <-p1(modelint[['sigma']][2][[1]])
  
}
#################################################################




d1<-Data
require(reshape)
d1$grp<-paste(level1. ,level2., level3., sep=".")
d1 <- rename(d1, c(Response="count"))
d1 <- rename(d1, c(grp="spray"))
d1 <- d1[order(d1$spray),]

limit<-dim(table(d1$spray))

attach(d1)
sprayTypes <- unique(spray)

y <- as.numeric(as.character(d1[,4])) 


plot(y, as.factor(d1[,4]) ,  ylim=c(min(y),max(y)) , xlim=c(1, limit) , xaxt="n",
     main=paste("Truth: intercept ",beta,"(",emu,"), top level sd=",sd1,"(",etop,")", ",\n middle level sd=",sd2 ,"(",eday,"), lowest level sd=",sd3, "(",erun,") & random error sd=", error,"(",esigma,")"),
     xlab=paste("Coloured by",l1,"top factor levels x",l2,"x",l3,"x",repz,"reps (LMM estimates in brackets)"), 
     ylab="Response", frame.plot=F , col="white")  


axis(1,  at=1:limit,labels=F  )


##make colours
chk<-as.character(d1$spray)
x<-as.data.frame(table(chk))
freq<-x[,2]
value<-max(dput(1:dim(x)[1]))
IR<-value+1
clr<-rainbow(IR)
clr<-rep(rainbow(l1*l2), each=l2)  #new
wR<-(2*IR-1:IR)/(2*IR)
##colour made
for (i in 1 : length(sprayTypes)){
  y <- count[spray == sprayTypes[i]]
  n <- sum(spray == sprayTypes[i])
  points(jitter(rep(i, n), amount = .1), y, pch = 16, cex = .5,
         
         col=clr[i]
  )
  
  lines(i + c(.12, .28), rep(mean(y), 2), lwd = 1, col="black")
  lines(rep(i + .2, 2),
        mean(y) + c(-1.96, 1.96) * sd(y) / sqrt(n),  lwd = 1, col="black"
        
  )
}

detach(d1)


