The mouse data is survival time in days after surgery.
"bootstrap"<- function(x,nboot,theta,...,func=NULL){
  call <- match.call()
  n <- length(x)
  bootsam<- matrix(sample(x,size=n*nboot,replace=TRUE),nrow=nboot)
  thetastar <- apply(bootsam,1,theta,...)
  func.thetastar <- NULL; jack.boot.val <- NULL; jack.boot.se <- NULL;
  if(!is.null(func)){
    match1 <- function(bootx,x){duplicated(c(bootx,x))[( length(x)+1) : (2*length(x))]}
    matchs <- t(apply(bootsam,1,match1,x))
    func.thetastar <- func(thetastar)
    jack.boot <- function(inout,thetastar,func){ func(thetastar[!inout])}
    jack.boot.val <- apply(matchs,2,jack.boot,thetastar,func)
    
    if(sum(is.na(jack.boot.val)>0)) {cat("At least one jackknife influence value for func(theta) is   undefined", fill=T)
      cat(" Increase nboot and try again",fill=T)
      return()}
    
    if( sum(is.na(jack.boot.val))==0) 
    {jack.boot.se <- sqrt( ((n-1)/n)*sum( (jack.boot.val-mean(jack.boot.val))^2 )  )
    
    
    
    }}
  
  return(list(thetastar=thetastar,func.thetastar=func.thetastar,jack.boot.val=jack.boot.val, jack.boot.se=jack.boot.se, call=call))
  
  
}

mouse.c <- scan("http://www.rohan.sdsu.edu/~babailey/stat672/mouse.c.data") 
mouse.t <- scan("http://www.rohan.sdsu.edu/~babailey/stat672/mouse.t.data")

#Bootstrap estimates for standard error for the mean of the treatment group:
seeds()
help(bootstrap)
results1 <- bootstrap(mouse.t,50,mean)

What is in the results1 object?

Make a histogram of the bootstrap replicates.

hist(results1$thetastar) 
thetahat <- mean(mouse.t) 
abline(v=thetahat, lty=2)

Now, the standard error of the 50 bootstrap replicates is

sd(results1$thetastar)

#Try this again with 1000 bootstrap samples, you can do this in one step

 results2 <- bootstrap(mouse.t,1000,mean,func=sd)

#The value should be close to 23.36.

#Try the above with the median and 1000 boostrap samples.

#The value should be close to 37.83.

#Confidence Intervals based on Bootstrap Percentiles

#Recall, thetahat=86.85, the mean of the 7 treated mice. The bootstrap standard error of thetahat is 23.13, so if we choose alpha=.05, then the standard 90% normal confidence intervals for the true mean theta is [86.85 - 1.645*23.13, 86.85 + 1.645*23.13].

#We could use the quantile function and construct percentiles of thetastar based on the 1000 bootstrap replications.

conf.level <- 0.95 
probs <- (1 + c(-1, 1) * conf.level) / 2 
quantile(results1$thetastar, probs = probs)

#What are the 5% and 95% percentiles of this histogram? What would be the percentile interval? (You could change conf.level to 0.9)

Example 2: More Complex Data Structures

From the help file:
  # To bootstrap functions of more complex data structures,
  # write theta so that its argument x
  # is the set of observation numbers
  #  and simply  pass as data to bootstrap the vector 1,2,..n.
  # For example, to bootstrap
  # the correlation coefficient from a set of 15 data pairs:
xdata <- matrix(rnorm(30),ncol=2)
cor(xdata[,1],xdata[,2])
n <- 15
x<-1:15;theta(x,xdata)
x<- sample(x,replace=T)
theta <- function(x,xdata){ cor(xdata[x,1],xdata[x,2],method = "kendall") }
results <- bootstrap(1:n,20,theta,xdata)

#Part II

#Back to Kendall's tau

How can we bootrap CI for tau? Can you write a function to return the estimated tau values from data?

OK, try it. I wrote one and it is linked off the course calendar.

Example 2 Hormone Data: Linear Regression Bootstrapping Pairs

The hormone dataset is (Amount in milligrams of anti-inflammatory hormone remaining in 27 devices after a certain number of hours of wear. Sampled from 3 different lots A, B, and C.)

Let's get the hormone dataset and use help to see what's in it.

> hormone <- read.table("http://www.rohan.sdsu.edu/~babailey/stat672/hormone.data", header=T)

Plot the data.

> plot(hormone$hrs,hormone$amount)

Fit a linear regression model:

> fit <- lm(amount~hrs,hormone) 
> summary(fit)

Write a function (for the argument theta) to be bootstrapped. It should fit a linear regression model to the bootstrap sample and return the estimated coefficients.


> ls.pairs <- function(x, hormone)
{
  lm(amount ~ hrs, hormone[x,  ])$coefficients
}
Now we can generate bootstrap replicates calling the boostrap function.

> results <- bootstrap(1:27,100,theta=ls.pairs,hormone)

Calculate the std. error of the estimated coefficients.

> sd(results$thetastar[1,]) 
> sd(results$thetastar[2,])

Example 3 Hormone Data: Linear Regression Bootstrapping Residuals

Write a function (for the argument theta) to be bootstrapped. It should fit a linear regression model to the bootstrap sample and return the estimated coefficients.

> ls.resid <- function(x, hormone, fit)
{
  lm(newY ~ hrs, data.frame(newY = fit$fitted.values + fit$residuals[x],
  hormone))$coefficients
}
or, there is a lsfit function:
> ls.resid2 <- function(x, hormone, fit)
{
  lsfit(hormone$hrs, fit$fitted.values + fit$residuals[x])$coef
}
Now we can generate bootstrap replicates calling the boostrap function

> results3 <- bootstrap(1:27,1000,theta=ls.resid,hormone,fit)

Calculate the std. error of the estimated coefficients.

> sd(results3$thetastar[1,]) 
> sd(results3$thetastar[2,])

Now, if we were really clever, we would write a function(s) to calculate the standard errors and pass it to the boostrap function! (for both examples above!)

Example 4 Mouse data: Jackknife-after-Bootstrap

The mouse data is survival time in days after surgery.

Bootstrap estimates for standard error for the mean of the treatment group:

> set.seed(1) 
> brep <- bootstrap(mouse.t,25,mean,func=sd)

What is in the brep$jack.boot.val ?

Let's see how to get the jackknife-after-bootstrap values.

First, set the seed again, and create the bootstrap matrix of indicies

> set.seed(1) 
> matrix(sample(1:7, size = 7 * 25, replace = T), nrow = 25)

For example, if we want to compute brep$jac.boot.val[1] we would look for all the rows in the matrix that do not contain the number 1 and caculate the standard error of those $thetastar bootstrap estimates.

> sd(brep$thetastar[c(1,3,4,9,11,12,14,15,20,21,23,25)])

or depending on the version of R:
  
  > sd(brep$thetastar[c(4,7,8,11,14,15,18,21,25)])

Now, to get the jackknife-after-bootstrap estimate of the standard error (brep$jack.boot.se) of the bootstrap estimate of the standard error (brep$func.thetastar) :
  
  > sd(brep$jack.boot.val)*(7-1)/sqrt(7)

#Pretty neat.

#If you are interested in a jackknife function you can get your own function by:
  
source("http://www.rohan.sdsu.edu/~babailey/stat672/jackknife.r")

help("jackknife.r")

Better Bootstrap

We will use the function bcanon:
  
  Let's get our own bcanon function by:

> source("http://www.rohan.sdsu.edu/~babailey/stat672/bcanon.r")

Here is the function: bcanon.r

There is a help file available: bcanon.help

Above, we used the quantile function and construct percentiles of thetastar based on the 1000 bootstrap replications.

Let's use the bcanon function:
  
  > set.seed(6) 
> bca <- bcanon(mouse.t, 1000, mean)

What's in bca?

How do they compare?

Permutation Tests

From the mouse data, the difference in the means, thetahat= zbar - ybar = 30.63, which suggests that the Treatment distribution F gives longer survival times than does the Control distribution G. Consider the hypothsis:

H_o: F=G

thetahat = 1/n sum_{g_i=z} data - 1/m sum_{g_i=y} data

Choose B independent vectors g^*(1), g^*(2), ..., g^*(B), each consisting of n z's and m y's and each being randomly selected from all P(n,N=n+m) possible vectors. (B will ususally be at least 1000.)
Evaluate the permutation replication of thetahat corresponding to each permutation vector.
Estimate the permutataion achieved significance level (ASL) by
#{thetahat^* >= thetahat}/B

See if you can write a function or source code to do a permutation test for the difference of the means of the mouse data.

OK, is there an R function for permutation tests?