#Q1a)
install.packages("exactRankTests")
library(exactRankTests)

testdat <- read.table("http://www.rohan.sdsu.edu/~babailey/stat672/testdat.txt",
                      header=T)
wilcox.test(testdat$y, testdat$x, paired=TRUE, alternative="two.sided")
#as p-value is less than the alpha so we reject the null hypothesis at 90% confidence level.

#Point Estimate of the theta
z <- sort(testdat$y - testdat$x)
walsh <- outer(z, z, "+") / 2
print(median(walsh)) # point estimate of median = -0.487823

# CI using wlash average
conf.level <- 0.90
z <- sort(testdat$y - testdat$x)
(n <- length(z))
walsh <- outer(z, z, "+") / 2
walsh <- sort(walsh[lower.tri(walsh, diag = TRUE)])
print(m <- length(walsh))
alpha <- 1 - conf.level
k <- qsignrank(alpha / 2, n)
if (k == 0) k <- k + 1
print(k)
cat("achieved confidencelevel:",
    1 - 2 * psignrank(k - 1, n), "\n")
c(walsh[k], walsh[m + 1 - k])
#Achived condidence interval is 0.9006407
# 90% CI is -0.94424815 to -0.06602079
boxplot(z)
#B1 <-A1:-  Z = Y ??? X, for i = 1, . . . , n. The differences Z1, . . . , Zn are mutually
#independent as x and y comes from the random samling of the cauchy and normal distribution. 
#B2:As in the box plot we can see that Z are symmetric about the median and x and y comes form the contineous cauchuy and normal distribution.so z will also be contineous.

#b Wilcox Rank sum Test
mu <- 0 # hypothesized value of median difference
x <- testdat$x[!is.na(x)]
y <- testdat$y[!is.na(y)]
print(nx <- length(x))
print(ny <- length(y))
y <- y - mu
data <- c(x, y)
names(data) <- c(rep("x", nx), rep("y", ny))
data <- sort(data)
r <- rank(data)
rbind(data, r)
print(w <- sum(r[names(data) == "y"]))
print(u <- w - ny * (ny + 1) / 2)
2*pwilcox(u, nx, ny)#Pvalue= 0.21 of the two tail is double the P-value of the one tail
# At alpha=0.10 significance levele we accept Ho as P-value is greater than the alpha.
# A1&A2:- X and y are iid Variables as well as mutually independent as they are picked up randomly from the the cauchy and the normal distribution
#A3:-X and y are contineous because they are picked form the contineous normal and cauchy distribution.

#Q2)b H0:- n random variables are actually observaiton from a contineous distribution
x<-c(1.8,1.0,1.5,1.4,1.9,1.6,1.2,1.7,1.3)
ri<-rank(x)
ri
print(ai<-c(rep(1,3),rep(2,3),rep(3,3)))
print(L<-sum(ai*ri))# L = 88
E_L <- 90 ; Var_L <- 45
print(large_sample<- (L-E_L)/sqrt(Var_L))#large sample approximation = -0.3
pnorm(large_sample)# pvalue=0.38
# we fail to reject Ho at alpha=0.05 significance level
