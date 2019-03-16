 x<-scan()
y <- scan() 
wilcox.test(x,y)
str(wilcox.test)
Post <-scan()
Pre <- scan()    
wilcox.test(Pre,Post, paired=TRUE, alterative = "less")
Z <- Pre - Post;Z
r <- rank(abs(Z))
print(rbind(Z, r))
z <- Z[Z!=0];z
tplus <- sum(r[Z>0])
tplus