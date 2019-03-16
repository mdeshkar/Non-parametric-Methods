#Q1)
data <- read.table("http://www.rohan.sdsu.edu/~babailey/stat672/t6-6.txt",header = TRUE)
attach(data)
 boxplot(number~information)
 fit<-kruskal.test(number~information)
 summary(fit)
 # we accept H0 
 information<-factor(information)
 is.factor(information)
 fit1<- aov(number~information)
 summary(fit1)
## Q2)H0: ro== 0 V/s H1: ro >= 0
 data1<-read.table("http://www.rohan.sdsu.edu/~babailey/stat672/t8-3.txt",header = TRUE)
 cor.test(data1$x,data1$y,method = "spearman",alternative = "greater")
 # we reject H0
 ## Q3)Ho: To== 0 V/s To >= 0
 data2 <- read.table("http://www.rohan.sdsu.edu/~babailey/stat672/t8-3.txt",header = TRUE)
 cor.test(data2$x,data2$y,method="kendall",alternative = "greater")
 #we reject H0
 