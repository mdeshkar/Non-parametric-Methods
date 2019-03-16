##################
## randomForest
help("randomForest")
library(randomForest)
fit.rf = randomForest(Class~., data=Credit)
print(fit.rf)
importance(fit.rf)
plot(fit.rf)
plot( importance(fit.rf), lty=2, pch=16)
lines(importance(fit.rf))
imp = importance(fit.rf)
impvar = rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op = par(mfrow=c(10,10 ))
for (i in seq_along(impvar)) {
  partialPlot(fit.rf, Credit, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]),
              ylim=c(0, 1))
}

###############
## OBLIQUE.TREE
install.packages("oblique.tree")
library(oblique.tree)

aug.crabs.data = data.frame( g=factor(rep(1:4,each=50)),
                             predict(princomp(crabs[,4:8]))[,2:3])
plot(aug.crabs.data[,-1],type="n")
text( aug.crabs.data[,-1], col=as.numeric(aug.crabs.data[,1]), labels=as.numeric(aug.crabs.data[,1]))
ob.tree = oblique.tree(formula = g~.,
                       data = aug.crabs.data,
                       oblique.splits = "only")
plot(ob.tree);text(ob.tree)