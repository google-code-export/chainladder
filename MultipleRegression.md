# Idea #
Transform a triangle in a table-like format and apply multiple regression to it.
```
library(ChainLadder)
dimnames(RAA)=list(origin=9:0, dev=0:9)
lRAA <- expand.grid(origin=as.numeric(dimnames(RAA)$origin), dev=as.numeric(dimnames(RAA)$dev))
lRAA$cum.value <- as.vector(RAA)
incRAA <- t(apply(RAA, 1,diff))
dimnames(incRAA) <- list(origin=9:0, dev=1:9)
lincRAA <- expand.grid(origin=as.numeric(dimnames(incRAA)$origin), dev=as.numeric(dimnames(incRAA)$dev))
lincRAA$inc.value <- as.vector(incRAA)
longRAA <- merge(lRAA, lincRAA, all=TRUE)
tmpRAA <- longRAA
tmpRAA$dev <- tmpRAA$dev + 1
names(tmpRAA[,3])
names(tmpRAA)[3]="prev.cum.value"

longRAA <- merge(longRAA, tmpRAA[,1:3])

pairs(na.omit(longRAA[,c(1,5,3,2,4)]), upper.panel=panel.smooth)

# Chain ladder link ratios from multiple regression:
 
lm(cum.value ~ 0 + factor(dev):prev.cum.value, weights=1/prev.cum.value, data=longRAA)


```