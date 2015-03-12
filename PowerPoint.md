The following code demonstrates how you can put R graphs output in PowerPoint using the `rcom` package.

```
# rcom create R-plot in PowerPoint

library(rcom)
library(ChainLadder)
library(MASS)

M <- MackChainLadder(RAA)
B <- BootChainLadder(RAA)
# fit a log-normal distribution
fit <- fitdistr(B$IBNR.Totals[B$IBNR.Totals>0], "lognormal")

plotMack=tempfile()
win.metafile(file=plotMack)
plot(M)
dev.off()

plotBoot=tempfile()
win.metafile(file=plotBoot)
plot(B)
dev.off()

plotFit=tempfile()
win.metafile(file=plotFit)
plot(ecdf(B$IBNR.Totals))
curve(plnorm(x,fit$estimate["meanlog"], fit$estimate["sdlog"]), col="red", add=TRUE)
dev.off()

## Now let's put the graphs into PowerPoint
ppt<-comCreateObject("Powerpoint.Application")
comSetProperty(ppt,"Visible",TRUE)
myPresColl<-comGetProperty(ppt,"Presentations")
myPres<-comInvoke(myPresColl,"Add")
mySlides<-comGetProperty(myPres,"Slides")

mySlide<-comInvoke(mySlides,"Add",1,12)
myShapes<-comGetProperty(mySlide,"Shapes")
myPicture<-comInvoke(myShapes,"AddPicture",
 plotFit,0,1,100,10)

mySlide<-comInvoke(mySlides,"Add",1,12)
myShapes<-comGetProperty(mySlide,"Shapes")
myPicture<-comInvoke(myShapes,"AddPicture",
 plotBoot,0,1,100,10)

mySlide<-comInvoke(mySlides,"Add",1,12)
myShapes<-comGetProperty(mySlide,"Shapes")
myPicture<-comInvoke(myShapes,"AddPicture",
 plotMack,0,1,100,10)
```