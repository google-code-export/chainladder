
```
library(ChainLadder)
# Get the Taylor & Ash data
GenIns
# Apply MackChainLadder
MCL <- MackChainLadder(GenIns)
MCL
plot(MCL)

## investigate in more detail
MCL[["Models"]][[1]]   # Model for first development period
summary( MCL[["Models"]][[1]]) # Look at the model stats
op=par(mfrow=c(2,2)) # plot residuals
plot(MCL[["Models"]][[1]])
par(op)

## let's include an intercept in our model
newModel <- update(MCL[["Models"]][[1]], y ~ x+1, 
                   weights=1/MCL[["Triangle"]][1:9,1],
                   data=data.frame(x=MCL[["Triangle"]][1:9,1], 
                   y=MCL[["Triangle"]][1:9,2])
                   ) 

## view the new model
## this shows that only an intercept is needed
summary(newModel)


## let's reduce our model to an intercept
newModel2 <- update(MCL[["Models"]][[1]], y ~ 1, 
                   weights=1/MCL[["Triangle"]][1:9,1],
                   data=data.frame(x=MCL[["Triangle"]][1:9,1], 
                   y=MCL[["Triangle"]][1:9,2])
                   ) 


## change the model for dev. period one to the newModel
MCL2 <- MCL
MCL2[["Models"]][[1]] <- newModel2
predict(MCL2) ## predict the full triangle with the new model 
##(only the last origin year will be affected)

MCL2[["FullTriangle"]] <-  predict(MCL2)
MCL2[["FullTriangle"]] 
MCL2   # Std. Errors have not been re-estimated!
## plot the result and residuals      
plot(MCL2, title="Changed MCL Model")

## How can we recalculate the standard error?

```