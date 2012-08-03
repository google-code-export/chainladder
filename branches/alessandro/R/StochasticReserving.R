#################################################
###                                           ###
###     Ultimate Reserve Risk Calculation     ###
###   by Alessandro Carrato and Luigi Lotti   ###
###       alessandro.carrato@gmail.com        ###
###                                           ###
#################################################


library(ChainLadder) #Load Chain Ladder package
library(tweedie) #for var.power fitting

####ONLY FOR DEBUG########

#Load toList function (to convert data.set in matrix ... to be improved?
toList <- function(data) {
  matr<-cbind(data[,1],data[,2])
  for(i in 3:length(data[1,])){
    matr<-cbind(matr,data[,i])
  }
  matr
}

#carico triangolo ed esposizione
triangleC<-as.triangle(toList(read.csv("P:/Risk_MAnagement/Analysis on demand/Luigi/RExcel Reserving/dati/GTPLMED.csv",h=F)))
expos<-read.csv("P:/Risk_MAnagement/Analysis on demand/Luigi/RExcel Reserving/dati/exp.csv",h=F)
#attr(triangleC,"exposure")<-expos[,1]
triangleC

####ONLY FOR DEBUG######


fit_gamma <- function(coeffs,design.type,n){
  pos.gamma<-2 #intercept + next position = 2
  for (i in 1:2){ 
    if (design.type[i] == 1) {
      pos.gamma<-pos.gamma+n
    }
    if (design.type[i] == 2) {
      pos.gamma<-pos.gamma+1
    }
  }
  y<-coeffs[pos.gamma:length(coeffs)]
  y<-y[!is.na(y)]
  x<-seq(1,length(y))
  new <- data.frame(x = seq(1, 2*n))
  new$y<-predict(lm(y~x),new)
  gamma_y<-rep(0,2*n)
  for (k in 1:(2*n)){
    if (!is.na(coeffs[pos.gamma+k-1])){
      gamma_y[k]<-coeffs[pos.gamma+k-1]
    }
    else {
      gamma_y[k]<-new$y[k]
    }    
  }
  
  out<-c(list(coeffs=append(coeffs[1:(pos.gamma-1)],gamma_y),gamma_y=gamma_y))
  return(out)
}


stochasticReserve <- function(triangle, var.power=1, link.power=0, design.type=c(1,1,0), ##link.power=0 is the log link ...
                              cum=TRUE, exposure=TRUE, bootstrap=0, boot.adj=0, nsim=1000, proc.err=TRUE, p.optim=F,...){
  
  call <- match.call()
  if (!("triangle") %in% class(triangle))
    stop("triangle must be of class 'triangle'")
  if ("offset" %in% names(list(...)))
    stop("'offset' should be passed using the 
         'exposure' attribute of the triangle!")
  if ("weigth" %in% names(list(...)))
    stop("'weight' should not be used")  
  # convert to incremental losses if needed
  # tr.incr <- cum2incr(triangleC)   ## giusto per far prima in debug!!
  tr.incr <- if (cum) cum2incr(triangle) else triangle
  
  # create family                            
  family <- tweedie(var.power=var.power,link.power=link.power)
  
  # convert to long format
  lda <-  as.data.frame(tr.incr)
  lda$offset <- if (is.null(attr(tr.incr,"exposure")))
    rep(0,nrow(lda)) else 
      family$linkfun(attr(tr.incr,"exposure")[lda$origin])
  
  #parameter fix for better intrepretation of results
  lda$origin<-lda$origin-1
  lda$dev<-lda$dev-1
  lda$cy <- lda$origin + lda$dev
  
  ######################################
  ####DESIGN MATRIX CALCULATION#####
  ######################################
  
  
  #  design matrix per AY
  design.string<-" "
  temp<-rep(1,length(lda$origin))
  
  if (design.type[1] == 0){
    ay<-matrix(0,nrow= length(lda$origin),ncol=1)
  }
  if (design.type[1] == 1){
    
    design.string=paste(design.string,"factor(origin)")
    
    ay<-matrix(0,nrow= length(lda$origin), ncol=max(lda["origin"]))  #ay<-matrix(0,nrow= length(lda$origin), ncol=max(lda["origin"])+1)
    for(r in 1:length(lda$origin)){
      if (lda$origin[r]!=0) {ay[r,lda$origin[r]]=1}
    }
    ay<-cbind(temp,ay)       
  }
  if (design.type[1] == 2){
    
    design.string=paste(design.string,"origin")
    
    ay<-matrix(0,nrow= length(lda$origin), ncol=1)
    for(r in 1:length(lda$origin)){
      ay[r,1]=lda$origin[r]
    }
    ay<-cbind(temp,ay)
  }
  
  #  design matrix per DY
  if (design.type[2] == 0){
    dy<-matrix(0,nrow= length(lda$dev),ncol=1)
  }
  if (design.type[2] == 1){
    
    if (design.type[1]==0) {
      design.string=paste(design.string,"factor(dev)")
    } 
    else {
      design.string=paste(design.string,"+ factor(dev)")
    }
    
    dy<-matrix(0,nrow= length(lda$dev), ncol=max(lda["dev"]))
    for(r in 1:length(lda$dev)){
      if (lda$dev[r]!=0) {dy[r,lda$dev[r]]=1}
    }
    dy<-cbind(temp,dy)
    
  }
  if (design.type[2] == 2){
    
    if (design.type[1]==0) {
      design.string=paste(design.string,"dev") 
    }
    else {
      design.string=paste(design.string,"+ dev")
    }
    
    dy<-matrix(0,nrow= length(lda$dev), ncol=1)
    for(r in 1:length(lda$dev)){
      dy[r,1]=lda$dev[r]
    }
    dy<-cbind(temp,dy)
  }
  
  #  design matrix per CY
  if (design.type[3] == 0){
    cy<-matrix(0,nrow= length(lda$cy),ncol=1)
  }
  if (design.type[3] == 1){
    
    if (design.type[1]==0 && design.type[2]==0) {
      design.string=paste(design.string,"factor(cy)")
    }
    else {
      design.string=paste(design.string,"+ factor(cy)")
    }
    
    cy<-matrix(0,nrow= length(lda$cy), ncol=max(lda["cy"]))
    for(r in 1:length(lda$cy)){
      if (lda$cy[r]!=0) {cy[r,lda$cy[r]]=1}
    }
    cy<-cbind(temp,cy)
    
  }
  if (design.type[3] == 2){
    
    if (design.type[1]==0 && design.type[2]==0) {
      design.string=paste(design.string,"cy")
    }
    else {
      design.string=paste(design.string,"+ cy")
    }
    
    cy<-matrix(0,nrow= length(lda$cy), ncol=1)
    for(r in 1:length(lda$cy)){
      cy[r,1]=lda$cy[r]
    }
    cy<-cbind(temp,cy)
    
  }
  
  design.matrix<-cbind(ay,dy,cy)
  design.matrix<-cbind(temp,subset(design.matrix, select=-c(1,length(ay[1,])+1,(length(ay[1,])+length(dy[1,])+1))))
  
  ######################################
  ####END DESIGN MATRIX CALCULATION#####
  ######################################
  
  
  ldaFit <- subset(lda,!is.na(lda$value)) 
  ldaOut <- subset(lda,is.na(lda$value))
  
  # fit the model
  #glmFit<-glm(value~factor(origin)+factor(dev),family=quasipoisson(log),data=ldaFit)
  
  if (exposure){
    glmstring<-paste(design.string,", family = family, data=ldaFit,offset=offset,...")
  }
  else{
    glmstring<-paste(design.string,", family = family, data=ldaFit,...")
  }
  
  eval(parse(text=
    c(
      "glmFit<-glm(value ~", glmstring,
      ")"
    )
  )
  )  
  
  ################################
  ## calculate reserve 
  ################################
  
  # prediction for each cell
  
  coeffs <- glmFit$coefficients
  temp_y<-data.frame(gamma_y<-c(0))
  
  # an extrapultion of future CY factors has to be made for the future CY
  if (design.type[3]==1) {
    temp_y<-fit_gamma(coeffs,design.type,n=max(lda$origin)) ## NOTE: 0 included!!! so actually we have (n+1) values!!
    coeffs<-temp_y$coeffs
  }
  n <- nrow(ldaFit) ## n° of data points (n)
  d.f<- df.residual(glmFit) ## n° of data points - parameters (n-p)
  bias <- sqrt(n/d.f)
  
  # dispersion
  #phi <- sum(resid(glmFit,type="pearson")^2)/d.f
  phi <- summary(glmFit)$dispersion
  
  lda$eta <- design.matrix %*% coeffs
  lda$eta <- lda$eta + lda$offset
  lda$yp <- exp(lda$eta)  
  
  # sum to get reserve by year
  
  resMeanAy <- tapply(lda$yp[is.na(lda$value)],ldaOut$origin, sum)
  resMeanTot <- sum(resMeanAy)
  
  ################################
  ## calculate prediction err 
  ################################                
  
  # process variance              
  
  Reserve <- round(c(resMeanAy,resMeanTot))
  Latest <- getLatestCumulative(incr2cum(tr.incr))[-1L]
  Latest <- c(Latest,sum(Latest))
  Ultimate <- Latest + Reserve
  
  count.neg<-0
  ###BOOTSTRAP CYCLE###
  if (bootstrap!=0){ 
    resMeanAyB <- matrix(0,length(resMeanAy),nsim)
    resMeanTotB <- rep(0,nsim)
    
    # loop nsim times 
    for (b in 1:nsim){      
      mu <- lda$yp[!is.na(lda$value)]
      
      ### PARAMETRIC BOOTSTRAP ###
      if(bootstrap==1) { 
        yB=rtweedie(length(mu),phi=phi,xi=var.power,mu=mu)
      }
      
      ### SEMI-PARAMETRIC BOOTSTRAP ###
      else{
        ### WHILE CYCLE until triangle > 0 ... WARNING: COULD BE TIME CONSUMING!!!!
        if(boot.adj==0){
          ybad <- 1 
          while (ybad){
            rn <- bias * sample(resid(glmFit, type = "pearson"), n, replace = TRUE)
            yB <- rn * sqrt(family$variance(mu)) + mu
            if (all(yB >= 0) || (!is.null(var.power) && var.power == 0))
              ybad <- 0  # Normal 
          }
          
        }
        
        ### OVERWRITE NEGATIVE VALUES with 0.01 ... WARNING: COULD LEAD TO LOWER UNCERTAINTY!!
        else{
          rn <- bias * sample(resid(glmFit,type="pearson"), n, replace=TRUE) ##adjustment for df
          yB <- rn * sqrt(family$variance(mu)) + mu      
          if (!all(yB>=0) || (!is.null(var.power) && var.power != 0)) {
            for (i in 1:n){
              if (yB[i]<=0) {        
                yB[i]<-0.01
                count.neg<-count.neg+1
              }
            }
          }
        }
      }
      
      
      eval(parse(text=
        c(
          "glmFitB<-glm(yB~",glmstring,
          ")"
        )
      )
      )
      
      coeffsB <- glmFitB$coefficients
      
      if (design.type[3]==1) {
        temp_yB<-fit_gamma(coeffsB,design.type,n=max(lda$origin)) ## NOTE: 0 included!!! so actually we have (n+1) values!!
        coeffsB<-temp_yB$coeffs
      }
      
      lda$etaB <- design.matrix %*% coeffsB
      lda$etaB <- lda$etaB + lda$offset
      lda$ypB <- exp(lda$etaB)
      
      ##ADD PROCESS ERROR
      if (proc.err){
        lda$ypB[is.na(lda$value)]=rtweedie(length(lda$ypB[is.na(lda$value)]),xi=var.power,mu=lda$ypB[is.na(lda$value)],phi=phi)
        
        ### OLD GAMMA APPROACH ###
        #a = (lda$ypB[is.na(lda$value)]^2) / (phi*family$variance(lda$ypB[is.na(lda$value)]))   ##phi*family$variance(yB[i])
        #s = lda$ypB[is.na(lda$value)] / a
        #rgamma(length(lda$ypB[is.na(lda$value)]),shape=a,scale=s)
      }
      
      resMeanAyB[,b] <- tapply(lda$ypB[is.na(lda$value)],ldaOut$origin, sum)
      resMeanTotB[b] <- sum(resMeanAyB[,b])     
      
    }
    # compute estimation variance, adjusted by df 
    
    ##the adjustment for DF is included directly in residuals (see England(2002) Addendum) or in parametric bootstrap
    mseEstAy <- apply(resMeanAyB,1,var)
    mseEstTot <- var(resMeanTotB)
    
    avgResAy <- apply(resMeanAyB,1,mean)
    avgResTot <- mean(resMeanTotB)
    
    Expected.Reserve <- round(c(avgResAy,avgResTot))
    
    S.E <- sqrt(c(mseEstAy,mseEstTot))
    CoV <- S.E / Expected.Reserve
    
    # percentage of negative values modified
    
  }
  
  perc.neg<-count.neg/(nsim*n)
  
  # compile results
  
  
  #WRITING REPORT
  if (bootstrap!=0) {
    resDf <- data.frame(Latest=Latest, 
                        Expected.Reserve=Expected.Reserve,
                        Prediction.Error=S.E,
                        CoV=CoV,
                        Expected.Ultimate=Latest+Expected.Reserve,
                        GLMReserve=Reserve,
                        Reserve.difference=Expected.Reserve/Reserve-1,
                        Dev.To.Date=Latest/Ultimate)
  }
  else {
    resDf <- data.frame(Latest=Latest, Reserve=Reserve,
                        Dev.To.Date=Latest/Ultimate,
                        Ultimate=Ultimate)
  }
  
  row.names(resDf) <- c(as.character(sort(unique(ldaOut$origin))),"total")
  
  # produce fully projected triangle
  ldaOut$value <- round(lda$yp[is.na(lda$value)])
  FullTriangle <- as.triangle(rbind(ldaFit,ldaOut))
  if (cum)
    FullTriangle <- incr2cum(FullTriangle)
  
  res.diag<-data.frame(unscaled=resid(glmFit,type="pearson"),
                       unscaled.biasadj=resid(glmFit,type="pearson")*bias,
                       scaled=resid(glmFit,type="pearson")/sqrt(phi),
                       scaled.biasadj=resid(glmFit,type="pearson")*bias/sqrt(phi),
                       dev=ldaFit$dev,
                       origin=ldaFit$origin,
                       cy=ldaFit$cy)
  
  # output
  out <- c(list(call=call,summary=resDf,
                Triangle=triangle,
                FullTriangle=FullTriangle,
                scale=phi,
                gamma_y=temp_y$gamma_y,
                res.diag=res.diag,
                if(bootstrap==2 && boot.adj==1) {perc.neg=perc.neg}
                ), 
           
           glmFit[!(names(glmFit) %in% c("call"))]
  )
  
  if (p.optim){
    #library(tweedie)
    p<-tweedie.profile(value~as.factor(dev)+cy,p.vec=c(seq(0, 2, by=0.1),3),data=ldaFit,offset=offset,do.plot=T)
    
    if (exposure){
      glmstring<-paste(design.string,", family = family, data=ldaFit,p.vec=c(seq(0, 2, by=0.1),3),do.plot=T,offset=offset")
    }
    else{
      glmstring<-paste(design.string,", family = family, data=ldaFit,p.vec=c(seq(0, 2, by=0.1),3),do.plot=T")
    }
    eval(parse(text=
      c(
        "p<-tweedie.profile(value ~", glmstring,
        ")"
      )
    )
    ) 
    
  }
  
  class(out) <- "glm"                     
  return(out)  
}


###DEBUG ONLY####
set.seed(42)
a<-stochasticReserve(triangleC,design.type=c(1,1,0),bootstrap=T,nsim=10000,boot.param=TRUE,proc.err=FALSE)
