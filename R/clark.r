# Clark method from 2003 eForum paper
# Author: Daniel Murphy
# Date: October 31, 2010
# Includes:
#   LDF and Cape Cod methods
#   Two growth functions -- loglogistic and weibull
#   print method: displays the table on p. 65 of the paper
#   plot method: displays 3 residual plots, QQ-plot with 
#       results of Shapiro-Wilk normality test.

# Organization:
#   "LDF" and "CapeCod" scripts that accept the data (a matrix)
#       and arguments to flag:
#           Does the matrix hold cumulative or incremental data?
#           In the case of the Cape Code method, a premium vector
#               whose length = nrow(data)
#           Should the analysis be conducted based on the average
#               date of loss within the origin period?
#           What is the maximum age to which losses should be projected?
#           What growth function should be utilized?
#   print and plot methods
#   Functions
#       Definition of "function class with derivatives"
#           Generics to access the derivatives of a function
#       Definition of growth function class with derivatives with respect to
#           the second, parameter argument
#       Loglogistic growth function
#       Weibull growth function
#       Loglikelihood function under Clark's ODP assumption
#       Function to calculate sigma2
#       Expected value (MU) functions
#           LDF Method
#           Cape Cod Method
#       Reserve Functions
#           LDF Method
#           Cape Cod Method

#require(ChainLadder)

clarkLDF <- function(data,
        cumulative = TRUE,
        adol = TRUE,
        maxage = c(Inf, Inf),
        G = "loglogistic"
        ) {
    # maxage can be a vector of length 1 or 2 and represents the age
    #   to which losses are projected at Ultimate
    # 1st element is the "traditional" age measured from the beginning 
    #   of the origin year
    # If second element exists, then it is the length of time from the
    #   average date of loss (adol.age) of the origin year, which is
    #   only relevant if adol=TRUE
    
    G <- switch(G,
         loglogistic = loglogistic,
         weibull = weibull,
         stop(paste("Growth function '", G, "' unrecognized", sep=""))
         )
        
    if (!is.matrix(data)) stop("clarkLDF expects data in matrix format")
    nr <- nrow(data)
    if (nr < 4L || ncol(data) < 4L) stop("matrix must be at least 4x4")

    dev <- as.numeric(colnames(data))
    if (any(is.na(dev))) stop("non-'age' column name(s)")
    if (any(dev[-1L]<=head(dev, -1L))) stop("ages must be strictly increasing")
    
    # 'workarea' stores intermediate calculations
    workarea <- new.env()

    if (!inherits(data, "triangle")) data <- as.triangle(data)

    # Save the origin, dev names
    origins <- rownames(data)
    devs <- colnames(data)
    # Save user's names for 'origin' (row) and 'dev' (column), if any
    dimnms <- c("origin", "dev")
    if (!is.null(nm<-names(dimnames(data)))) 
        # If only one name specified by user, other will be NA
        dimnms[!is.na(nm)] <- nm[!is.na(nm)]

    # Calculate the age.from/to's.
    Age.to <- dev
    Age.from <- c(0, head(Age.to, -1L))
    if (adol) {
        Age.to <- (Age.from + Age.to)/2
        Age.from <- c(0, head(Age.to, -1L))
        colnames(data) <- Age.to
        }

    # maxage can be a vector of length 1 or 2
    # 1st element is the "traditional" age measured from the beginning 
    #   of the origin year
    # If second element exists, then it is the length of time from the
    #   average date of loss (adol.age) of the origin year, which is
    #   only relevant if adol=TRUE
    # If the second element does not exist, we'll guess its value.
    maxage.traditional <- maxage[1L]
    if (length(maxage)>1L) {
        if (adol) maxage <- maxage[2L]
        else {
            warning("Length(maxage)>2 but !adol -- maxage[2] ignored")
            maxage <- maxage[1L]
            }
        }
    else {
        if (adol) {
            agediff <- (Age.to-Age.from)[-1L]
            meandiff <- mean(agediff)/2
            if (!all(agediff==agediff[1L]))
                warning("Varying age differences; check reasonability of maxage calculation")
            maxage <- maxage.traditional - meandiff
            }
        }
    

    # Before massaging 'data' below, calculate some values we'll need later

    # Save current cumulative diagonal and its age
    CurrentValue <- getLatestCumulative(if (cumulative) data else incr2cum(data))
    CurrentAge <- getLatestCumulative({
        z <- col(data)
        z[is.na(data)]<-NA
        array(dev[z], dim(data))
        })
    CurrentAge.from <- getLatestCumulative(array(Age.from[z], dim(data)))
    CurrentAge.to <- getLatestCumulative(array(Age.to[z], dim(data)))

    # We will use chainladder estimates as starting parameter values for U.
    CLU <- if (cumulative) predict(chainladder(data))[,ncol(data)] else predict(chainladder(incr2cum(data)))[,ncol(data)]

    # Massage 'data': make incremental triangle, first wide then long format
    
    # Turn loss matrix into incremental losses, if not already
    if (cumulative) data <-cum2incr(data)

    # Create the "long format" data.frame as in Table 1.1 of paper.
    Table1.1 <- as.data.frame(as.triangle(data))
    Table1.1$origin <- seq.int(nr)
    Table1.1$dev <- rep(seq.int(ncol(data)), each=nr)
    Table1.1$Age.from <- rep(Age.from, each=nr)
    Table1.1$Age.to <- rep(Age.to, each=nr)
    Table1.1 <- Table1.1[!is.na(Table1.1[[3L]]),]
    
    # "prime" workarea with initial data
    workarea$origin   <- Table1.1$origin
    workarea$value    <- Table1.1[[3L]]
    workarea$Age.from <- Table1.1$Age.from
    workarea$Age.to   <- Table1.1$Age.to
    workarea$dev      <- devs[Table1.1$dev]
    workarea$nobs     <- nobs <- nrow(Table1.1)
    workarea$np       <- np   <- nr + G@np
    rm(Table1.1)

    # Calc starting values for the parameters, call optim
    theta <- c(structure(CLU, names=origins), G@initialGuess(workarea))
    S <- optim(
        par = theta,
        LL.ODP, # function to be maximized (fmscale=-1)
        gr = dLL.ODPdt,     ## much faster with gradient function 
        MU.LDF,
        G,              
        workarea,
        method="L-BFGS-B",
        lower=c(CurrentValue, .1, min(c(.5, workarea$Age.to))),
        control = list(
            fnscale=-1,
            parscale=c(CLU, 1, 1),
            maxit=100000
            ),
        hessian=FALSE
        )


    if (S$convergence>0) {
        msg <- "Maximum likelihood solution not found."
        if (S$convergence == 1)
            msg<-paste(msg,"Max interations (100000) reached.")
        else msg<-paste(msg,"'convergence' code=",S$convergence)
        warning(msg)
        return(NULL)
        }

    # Pull the parameters out of the solution list
    theta <- S$par
    K  <- np - G@np
    K1 <- seq.int(K)
    U<-thetaU <- theta[K1]
    thetaG <- theta[seq.int(length = G@np, to = np)]
    
    # Calculate the sigma2 "scaling parameter"
    sigma2 <- workarea$sigma2 <- LL.ODP.sigma2(workarea)

    # AY DETAIL LEVEL

    # Expected value of reserves
    R <- R.LDF(theta, G, CurrentAge.to, maxage, workarea)
    # Alternatively, by developing current diagonal
    #   For LDF formula, see table at top of p. 64
    LDF <- G(maxage, thetaG) / G(CurrentAge.to, thetaG)
    R.alt <- (LDF - 1) * CurrentValue

    # PROCESS RISK OF RESERVES
    gammar2 <- R * sigma2
    gammar <- sqrt(gammar2)

    # PARAMETER RISK OF RESERVES

    # Calculate the Fisher Information matrix = matrix of
    #   2nd partial derivatives of the LL fcn w.r.t. all parameters
    workarea$FI <- FI <- d2LL.ODPdt2(S$par, MU.LDF, G, workarea)

    # Calculate the gradient matrix, dR = matrix of 1st partial derivatives
    #   for every origin year w.r.t. every parameter
    dR <- dfdx(R.LDF, theta, G, CurrentAge.to, maxage, workarea)
    
    # Delta Method => approx var/cov matrix of reserves
    VCOV.R <- -workarea$sigma2*t(dR)%*%solve(FI,dR)

    # Origin year parameter risk estimates come from diagonal entries
    # Parameter risk for sum over all origin years = sum  over
    #   entire matrix (see below).
    deltar2 <- diag(VCOV.R)
    deltar  <- sqrt(deltar2)

    # Total Risk = process risk + parameter risk
    totalr2 <- gammar2 + deltar2
    totalr  <- sqrt(totalr2)

    # AY Total row
    CurrentValue.sum <- sum(CurrentValue)
    R.sum <- sum(R)
    R.alt.sum <- sum(R.alt)
    gammar2.sum <- sum(gammar2)
    gammar.sum = sqrt(gammar2.sum)
    deltar2.sum <- sum(VCOV.R)
    deltar.sum <- sqrt(deltar2.sum)
    totalr2.sum <- gammar2.sum + deltar2.sum
    totalr.sum = sqrt(totalr2.sum)

    # Form "report table" as on p. 65 of paper
    Table65 <- data.frame(
        Origin = c(origins, "Total"),
        CurrentValue = c(CurrentValue, CurrentValue.sum),
        EstimatedReserves = c(R.alt, R.alt.sum),
        ProcessSE = c(gammar, gammar.sum),
        ProcessCV = 100*round(c(gammar, gammar.sum) / c(R.alt, R.alt.sum), 3),
        ParameterSE = c(deltar, deltar.sum),
        ParameterCV = 100*round(c(deltar, deltar.sum) / c(R.alt, R.alt.sum), 3),
        TotalSE = c(totalr, totalr.sum),
        TotalCV = 100*round(c(totalr, totalr.sum) / c(R.alt, R.alt.sum), 3),
        stringsAsFactors = FALSE
        )

    g <- G(c(maxage, CurrentAge.to), thetaG)
    Table64 <- data.frame(
        Origin = c("", origins, "Total"),
        CurrentValue = c(NA, CurrentValue, CurrentValue.sum),
        CurrentAge = c(maxage.traditional, CurrentAge, NA),
        AgeUsed = c(maxage, CurrentAge.to, NA),
        GrowthFunction = c(g, NA),
        LDF = c(1 / g, NA),
        TruncatedLDF = c(G(maxage, thetaG) / g, NA),
        LossesAtMaxage = c(NA, CurrentValue + R.alt, CurrentValue.sum + R.alt.sum),
        EstimatedReserves = c(NA, R.alt, R.alt.sum),
        stringsAsFactors = FALSE
        )

    structure(
        list(
            method = "LDF",
            growthFunction = G@name,
            Table65=Table65,
            Table64=Table64,
            par=c(unclass(S$par)),
            sigma2=c(unclass(sigma2)),
            LDF = LDF,
            dR=dR,
            origin = workarea$origin,
            age = workarea$dev,
            fitted = workarea$mu,
            residuals = workarea$residuals,
            stdresid = workarea$residuals/sqrt(sigma2*workarea$mu),
            FI=FI
            ),
        class=c("clark","list")
        )
    }

clarkCapeCod <- function(data,
        Premium,
        cumulative = TRUE,
        adol = TRUE,
        maxage = c(Inf, Inf),
        G = "loglogistic"
        ) {
    # maxage can be a vector of length 1 or 2 and represents the age
    #   to which losses are projected at Ultimate
    # 1st element is the "traditional" age measured from the beginning 
    #   of the origin year
    # If second element exists, then it is the length of time from the
    #   average date of loss (adol.age) of the origin year, which is
    #   only relevant if adol=TRUE
        
    G <- switch(G,
         loglogistic = loglogistic,
         weibull = weibull,
         stop(paste("Growth function '", G, "' unrecognized", sep=""))
         )
        
    if (!is.matrix(data)) stop("clarkCapeCod expects data in matrix format")
    nr <- nrow(data)
    if (nr < 4L || ncol(data) < 4L) stop("matrix must be at least 4x4")

    dev <- as.numeric(colnames(data))
    if (any(is.na(dev))) stop("non-'age' column name(s)")
    if (any(dev[-1L]<=head(dev, -1L))) stop("ages must be strictly increasing")
    
    # 'workarea' stores intermediate calculations
    workarea <- new.env()

    if (!inherits(data, "triangle")) data <- as.triangle(data)

    # Save the origin, dev names
    origins <- rownames(data)
    devs <- colnames(data)
    # Save user's names for 'origin' (row) and 'dev' (column), if any
    dimnms <- c("origin", "dev")
    if (!is.null(nm<-names(dimnames(data)))) 
        # If only one name specified by user, other will be NA
        dimnms[!is.na(nm)] <- nm[!is.na(nm)]

    # Calculate the age.from/to's.
    Age.to <- dev
    Age.from <- c(0, head(Age.to, -1L))
    if (adol) {
        Age.to <- (Age.from + Age.to)/2
        Age.from <- c(0, head(Age.to, -1L))
        colnames(data) <- Age.to
        }

    # maxage can be a vector of length 1 or 2
    # 1st element is the "traditional" age measured from the beginning 
    #   of the origin year
    # If second element exists, then it is the length of time from the
    #   average date of loss (adol.age) of the origin year, which is
    #   only relevant if adol=TRUE
    # If the second element does not exist, we'll guess its value.
    maxage.traditional <- maxage[1L]
    if (length(maxage)>1L) {
        if (adol) maxage <- maxage[2L]
        else {
            warning("Length(maxage)>2 but !adol -- maxage[2] ignored")
            maxage <- maxage[1L]
            }
        }
    else {
        if (adol) {
            agediff <- (Age.to-Age.from)[-1L]
            meandiff <- mean(agediff)/2
            if (!all(agediff==agediff[1L]))
                warning("Varying age differences; check reasonability of maxage calculation")
            maxage <- maxage.traditional - meandiff
            }
        }
    

    # Before massaging 'data' below, calculate some values we'll need later

    # Save current cumulative diagonal and its age
    CurrentValue <- getLatestCumulative(if (cumulative) data else incr2cum(data))
    CurrentAge <- getLatestCumulative({
        z <- col(data)
        z[is.na(data)]<-NA
        array(dev[z], dim(data))
        })
    CurrentAge.from <- getLatestCumulative(array(Age.from[z], dim(data)))
    CurrentAge.to <- getLatestCumulative(array(Age.to[z], dim(data)))

    # We will use chainladder estimates as starting parameter values for U.
    CLU <- if (cumulative) predict(chainladder(data))[,ncol(data)] else predict(chainladder(incr2cum(data)))[,ncol(data)]

    # Massage 'data': make incremental triangle, first wide then long format
    
    # Turn loss matrix into incremental losses, if not already
    if (cumulative) data <-cum2incr(data)

    # Create the "long format" data.frame as in Table 1.1 of paper.
    Table1.1 <- as.data.frame(as.triangle(data))
    Table1.1$origin <- seq.int(nr)
    Table1.1$P   <- rep(Premium, ncol(data))
    Table1.1$dev <- rep(seq.int(ncol(data)), each=nr)
    Table1.1$Age.from <- rep(Age.from, each=nr)
    Table1.1$Age.to <- rep(Age.to, each=nr)
    Table1.1 <- Table1.1[!is.na(Table1.1[[3L]]),]
    
    # "prime" workarea with initial data
    workarea$origin   <- Table1.1$origin
    workarea$value    <- as.numeric(Table1.1$value)
    workarea$P        <- Table1.1$P
    workarea$Age.from <- Table1.1$Age.from
    workarea$Age.to   <- Table1.1$Age.to
    workarea$dev      <- devs[Table1.1$dev]
    workarea$nobs     <- nobs <- nrow(Table1.1)
    workarea$np       <- np   <- 1L + G@np
    rm(Table1.1)

    # Calc starting values for the parameters, call optim
    theta <- c(ELR=ifelse(is.na(sum(CLU)), .5, sum(CLU)/sum(Premium)), G@initialGuess(workarea))

    S <- optim(
        par = theta,
        LL.ODP, # function to be maximized (fmscale=-1)
        gr = dLL.ODPdt,     ## much faster with gradient function 
        MU.CapeCod,
        G,              
        workarea,
        method="L-BFGS-B",
        lower=c(.01, .1, min(c(.5, workarea$Age.to))),
        control = list(
            fnscale=-1,
            maxit=100000
            ),
        hessian=FALSE
        )

    if (S$convergence>0) {
        msg <- "Maximum likelihood solution not found."
        if (S$convergence == 1)
            msg<-paste(msg,"Max interations (100000) reached.")
        else msg<-paste(msg,"'convergence' code=",S$convergence)
        warning(msg)
        return(NULL)
        }

    # Pull the parameters out of the solution list
    theta <- S$par
    ELR <- theta[1L]
    thetaG <- theta[seq.int(length = G@np, to = np)]
    
    # Calculate the sigma2 "scaling parameter"
    sigma2 <- workarea$sigma2 <- LL.ODP.sigma2(workarea)

    # AY DETAIL LEVEL

    # Expected value of reserves
    R <- R.CapeCod(theta, Premium, G, CurrentAge.to, maxage, workarea)

    # PROCESS RISK OF RESERVES
    gammar2 <- R * sigma2
    gammar <- sqrt(gammar2)

    # PARAMETER RISK OF RESERVES

    # Calculate the Fisher Information matrix = matrix of
    #   2nd partial derivatives of the LL fcn w.r.t. all parameters
    workarea$FI <- FI <- d2LL.ODPdt2(S$par, MU.CapeCod, G, workarea)

    # Calculate the gradient matrix, dR = matrix of 1st partial derivatives
    #   for every origin year w.r.t. every parameter
    dR <- dfdx(R.CapeCod, theta, Premium, G, CurrentAge.to, maxage, workarea)
    
    # Delta Method => approx var/cov matrix of reserves
    VCOV.R <- -workarea$sigma2*t(dR)%*%solve(FI,dR)

    # Origin year parameter risk estimates come from diagonal entries
    # Parameter risk for sum over all origin years = sum  over
    #   entire matrix (see below).
    deltar2 <- diag(VCOV.R)
    deltar  <- sqrt(deltar2)

    # Total Risk = process risk + parameter risk
    totalr2 <- gammar2 + deltar2
    totalr  <- sqrt(totalr2)

    # AY Total row
    CurrentValue.sum <- sum(CurrentValue)
    Premium.sum <- sum(Premium)
    R.sum <- sum(R)
    gammar2.sum <- sum(gammar2)
    gammar.sum = sqrt(gammar2.sum)
    deltar2.sum <- sum(VCOV.R)
    deltar.sum <- sqrt(deltar2.sum)
    totalr2.sum <- gammar2.sum + deltar2.sum
    totalr.sum = sqrt(totalr2.sum)

    # Form "report table" as on p. 65 of paper
    Table65 <- data.frame(
        Origin = c(origins, "Total"),
        CurrentValue = c(CurrentValue, CurrentValue.sum),
        EstimatedReserves = c(R, R.sum),
        ProcessSE = c(gammar, gammar.sum),
        ProcessCV = 100*round(c(gammar, gammar.sum) / c(R, R.sum), 3),
        ParameterSE = c(deltar, deltar.sum),
        ParameterCV = 100*round(c(deltar, deltar.sum) / c(R, R.sum), 3),
        TotalSE = c(totalr, totalr.sum),
        TotalCV = 100*round(c(totalr, totalr.sum) / c(R, R.sum), 3),
        stringsAsFactors = FALSE
        )

    g <- G(c(maxage, CurrentAge.to), thetaG)
    gInf <- G(c(Inf, CurrentAge.to), thetaG)
    Table68 <- data.frame(
        Origin = c("", origins, "Total"),
        Premium = c(NA, Premium, Premium.sum),
        CurrentAge = c(maxage.traditional, CurrentAge, NA),
        AgeUsed = c(maxage, CurrentAge.to, NA),
        GrowthFunction = c(g, NA),
        TruncatedGrowth = G(maxage, thetaG) - c(g, NA),
        PremiumxELR = ELR * c(NA, Premium, Premium.sum),
        EstimatedReserves = c(NA, R, R.sum),
        stringsAsFactors = FALSE
        )

    structure(
        list(
            method = "Cape Cod",
            growthFunction = G@name,
            Table65=Table65,
            Table68=Table68,
            par=c(unclass(S$par)),
            sigma2=c(unclass(sigma2)),
            dR=dR,
            origin = workarea$origin,
            age = workarea$dev,
            fitted = workarea$mu,
            residuals = workarea$residuals,
            stdresid = workarea$residuals/sqrt(sigma2*workarea$mu),
            FI=FI
            ),
        class=c("clark","list")
        )
    }

print.clark <- function(x, ...) 
    print.data.frame(x$Table65, row.names=FALSE, ...)

plot.clark <- function(x, ...) {
    # We will plot the residuals as functions of
    #   1. origin
    #   2. age (at end of development period)
    #   3. fitted value (observe heteroscedasticity)
    # The y-values of the plots are the x$stdresid's
    # 4th plot shows results of normality test
    par(mfrow=c(2,2),         # 4 plots on one page
        oma = c(0, 0, 5, 0))  # outer margins necessary for page title
    #
    plot(x$origin,
        x$stdresid,ylab="standardized residuals",
        xlab="Origin",
        main="By Origin")
    z <- lm(x$stdresid~x$origin)
    abline(z, col="blue")
    origin <- x$origin
    z <- loess(x$stdresid~origin)
    y <- predict(z, data.frame(origin = (xseq<-seq(min(origin), max(origin), length.out=50))))
    lines(xseq, y, col="red", lty="dashed")
    #
    plot(x$age,
        x$stdresid, 
        xlab="Age",
        ylab="standardized residuals",
        main="By Projected Age")
    age <- as.numeric(x$age)
    z <- lm(x$stdresid~age)
    abline(z, col="blue")
    z <- loess(x$stdresid~age)
    y <- predict(z, data.frame(age = (xseq<-seq(min(age), max(age), length.out=50))))
    lines(xseq,y, col="red", lty="dashed")
    #
    plot(x$fitted,
        x$stdresid,ylab="standardized residuals",
        xlab="Expected Value",
        main="By Fitted Value")
    z <- lm(x$stdresid~x$fitted)
    abline(z, col="blue")
    fitted <- x$fitted
    z <- loess(x$stdresid~fitted)
    y <- predict(z, data.frame(fitted = (xseq<-seq(min(fitted), max(fitted), length.out=50))))
    lines(xseq,y, col="red", lty="dashed")
    # Normality test
    qqnorm(x$stdresid)
    qqline(x$stdresid, col="blue")
    N <- min(length(x$stdresid),5000) # 5000=shapiro.test max sample size
    shap.p <- shapiro.test(sample(x$stdresid,N))
    shap.p.value <- round(shap.p$p.value,5)
    text(-2,2.0, paste("Shapiro-Wilk p.value = ", shap.p.value, ".", sep=""), cex=.7, adj=c(0,0))
    text(-2,1.5, paste(ifelse(shap.p.value<.05,"Should reject",
                                              "Cannot reject"),
                      "ODP hypothesis"), cex=.7, adj=c(0,0))
    text(-2,1.0,expression(paste("at ",alpha," = .05 level.",sep="")), cex=.7, adj=c(0,0))

    # Finally, the overall title of the page of plots    
    mtext(
        paste(
            "Clark Standardized Residuals\nMethod: ", 
            x$method, 
            "; Growth function: ", 
            x$growthFunction,
            sep=""), 
        outer = TRUE, 
        cex = 1.5
        )
    par(mfrow=c(1,1))
    }

# FUNCTIONS AND FUNCTION CLASSES

# Function Classes

# FUNCTION CLASS WITH DERIVATIVES

# First, a function-or-NULL virtual class
setClassUnion("funcNull", c("function","NULL"))

# Functions stemming from this class have one argument, x
setClass("dfunction", 
    # By issuing the name of the instance of this class, you will get
    #   the function as defined when the instance was created.
    contains = "function",
    representation = representation(
        # partial derivative function, vector of length = length(x)
        dfdx = "funcNull",
        # 2nd partial derivative function, vector of length = length(x)
        d2fdx2 = "funcNull"
        )
    )

# Generics to return the 1st & 2nd partial derivatives 
setGeneric("dfdx", function(f, ...) standardGeneric("dfdx"))
setMethod("dfdx", "dfunction", function(f, ...) f@dfdx(...))
setGeneric("d2fdx2", function(f, ...) standardGeneric("d2fdx2"))
setMethod("d2fdx2", "dfunction", function(f, ...) f@d2fdx2(...))

# Generic "change-in-function" method
setGeneric("del", function(f, from, to, ...) standardGeneric("del"))
setMethod("del", "function", function(f, from, to, ...) f(to, ...) - f(from, ...))

# GROWTH FUNCTION CLASS

# Functions stemming from this class have two arguments:
#   x = age/lag/time at which the function will be evaluated
#   theta = vector of parameters
# Growth function's derivatives will be with respect to 2nd argument --
#   't' stands for 'theta' = vector of parameters -- which is different
#   from dfunction class whose derivatives are w.r.t. 1st argument
setClass("GrowthFunction", 
    # By issuing the name of the instance of this class, you will get
    #   the function as defined when the instance was created.
    contains = "function",
    representation = representation(
        name = "character", 
        # number of parameters (length of theta)
        np = "integer", 
        # function to return initial parameters before running optim
        initialGuess = "funcNull", 
        # partial derivative function, vector of length np
        dGdt = "funcNull",
        # 2nd partial derivative function, np x np matrix
        d2Gdt2 = "funcNull"
        )
    )

# LOGLOGISTIC FUNCTION

G.loglogistic <- function(x, theta) {
    if (any(theta <= 0)) return(rep(0, length(x)))
    om <- unname(theta[1L])
    th <- unname(theta[2L])
    y <- 1/(1+(th/x)^om)
    y[is.na(y)] <- 0
    y
    }

dG.loglogisticdtheta <- function(x, theta) {
    if (any(theta <= 0)) return(
        if (length(x) > 1L) array(0, dim = c(2L, length(x)))
        else numeric(2L)
        )
    om <- theta[1L]
    th <- theta[2L]
    thx <- th/x
    y <- 1/(1+(thx)^om)
    xth <- 1/thx
    dydom <- y / (xth^om + 1) * log(xth)
    tom <- th^om
    dydth <- y * tom / (x^om + tom) * (-om / th)
    # Pancake adds new dimension in row direction. Can create column matrix.
    dtheta <- rbind(dydom, dydth)
    # If x <= 0, Inf, derivative = 0 by definition  (log returns NA)
    dtheta[is.na(dtheta)] <- 0
    rownames(dtheta) <- names(theta)
    dtheta
    }

d2G.loglogisticdtheta2 <- function(x, theta) {
    if (any(theta <= 0)) return(
        array(0, dim = if (length(x) > 1L) c(4L, length(x)) else c(2L, 2L))
        )
    om <- theta[1L]
    th <- theta[2L]
    thx <- th/x
    y <- 1/(1+(thx)^om)
    tom <- th^om
    xth <- 1/thx

    dydom <- y / (xth^om + 1) * log(xth)
    dydth <- y * tom / (x^om + tom) * (-om / th)

    d2ydom2 <- dydom * log(xth) * (1 - 2 * y)
    d2ydth2 <- -dydth / th * (1 + om * (1 - 2 * y))
    d2ydomdth <- 1/om * dydth * (1 + om * log(xth) * (1 - 2*y))

    ndx <- x<=0 | is.infinite(x)
    d2ydom2[ndx] <- 0
    d2ydth2[ndx] <- 0
    d2ydomdth[ndx] <- 0
    
    if (length(x)>1L) structure(
        # Create a matrix where each column holds an observation's
        #   d2 matrix "stretched out" into a vector of length 4
        rbind(d2ydom2, d2ydomdth, d2ydomdth, d2ydth2),
        dim = c(4L, length=length(x)),
        dimnames = list(c(names(theta), rep(NA, 2L)), names(x))
        )
    else array(
        c(d2ydom2, d2ydomdth, d2ydomdth, d2ydth2),
        dim = c(2L, 2L),
        dimnames=list(names(theta), names(theta))
        )
    }

loglogistic <- new("GrowthFunction", 
    G.loglogistic,
    name = "loglogistic",
    np = 2L,
    initialGuess = function(env) c(
        omega = 2, 
        theta = median(env$Age.to, na.rm=TRUE)
        ),
    dGdt = dG.loglogisticdtheta,
    d2Gdt2 = d2G.loglogisticdtheta2
    )

# WEIBULL FUNCTION

G.weibull <- function(x, theta) {
    if (any(theta <= 0)) return(rep(0, length(x)))
    om <- unname(theta[1L])
    th <- unname(theta[2L])
    y <- 1 - exp(-(x/th)^om)
    y[is.na(y)] <- 0
    y
    }

dG.weibulldtheta <- function(x, theta) {
    if (any(theta <= 0)) return(
        if (length(x) > 1L) array(0, dim = c(2L, length(x)))
        else numeric(2L)
        )
    om <- theta[1L]
    th <- theta[2L]
    xth <- x / th
    u   <- xth^om
    # dydom <- exp(-u) * u * log(u) / om
    # dydth <- -exp(-u) * u * om / th
    v <- exp(-u) * u
    dydom <- v * log(xth)
    dydth <- -v * om / th
    # Pancake adds new dimension in row direction. Can create column matrix.
    dtheta <- rbind(dydom, dydth)
    # If x <= 0, Inf, derivative = 0 by definition  (log returns NA)
    dtheta[is.na(dtheta)] <- 0
    rownames(dtheta) <- names(theta)
    dtheta
    }

d2G.weibulldtheta2 <- function(x, theta) {
    if (any(theta <= 0)) return(
        array(0, dim = if (length(x) > 1L) c(4L, length(x)) else c(2L, 2L))
        )
    om <- theta[1L]
    th <- theta[2L]
    xth  <- x/th
    u    <- xth^om
    logu <- log(u)
    u1   <- 1 - u
    v    <- exp(-u) * u
    dydom <- v * log(xth)
    dydthpositive <- v * om / th

    d2ydom2 <- 2 * dydom * u1
    d2ydth2 <- dydthpositive * (1 + om * u1) / th
    d2ydomdth <- -v * (1 + logu * u1) / th

    ndx <- x<=0 | is.infinite(x)
    d2ydom2[ndx] <- 0
    d2ydth2[ndx] <- 0
    d2ydomdth[ndx] <- 0
    
    if (length(x)>1L) structure(
        # Create a matrix where each column holds an observation's
        #   d2 matrix "stretched out" into a vector of length 4
        rbind(d2ydom2, d2ydomdth, d2ydomdth, d2ydth2),
        dim = c(4L, length=length(x)),
        dimnames = list(c(names(theta), rep(NA, 2L)), names(x))
        )
    else array(
        c(d2ydom2, d2ydomdth, d2ydomdth, d2ydth2),
        dim = c(2L, 2L),
        dimnames=list(names(theta), names(theta))
        )
    }

weibull <- new("GrowthFunction", 
    G.weibull,
    name = "weibull",
    np = 2L,
    initialGuess = function(env) c(
        omega = 2, 
        theta = median(env$Age.to, na.rm=TRUE)
        ),
    dGdt = dG.weibulldtheta,
    d2Gdt2 = d2G.weibulldtheta2
    )

# LOGLIKELIHOOD FUNCTION UNDER ODP ASSUMPTION

LL.ODP <- function(theta, MU, G, workarea) {
    # Calculate the expected value of all observations, store in workarea.
    MU(theta, G, workarea)
    # Do ODP-model calc for all observations, add them up.
    sum(workarea$value * log(workarea$mu) - workarea$mu)
    }

dLL.ODPdt <- function(theta, MU, G, workarea) {
    # Calculate the gradient for all observations, store in workarea.
    #   Creates workarea$dmudt
    dfdx(MU, theta, G, workarea)
    # Must return a vector for optim, I believe, rt a column matrix
    c(workarea$dmudt %*% (workarea$value/workarea$mu-1))
    }

d2LL.ODPdt2 <- function(theta, MU, G, workarea) {
    # Calculate all the 2nd derivatives of MU
    d2fdx2(MU, theta, G, workarea)
    # Calculate the hessian matrix for every observation, store as a 
    #   column in a matrix with # cols = # obs, add up all the 
    #   matrices, and reshape.
    # For each obs, the hessian matrix is the matrix of second partial 
    #   derivatives of LL. The first term is the product of the 
    #   ("stretched out") matrix of second partial derivatives of the 
    #   MU function and the quantity cit/mu-1 (paper's notation).
    #   The second term is the outer product of (not the square of --
    #   we need a matrix not a vector) two first partial derivatives
    #   of the MU function times the quantity cit/mu^2.
    structure(
        rowSums(
            sapply(
                seq.int(length=workarea$nobs), 
                function(i)
                    workarea$d2mudt2[,i] * rep(workarea$value[i]/unname(workarea$mu[i]) - 1, each=length(theta)^2) -
                    workarea$value[i] / (workarea$mu[i]*workarea$mu[i]) *
                    c(outer(workarea$dmudt[,i], workarea$dmudt[,i], "*"))
                )
            ),
        dim=c(length(theta), length(theta))
        )
    }

LL.ODP.sigma2 <- function(workarea) {
    workarea$residuals <- workarea$value-workarea$mu
    return(workarea$sigma2 <- 
        sum(workarea$residuals^2/workarea$mu) / (workarea$nobs-workarea$np)
        )
    }
    
# EXPECTED VALUE (MU) FUNCTIONS

# LDF METHOD

MU.LDF <- new("dfunction",
    # theta    = vector of parameters: U followed by omega and theta
    # G        = growth function (eg, loglogistic or weibull)
    # workarea = environment where intermediate values are stored
    function(theta, G, workarea) {
        Useq        <- seq.int(length=length(theta)-G@np)
        thetaU      <- theta[Useq]
        thetaG      <- theta[seq.int(length=G@np, to=length(theta))]
        workarea$io <- outer(Useq, workarea$origin, `==`)
        workarea$u  <- thetaU[workarea$origin] ## or thetaU %*% workarea$io
        workarea$delG <- del(G, workarea$Age.from, workarea$Age.to, thetaG)
        workarea$mu <- workarea$u * workarea$delG
        },
    dfdx = function(theta, G, workarea) {
        thetaU      <- theta[seq.int(length=length(theta)-G@np)]
        thetaG      <- theta[seq.int(length=G@np, to=length(theta))]
        workarea$deldG <- del(G@dGdt, workarea$Age.from, workarea$Age.to, thetaG)
        # Pancake
        workarea$dmudt <- rbind(
            workarea$io * rep(workarea$delG, each=length(thetaU)),
            workarea$deldG * rep(workarea$u, each=G@np)
            )
        },
    d2fdx2 = function(theta, G, workarea) {
        # Separately calculate the three matrices for each obs:
        #   d2MUdthetaU2, d2MUdthetaUdthetaG, dtMUdthetaG2
        # Bind them into one hessian matrix for each obs.
        if (!exists("deldG", env=workarea)) stop("gr=NULL? Must run the derivatives.")
        K <- length(theta) - G@np
        K1 <- seq.int(K)
        thetaU      <- theta[K1]
        thetaG      <- theta[seq.int(length = G@np, to = length(theta))]
        
        # The result of the following will be a matrix with a column for 
        #   every observation. Each column is the hessian matrix for
        #   that observation, automatically "stretched out" into a 
        #   column vector, the default behavior of the sapply function.
        U2 <- array(0, c(K, K))
        workarea$d2mudt2 <- sapply(seq.int(workarea$nobs), function(i) {
            # Cross partials of thetaU and thetaG
            crossPartials.UG <- structure(workarea$deldG[,i], dim = c(1L, G@np)) %x% workarea$io[K1, i]
            # Cross partials of thetaG and thetaG
            crossPartials.GG <- workarea$u[i] * del(G@d2Gdt2, workarea$Age.from[i], workarea$Age.to[i], thetaG)
            rbind(cbind(U2, crossPartials.UG),
                  cbind(t(crossPartials.UG), crossPartials.GG))
            })
        }
    )

# CAPE COD METHOD

MU.CapeCod <- new("dfunction",
    # theta    = vector of parameters: U followed by omega and theta
    # G        = growth function (eg, loglogistic or weibull)
    # workarea = environment where intermediate values are stored
    function(theta, G, workarea) {
        ELR         <- theta[1L]
        thetaG      <- theta[seq.int(length=G@np, to=length(theta))]
        workarea$u  <- ELR * workarea$P
        workarea$delG <- del(G, workarea$Age.from, workarea$Age.to, thetaG)
        workarea$mu <- workarea$u * workarea$delG
        },
    dfdx = function(theta, G, workarea) {
        ELR         <- theta[1L]
        thetaG      <- theta[seq.int(length=G@np, to=length(theta))]
        workarea$deldG <- del(G@dGdt, workarea$Age.from, workarea$Age.to, thetaG)
        # Pancake
        workarea$dmudt <- rbind(
            workarea$P * workarea$delG,
            workarea$deldG * rep(workarea$u, each=G@np)
            )
        },
    d2fdx2 = function(theta, G, workarea) {
        # Separately calculate the three matrices for each obs:
        #   d2MUdthetaU2, d2MUdthetaUdthetaG, dtMUdthetaG2
        # Bind them into one hessian matrix for each obs.
        if (!exists("deldG", env=workarea)) stop("gr=NULL? Must run the derivatives.")
        thetaG      <- theta[seq.int(length = G@np, to = length(theta))]
        # The result of the following will be a matrix with a column for 
        #   every observation. Each column is the hessian matrix for
        #   that observation, automatically "stretched out" into a 
        #   column vector, the default behavior of the sapply function.
        workarea$d2mudt2 <- sapply(seq.int(workarea$nobs), function(i) {
            # Cross partials of thetaU and thetaG
            crossPartials.UG <- structure(workarea$deldG[,i], dim = c(1L, G@np)) * workarea$P[i]
            # Cross partials of thetaG and thetaG
            crossPartials.GG <- workarea$u[i] * 
                                del(G@d2Gdt2, workarea$Age.from[i], workarea$Age.to[i], thetaG)
            rbind(c(0, crossPartials.UG),
                  cbind(t(crossPartials.UG), crossPartials.GG))
            })
        }
    )

# RESERVE FUNCTIONS
#   Functions to calculate the "reserve" (future development)
#       and partial derivatives under the two methods.

R.LDF <- new("dfunction",
    # theta    = vector of parameters: U followed by omega and theta
    # G        = growth function (eg, loglogistic or weibull)
    # workarea = environment where intermediate values are stored
    function(theta, G, from, to, workarea) {
        K      <- length(theta) - G@np
        K1     <- seq.int(K)
        thetaU <- theta[K1]
        thetaG <- theta[seq.int(length = G@np, to = length(theta))]
        thetaU * del(G, from, to, thetaG )
        },
    dfdx = function(theta, G, from, to, workarea) {
        # R is a vector valued function (think "column matrix").
        # Taking the partials adds a new dimension ... put at beginning.
        # So dR is a matrix valued function where nrows=length(theta)
        #   and ncol = nrow(R)
        K      <- length(theta) - G@np
        K1     <- seq.int(K)
        thetaU <- theta[K1]
        thetaG <- theta[seq.int(length = G@np, to = length(theta))]
        # dR
        # |A 0 0 . 0 | A: partial of R_ay wrt thetaU
        # |0 A 0 . . | B: partial of R_ay wrt thetaG
        # |0 0 .   . |
        # |. . . A 0 |
        # |0 0 . 0 A |
        # |B . . . B |
        ##A <- diag(del(G, from, to, thetaG))
        ##B <- del(G@dGdt, from, to, thetaG) %*% thetaU
        ##  del(G@dGdt, from, to, thetaG) is a nrow(Table65) x G@np matrix --
        ##      a row for each origin year in Table65, a column for each
        ##      parameter in the new derivative dimension
        ##  thetaU is a vector of estimated ultimates corresponding to
        ##      each origin year being projected; it has length=nrow(Table65)
        rbind(
            diag(del(G, from, to, thetaG)),
            # c to remove dim
            (c(G@dGdt(to, thetaG)) - G@dGdt(from, thetaG)) * rep(thetaU, each=G@np)
            )
        },
    d2fdx2 = NULL # not needed at this point
    )

R.CapeCod <- new("dfunction",
    # theta    = vector of parameters: U followed by omega and theta
    # G        = growth function (eg, loglogistic or weibull)
    # workarea = environment where intermediate values are stored
    function(theta, Premium, G, from, to, workarea) {
        ELR    <- theta[1L]
        thetaG <- theta[seq.int(length = G@np, to = length(theta))]
        ELR * Premium * del(G, from, to, thetaG )
        },
    dfdx = function(theta, Premium, G, from, to, workarea) {
        # R is a vector valued function (think "column matrix").
        # Taking the partials adds a new dimension ... put at beginning.
        # So dR is a matrix valued function where nrows=length(theta)
        #   and ncol = nrow(R)
        ELR    <- theta[1L]
        thetaG <- theta[seq.int(length = G@np, to = length(theta))]
        # dR
        # |A A A . A | A: partial of R_ay wrt ELR
        # |B . . . B | B: partial of R_ay wrt thetaG
        # |B . . . B | 
        ##  Columns ~ origin years, Rows ~ parameters
        rbind(
            ELR=Premium * del(G, from, to, thetaG),
            del(G@dGdt, from, rep(to, length(from)), thetaG) * rep(ELR * Premium, each=G@np)
            )
        },
    d2fdx2 = NULL # not needed at this point
    )


