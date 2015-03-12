# Chain ladder as linear regression #

Barnet & Zehnwirth pointed out in there 2000 paper Best Estimates for Reserves. Proceedings of the CAS. Volume LXXXVII. Number 167 that chain ladder age-to-age factors can be regarded as coefficients of weighted linear regression with no intercept.

The following examples illustrate the concept. (Requires ChainLadder version 0.1.2-13)
```
> library(ChainLadder)
> RAA
        1     2     3     4     5     6     7     8     9    10
1981 5012  8269 10907 11805 13539 16181 18009 18608 18662 18834
1982  106  4285  5396 10666 13782 15599 15496 16169 16704    NA
1983 3410  8992 13873 16141 18735 22214 22863 23466    NA    NA
1984 5655 11555 15766 21266 23425 26083 27067    NA    NA    NA
1985 1092  9565 15836 22169 25955 26180    NA    NA    NA    NA
1986 1513  6445 11702 12935 15852    NA    NA    NA    NA    NA
1987  557  4020 10946 12314    NA    NA    NA    NA    NA    NA
1988 1351  6947 13112    NA    NA    NA    NA    NA    NA    NA
1989 3133  5395    NA    NA    NA    NA    NA    NA    NA    NA
1990 2063    NA    NA    NA    NA    NA    NA    NA    NA    NA
> ## Concept of different chain ladder age-to-age factors
> x <- RAA[1:9,1]
> y <- RAA[1:9,2]
> 
> weights <- RAA
> weights[!is.na(weights)] <- 1
> w <- weights[1:9,1]
> 
> F <- y/x
> ## wtd. average chain ladder age-to-age factors
> alpha <- 1
> delta <- 2-alpha
> ## Compare with Mack's 1999 paper:
> ## The standard error of chain ladder reserve estimates: Recursive calculation and inclusion of a tail factor. 
> ## Astin Bulletin. Vol. 29. No 2. 1999. pp.361:366
> sum(w*x^alpha*F)/sum(w*x^alpha)
[1] 2.999359
> lm(y~x + 0 ,weights=w/x^delta)

Call:
lm(formula = y ~ x + 0, weights = w/x^delta)

Coefficients:
    x  
2.999  

> summary(chainladder(RAA, weights=weights, delta=delta)$Models[[1]])$coef
  Estimate Std. Error  t value   Pr(>|t|)
x 2.999359   1.130203 2.653822 0.02908283
> 
> ## straight average age-to-age factors
> alpha <- 0
> delta <- 2 - alpha 
> sum(w*x^alpha*F)/sum(w*x^alpha)
[1] 8.2061
> lm(y~x + 0 ,weights=w/x^(2-alpha))

Call:
lm(formula = y ~ x + 0, weights = w/x^(2 - alpha))

Coefficients:
    x  
8.206  

> summary(chainladder(RAA, weights=weights, delta=delta)$Models[[1]])$coef
  Estimate Std. Error  t value   Pr(>|t|)
x   8.2061   4.113487 1.994925 0.08115167
> 
> ## regression age-to-age factors
> alpha=2
> delta <- 2-alpha
> sum(w*x^alpha*F)/sum(w*x^alpha)
[1] 2.217241
> lm(y~x + 0 ,weights=w/x^delta)

Call:
lm(formula = y ~ x + 0, weights = w/x^delta)

Coefficients:
    x  
2.217  

> summary(chainladder(RAA, weights=weights, delta=delta)$Models[[1]])$coef
  Estimate Std. Error  t value     Pr(>|t|)
x 2.217241  0.4112176 5.391893 0.0006522995
> 
> ## Change weights
> 
> weights[2,1] <- 0.5
> w <- weights[1:9,1] 
> 
> ## wtd. average chain ladder age-to-age factors
> alpha <- 1
> delta <- 2-alpha
> sum(w*x^alpha*F)/sum(w*x^alpha)
[1] 2.908271
> lm(y~x + 0 ,weights=w/x^delta)

Call:
lm(formula = y ~ x + 0, weights = w/x^delta)

Coefficients:
    x  
2.908  

> summary(chainladder(RAA, weights=weights, delta=delta)$Models[[1]])$coef
  Estimate Std. Error  t value   Pr(>|t|)
x 2.908271  0.9237467 3.148342 0.01363253
> 
> ## straight average age-to-age factors
> alpha <- 0
> delta <- 2 - alpha 
> sum(w*x^alpha*F)/sum(w*x^alpha)
[1] 6.310898
> lm(y~x + 0 ,weights=w/x^(2-alpha))

Call:
lm(formula = y ~ x + 0, weights = w/x^(2 - alpha))

Coefficients:
    x  
6.311  

> summary(chainladder(RAA, weights=weights, delta=delta)$Models[[1]])$coef
  Estimate Std. Error  t value   Pr(>|t|)
x 6.310898   3.136015 2.012394 0.07898456
> 
> ## regression age-to-age factors
> alpha=2
> delta <- 2-alpha
> sum(w*x^alpha*F)/sum(w*x^alpha)
[1] 2.214691
> lm(y~x + 0 ,weights=w/x^delta)

Call:
lm(formula = y ~ x + 0, weights = w/x^delta)

Coefficients:
    x  
2.215  

> summary(chainladder(RAA, weights=weights, delta=delta)$Models[[1]])$coef
  Estimate Std. Error  t value     Pr(>|t|)
x 2.214691  0.3961443 5.590616 0.0005158634
> 
> ## Model review
> CL0 <- chainladder(RAA, weights=weights, delta=0)
> ## age-to-age factors
> sapply(CL0$Models, function(x) summary(x)$coef["x","Estimate"])
[1] 2.214691 1.568952 1.260889 1.161972 1.099707 1.040534 1.032196 1.015888 1.009217
> ## f.se
> sapply(CL0$Models, function(x) summary(x)$coef["x","Std. Error"])
[1] 0.39614431 0.10878638 0.07063776 0.02307658 0.03610088 0.01984237 0.00471755 0.01494527        NaN
> ## sigma
> sapply(CL0$Models, function(x) summary(x)$sigma)
[1] 3634.3009 2429.0324 2339.2053  930.1536 1592.7979  813.0382  155.5481  368.4226       NaN
> 
> CL1 <- chainladder(RAA, weights=weights, delta=1)
> ## age-to-age factors
> sapply(CL1$Models, function(x) summary(x)$coef["x","Estimate"])
[1] 2.908271 1.623523 1.270888 1.171675 1.113385 1.041935 1.033264 1.016936 1.009217
> ## f.se
> sapply(CL1$Models, function(x) summary(x)$coef["x","Std. Error"])
[1] 0.923746665 0.135836119 0.090498216 0.025389927 0.035376679 0.022577813 0.004881918 0.015055851         NaN
> ## sigma
> sapply(CL1$Models, function(x) summary(x)$sigma)
[1] 136.314463  33.294538  26.295300   7.824960  10.928818   6.389042   1.159062   2.807704        NaN
> 
> CL2 <- chainladder(RAA, weights=weights, delta=2)
> ## age-to-age factors
> sapply(CL2$Models, function(x) summary(x)$coef["x","Estimate"])
[1] 6.310898 1.695894 1.314510 1.182926 1.126962 1.043328 1.034355 1.017995 1.009217
> ## f.se
> sapply(CL2$Models, function(x) summary(x)$coef["x","Std. Error"])
[1] 3.136015094 0.167616428 0.119849168 0.027269226 0.033389333 0.025122915 0.004953969 0.015093015         NaN
> ## sigma
> sapply(CL2$Models, function(x) summary(x)$sigma)
[1] 9.142976577 0.474090850 0.317091093 0.066795689 0.074660819 0.050245830 0.008580526 0.021344747         NaN
> 
> ## Forecasting
> 
> predict(CL0)
        1         2         3         4        5        6        7        8        9       10
1981 5012  8269.000 10907.000 11805.000 13539.00 16181.00 18009.00 18608.00 18662.00 18834.00
1982  106  4285.000  5396.000 10666.000 13782.00 15599.00 15496.00 16169.00 16704.00 16857.95
1983 3410  8992.000 13873.000 16141.000 18735.00 22214.00 22863.00 23466.00 23838.84 24058.55
1984 5655 11555.000 15766.000 21266.000 23425.00 26083.00 27067.00 27938.45 28382.35 28643.94
1985 1092  9565.000 15836.000 22169.000 25955.00 26180.00 27241.19 28118.25 28565.00 28828.28
1986 1513  6445.000 11702.000 12935.000 15852.00 17432.56 18139.18 18723.19 19020.67 19195.98
1987  557  4020.000 10946.000 12314.000 14308.52 15735.19 16373.00 16900.15 17168.66 17326.90
1988 1351  6947.000 13112.000 16532.776 19210.62 21126.06 21982.39 22690.14 23050.65 23263.10
1989 3133  5395.000  8464.494 10672.786 12401.48 13638.00 14190.80 14647.69 14880.42 15017.57
1990 2063  4568.907  7168.394  9038.549 10502.54 11549.72 12017.88 12404.81 12601.90 12718.05
> predict(CL1)
        1         2         3        4        5        6        7        8        9       10
1981 5012  8269.000 10907.000 11805.00 13539.00 16181.00 18009.00 18608.00 18662.00 18834.00
1982  106  4285.000  5396.000 10666.00 13782.00 15599.00 15496.00 16169.00 16704.00 16857.95
1983 3410  8992.000 13873.000 16141.00 18735.00 22214.00 22863.00 23466.00 23863.43 24083.37
1984 5655 11555.000 15766.000 21266.00 23425.00 26083.00 27067.00 27967.34 28441.01 28703.14
1985 1092  9565.000 15836.000 22169.00 25955.00 26180.00 27277.85 28185.21 28662.57 28926.74
1986 1513  6445.000 11702.000 12935.00 15852.00 17649.38 18389.50 19001.20 19323.01 19501.10
1987  557  4020.000 10946.000 12314.00 14428.00 16063.92 16737.55 17294.30 17587.21 17749.30
1988 1351  6947.000 13112.000 16663.88 19524.65 21738.45 22650.05 23403.47 23799.84 24019.19
1989 3133  5395.000  8758.905 11131.59 13042.60 14521.43 15130.38 15633.68 15898.45 16044.98
1990 2063  5999.762  9740.750 12379.40 14504.63 16149.24 16826.45 17386.16 17680.62 17843.58
> predict(CL2)
        1        2        3        4        5        6        7        8        9       10
1981 5012  8269.00 10907.00 11805.00 13539.00 16181.00 18009.00 18608.00 18662.00 18834.00
1982  106  4285.00  5396.00 10666.00 13782.00 15599.00 15496.00 16169.00 16704.00 16857.95
1983 3410  8992.00 13873.00 16141.00 18735.00 22214.00 22863.00 23466.00 23888.27 24108.44
1984 5655 11555.00 15766.00 21266.00 23425.00 26083.00 27067.00 27996.90 28500.70 28763.38
1985 1092  9565.00 15836.00 22169.00 25955.00 26180.00 27314.32 28252.71 28761.12 29026.20
1986 1513  6445.00 11702.00 12935.00 15852.00 17864.61 18638.64 19278.97 19625.90 19806.78
1987  557  4020.00 10946.00 12314.00 14566.55 16415.95 17127.21 17715.62 18034.42 18200.63
1988 1351  6947.00 13112.00 17235.86 20388.74 22977.34 23972.89 24796.49 25242.70 25475.36
1989 3133  5395.00  9149.35 12026.92 14226.95 16033.23 16727.91 17302.61 17613.97 17776.31
1990 2063 13019.38 22079.50 29023.73 34332.91 38691.89 40368.32 41755.19 42506.58 42898.34
> 
```