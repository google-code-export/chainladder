
```
> library(ChainLadder)
> RAA # example data set
      dev
origin    1     2     3     4     5     6     7     8     9    10
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
```
## Mack-Chain-Ladder ##
The Mack-chain-ladder model forecasts IBNR (Incurred But Not Reported) claims based
on a cumulative claims development triangle and estimates the standard error around it.

The `MackChainLadder` function is based on the following papers:

  * Thomas Mack. Distribution-free calculation of the standard error of chain ladder reserve estimates. Astin Bulletin. Vol. 23. No 2. 1993. pp.213:225.

  * Thomas Mack. The standard error of chain ladder reserve estimates: Recursive calculation and inclusion of a tail factor. Astin Bulletin. Vol. 29. No 2. 1999. pp.361:366.
```
> library(ChainLadder)
> M <- MackChainLadder(RAA, est.sigma="Mack")
> M
MackChainLadder(Triangle = RAA, est.sigma = "Mack")

     Latest Dev.To.Date Ultimate   IBNR Mack.S.E    CV
1981 18,834       1.000   18,834      0        0   NaN
1982 16,704       0.991   16,858    154      206 1.339
1983 23,466       0.974   24,083    617      623 1.010
1984 27,067       0.943   28,703  1,636      747 0.457
1985 26,180       0.905   28,927  2,747    1,469 0.535
1986 15,852       0.813   19,501  3,649    2,002 0.549
1987 12,314       0.694   17,749  5,435    2,209 0.406
1988 13,112       0.546   24,019 10,907    5,358 0.491
1989  5,395       0.336   16,045 10,650    6,333 0.595
1990  2,063       0.112   18,402 16,339   24,566 1.503

               Totals
Latest:    160,987.00
Ultimate:  213,122.23
IBNR:       52,135.23
Mack S.E.:  26,909.01
CV:              0.52
> plot(M)
```
![http://chainladder.googlecode.com/svn/trunk/inst/Images/MackChainLadder_RAA.png](http://chainladder.googlecode.com/svn/trunk/inst/Images/MackChainLadder_RAA.png)
```
> plot(M, lattice=TRUE)
```
![http://chainladder.googlecode.com/svn/trunk/inst/Images/MackLattice.png](http://chainladder.googlecode.com/svn/trunk/inst/Images/MackLattice.png)

## Bootstrap-Chain-Ladder ##
The `BootChainLadder` procedure provides a predictive distribution of reserves for a cumulative claims development triangle.

The `BootChainLadder` function is based on the paper by England and Verrall, while the plot uses ideas from Barnett and Zehnwirth:

  * England, PD and Verrall, RJ (2002). Stochastic Claims Reserving in General Insurance (with discussion), British Actuarial Journal 8, III.
  * Barnett and Zehnwirth. The need for diagnostic assessment of  bootstrap predictive models, Insureware technical report. 2007
```
> set.seed(1)
> B <- BootChainLadder(RAA)
> B
BootChainLadder(Triangle = RAA)

     Latest Mean Ultimate Mean IBNR SD IBNR IBNR 75% IBNR 95%
1981 18,834        18,834         0       0        0        0
1982 16,704        16,906       202     739      215    1,457
1983 23,466        24,168       702   1,312    1,160    3,364
1984 27,067        28,782     1,715   1,975    2,593    5,315
1985 26,180        28,898     2,718   2,167    3,872    6,743
1986 15,852        19,580     3,728   2,446    5,124    8,461
1987 12,314        17,728     5,414   3,128    7,235   11,426
1988 13,112        23,991    10,879   5,004   13,722   19,699
1989  5,395        16,331    10,936   6,087   14,161   21,830
1990  2,063        18,894    16,831  13,324   24,129   40,769

                 Totals
Latest:         160,987
Mean Ultimate:  214,112
Mean IBNR:       53,125
SD IBNR:         18,679
Total IBNR 75%:  65,003
Total IBNR 95%:  87,300

> plot(B)
```
![http://chainladder.googlecode.com/svn/trunk/inst/Images/BootChainLadder_RAA.png](http://chainladder.googlecode.com/svn/trunk/inst/Images/BootChainLadder_RAA.png)
## Munich-Chain-Ladder ##
The Munich-Chain-Ladder model forecasts IBNR claims based on a cumulative paid and
incurred claims triangle.

The `MunichChainLadder` function is based on the following paper:

  * Gerhard Quarg and Thomas Mack. Munich Chain Ladder. Blatter DGVFM 26, Munich, 2004.
```
> MCL <- MunichChainLadder(Paid = MCLpaid, Incurred = MCLincurred, est.sigmaP=0.1, est.sigmaI=0.1)
> MCL
MunichChainLadder(Paid = MCLpaid, Incurred = MCLincurred, est.sigmaP = 0.1, 
    est.sigmaI = 0.1)

  Latest Paid Latest Incurred Latest P/I Ratio Ult. Paid Ult. Incurred Ult. P/I Ratio
1       2,131           2,174            0.980     2,131         2,174          0.980
2       2,348           2,454            0.957     2,383         2,444          0.975
3       4,494           4,644            0.968     4,597         4,629          0.993
4       5,850           6,142            0.952     6,119         6,176          0.991
5       4,648           4,852            0.958     4,937         4,950          0.997
6       4,010           4,406            0.910     4,656         4,665          0.998
7       2,044           5,022            0.407     7,549         7,650          0.987

Totals
            Paid Incurred P/I Ratio
Latest:   25,525   29,694      0.86
Ultimate: 32,371   32,688      0.99

> plot(MCL)
```
![http://chainladder.googlecode.com/svn/trunk/inst/Images/MunichChainLadder_MCLpaid_MCLincurred.png](http://chainladder.googlecode.com/svn/trunk/inst/Images/MunichChainLadder_MCLpaid_MCLincurred.png)

## Utility functions ##
### Cumulative to incremental ###
```
> cum2incr(RAA)
        1    2    3    4    5    6    7   8   9  10
1981 5012 3257 2638  898 1734 2642 1828 599  54 172
1982  106 4179 1111 5270 3116 1817 -103 673 535  NA
1983 3410 5582 4881 2268 2594 3479  649 603  NA  NA
1984 5655 5900 4211 5500 2159 2658  984  NA  NA  NA
1985 1092 8473 6271 6333 3786  225   NA  NA  NA  NA
1986 1513 4932 5257 1233 2917   NA   NA  NA  NA  NA
1987  557 3463 6926 1368   NA   NA   NA  NA  NA  NA
1988 1351 5596 6165   NA   NA   NA   NA  NA  NA  NA
1989 3133 2262   NA   NA   NA   NA   NA  NA  NA  NA
1990 2063   NA   NA   NA   NA   NA   NA  NA  NA  NA
```
### Incremental to cumulative ###
```
> incr2cum(cum2incr(RAA))
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
```
### Triangle to data.frame ###
```
> as.data.frame(RAA, na.rm=TRUE)
   origin dev value
1    1981   1  5012
2    1982   1   106
3    1983   1  3410
4    1984   1  5655
5    1985   1  1092
6    1986   1  1513
7    1987   1   557
8    1988   1  1351
9    1989   1  3133
10   1990   1  2063
11   1981   2  8269
12   1982   2  4285
13   1983   2  8992
14   1984   2 11555
15   1985   2  9565
16   1986   2  6445
17   1987   2  4020
18   1988   2  6947
19   1989   2  5395
21   1981   3 10907
22   1982   3  5396
23   1983   3 13873
24   1984   3 15766
25   1985   3 15836
26   1986   3 11702
27   1987   3 10946
28   1988   3 13112
31   1981   4 11805
32   1982   4 10666
33   1983   4 16141
34   1984   4 21266
35   1985   4 22169
36   1986   4 12935
37   1987   4 12314
41   1981   5 13539
42   1982   5 13782
43   1983   5 18735
44   1984   5 23425
45   1985   5 25955
46   1986   5 15852
51   1981   6 16181
52   1982   6 15599
53   1983   6 22214
54   1984   6 26083
55   1985   6 26180
61   1981   7 18009
62   1982   7 15496
63   1983   7 22863
64   1984   7 27067
71   1981   8 18608
72   1982   8 16169
73   1983   8 23466
81   1981   9 18662
82   1982   9 16704
91   1981  10 18834
```
### Data frame to triangle ###
Suppose you have a data extract from your data base in R as a data.frame in X, which looks like this:
```
> head(X)
  Accident Year Dev. Year Incurred
1          1981         1     5012
2          1982         1      106
3          1983         1     3410
4          1984         1     5655
5          1985         1     1092
6          1986         1     1513
> 
```
Convert to triangle
```
> as.triangle(X, origin="Accident Year", dev="Dev. Year", value="Incurred")
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
```