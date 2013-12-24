artR
====

Aligned Rank TransformR: An R port of Jacob Wobbrock's (C#) ARTool (https://depts.washington.edu/aimgroup/proj/art/)

Ported to R by Juan Sebastián Casallas in 2013

The Aligned Rank Transform (ART) allows using parametric tests on nonparametric data.
The ARTool is a C# program that does this transformation on an input dataset, and generates 
a new dataset with extra aligned and ranked columns. Its main limitation is that it's only
available for Windows (or, if you're courageous to Unix via Mono), and it involves an external
step in the data analysis worflow. Thus, I've created this R port to integrate the ART in my usual
*nix–R workflow. More details about ARTool and ART are presented in the ARTool website and in [2].

Installation
------------

This package can be installed using devtools: `devtools::install_github("casallas/artR")`

Usage
-----
As with ARTool, input for the `ART` function should be a "long format" data frame, whose first column
corresponds to the subject number and whose last column corresponds to the Y, or dependent variable;
all the columns in between correspond to the X, or independent variables.

The following example uses Table 1 from [1] provided in ARTool (https://depts.washington.edu/aimgroup/proj/art/ARTool.zip)
```
library(compare) # Compare equal
source("art.r", chdir=T)

higgins1 <- within(read.csv("Higgins1990-Table1.csv"), {
	Subject <- factor(Subject)
	Row <- factor(Row)
	Column <- factor(Column)
})

# Results given by ARTool
higgins1.artool <- within(read.csv("Higgins1990-Table1.art.csv"), {
  Subject <- factor(Subject)
  Row <- factor(Row)
  Column <- factor(Column)
})
# Both results are the same
compareEqual(higgins1.artool, higgins1.art)$detailedResult # All columns have the same values
```

The following ANOVA coincides with that of Table 4 in [1] (Row*Column interaction)
```
library(ez) # ezANOVA
ezANOVA(data=higgins1.art, dv=ART.Response.Row.x.Column, wid=Subject, between=.(Row, Column))
#       Effect DFn DFd          F         p p<.05
# 1        Row   2  27 0.11058723 0.8957116      
# 2     Column   2  27 0.08551222 0.9182895      
# 3 Row:Column   4  27 0.64166309 0.6374203      
```

ANOVA for individual Row and Column can also be computed
```
ezANOVA(data=higgins1.art, dv=ART.Response.Row, wid=Subject, between=.(Row, Column))
ezANOVA(data=higgins1.art, dv=ART.Response.Column, wid=Subject, between=.(Row, Column))
```

Differences with ARTool
-----------------------

ARTool and artR give very similar results, in terms of number of columns and values.
One minor difference is the name of the columns:

Columns |ARTool                |artR
--------|----------------------|-------------
aligned |"aligned(Y) for X"    |"aligned.Y.X"
        |"aligned(Y) for X1*X2"|"aligned.Y.X1.x.X2"
ART     |"ART(Y) for X"        |"ART.Y.X"
        |"ART(Y) for X1*X2"    |"ART.Y.X1.x.X2"

artR's column notation is shorter and allows using `aligned` and `ART` column names in R formulae, e.g. in `lmer` or `lm`.
For example Table 4 in [1] can also be generated using:
```
summary(aov(ART.Response.Row.x.Column ~ Row*Column, data=higgins1.art))
#             Df Sum Sq Mean Sq F value Pr(>F)
# Row          2     29   14.33   0.111  0.896
# Column       2     22   11.08   0.086  0.918
# Row:Column   4    333   83.17   0.642  0.637
# Residuals   27   3500  129.61
```

Although the expected data frame's header should have the form "Subject", "X1", "X2", ..., "Xn", "Y", it's easy
to transform any data frame to this format before calling ART:
```
df <- data.frame(X1=(1:100)^2, subj=1:100, Y=log(1:100), X2=sqrt(1:100))
ART(df[c("subj", "X1", "X2", "Y")])
```

References
----------

[1] Higgins, J. J., Blair, R. C. and Tashtoush, S. (1990). The aligned rank transform procedure. Proceedings of the Conference on Applied Statistics in Agriculture. Manhattan, Kansas: Kansas State University, pp. 185-195.

[2] Wobbrock, J. O., Findlater, L., Gergle, D., & Higgins, J. J. (2011, May). The aligned rank transform for nonparametric factorial analyses using only anova procedures. In Proceedings of the SIGCHI Conference on Human Factors in Computing Systems (pp. 143-146). ACM.
