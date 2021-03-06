Assignment 2
================

In a famous investigation, Ladislaus Bortkiewicz collected data on the number of deaths each year from horse-kicks in the Prussian army. His data came from ten army corps, over a period of twenty years. We tabulate here the number of deaths j and the number of occurrences nj (i.e. the number of times an army corps had that many deaths in one year):

j nj

0 109 1 65 2 22 3 3 4 1 ≥ 5 0

Total 200 ///

We model the number of yearly deaths X1, . . . , X200 from horse-kicks in each army corps as independent Poisson(λ) random variables. Notice that there is no need to specify the year or army corps in the notation: since the random variables are i.i.d., we can just index the random variables using the integers from 1 to 200.

``` r
#put data into a vector
xtab <- c(109, 65, 22, 3, 1, 0)
xobs <- rep(0:5, xtab)
xobs
```

    ##   [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ##  [36] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ##  [71] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ## [106] 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## [141] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2
    ## [176] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 4

One of Bortkiewicz’s assistants suggests the following estimate for λ. Since P(X = 0; λ) = e^−λ, it follows that an estimate of λ is T(x) = − log p0(x), where, p0(x) is the proportion of the x’s that are equal to zero. Bortkiewicz prefers to use the ML estimate λ(x), the sample mean. In a sampling experiment, we contrast the behaviour of the estimators T(X) and λ(X) for two different values of λ, 1.0 and 2.0.

``` r
#Set each parameter as the sample mean of 1000 200-size samples from a poisson distribution, lambda1 being with rate 1, lambda2 with rate 2, as the parameter.
N <- 1000
n <- 200
lambda1 <- sapply(1:N, function(i) mean(rpois(n = n, lambda = 1)))
lambda2 <- sapply(1:N, function(i) mean(rpois(n = n, lambda = 2)))
#Set T1 and T2 as vectors of 1000 200-sized samples using the alternative estimate, with lambda = 1 and lambda = 2.
T1 <- sapply(1:N, function(i){
     x <- rpois(n = n, lambda = 1)
     p <- mean(x==0)
     -log(p)})
T2 <- sapply(1:N, function(i){
     x <- rpois(n = n, lambda = 2)
p <- mean(x==0)
     -log(p)})
#Plot the density function of the 4 samples
plot(density(lambda1), main = "Lambda estimators", xlim = c(0,3))
 lines(density(lambda2), col = "blue")
 lines(density(T1), col = "green")
 lines(density(T2), col = "red")
```

![](Assignment_2_files/figure-markdown_github/unnamed-chunk-2-1.png) Evidently, the lambda estimators (sample mean) are more concentrated around the true values for the parameters, so would be a preferred estimator.

///

We plot the log-likelihood function, the ML estimate, and the 95% CI implied by the Rule of Thumb

``` r
#define a log-likelihood function
ell <- function(L,x){
     est <- sum(x)*log(L) - length(x)*L
     return(-est)}
#plot
plot( seq(0.2,2, 0.01), -1*ell(seq(0.2,2, 0.01), xobs), xlab = "Lambda", ylab = "log-likelihood", main = "Rule of Thumb")
abline(v = 0.61)
abline(a = -1*ell(0.61,xobs) -2, b=0)
```

![](Assignment_2_files/figure-markdown_github/unnamed-chunk-3-1.png)
