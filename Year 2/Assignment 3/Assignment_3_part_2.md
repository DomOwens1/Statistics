Assignment 3, Part 2
================

An archer has shot 10 arrows at a target. The measured distance of the ten arrow tips from the centre of the target is, in cm. Let Xi model the distance from the centre of the target to the tip of arrow i. Suppose that all X iid∼ Weibull(2,√2σ), where the first argument is the Weibull ‘shape’ parameter, the second is the Weibull ‘scale’ parameter, and the unknown parameter σ is a measure of accuracy. We test the hypotheses h0: σ = 8 against h1: σ &gt; 8, and use the test statistic T(x), the sum of squares.

``` r
#declare observations
xobs <- c(14, 5, 7, 6, 5, 8, 10, 14, 18, 17)
#create test statistic
T <- sum(xobs^2)
T
```

    ## [1] 1304

``` r
#use 100 trials to simulate
 trials <- 1e3
 
 #generate simulations from rweibull
 rejections <- 0
 for (i in 1:trials) {
     x <- rweibull(10,2,8*sqrt(2))
     Ttrial <- sum(x^2)
     rejections <- rejections + (Ttrial >= T)
 }
 #return the p value from these simulations
power.est <- rejections/trials
power.est
```

    ## [1] 0.439

Our p value is greater than 0.05. so we have insufficient evidence to reject h0, and hence we believe the archer's accuracy is distributed according to the weibull(2, 4) function.

///

The critical region R = {x : T(x) ≥ 2009} has size approximately 5% for the hypothesis test above. We compute and plot a simulation-based approximation of the power function associated with this critical region for values of σ in the range \[6, 16\].

``` r
#Create a power function, input sigma
powerfn <- function(sigma){
     rejections <- 0
      for (i in 1:trials) {
     x <- rweibull(10,2,sigma*sqrt(2))
      Ttrial <- sum(x^2)
      rejections <- rejections + (Ttrial >= T)
  }
  power.est <- rejections/trials
  power.est
}
#iterate over the range [6,16]
powers <- sapply(seq(6,16,0.01), function(i)powerfn(i))

#Plot the result
plot( seq(6,16, 0.01),powers, xlab = "Sigma", ylab = "power", main = "Power Function")
```

![](Assignment_3_part_2_files/figure-markdown_github/unnamed-chunk-2-1.png)
