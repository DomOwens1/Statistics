Assignment 1
================

A particular task takes a variable amount of time to perform, which is modelled using an Exponential(λ) random variable with λ unknown. In an experiment, n = 7 pairs of individuals are assigned to complete this task simultaneously and independently, so that their time taken can be modelled as (A1, B1),(A2, B2), . . . ,(An, Bn), where A1, . . . , An, B1, . . . , Bn are independent Exponential(λ) random variables. Each individual knows if they have been labeled as being in the A group or the B group. Technological limitations (or perhaps a flaw in the design of the experiment) make it impossible to observe realizations of A1, . . . , An, B1, . . . , Bn. Instead, the first individual in each pair to finish the task records how much longer the other individual takes to complete the task, resulting in the random variables Yi := Ai − Bi for i ∈ {1, . . . , n} (notice that Ai − Bi takes a negative value if Ai &lt; Bi , i.e. the person in group A finishes first). Unfortunately, observations (i.e. observed realizations) of Y1, . . . , Yn are also not obtained due to a systematic corruption of the data that adds some unknown constant µ to the value of each Yi . Hence, we model Xi := µ + Yi = µ + Ai − Bi for i ∈ {1, . . . , n} and after the experiment has concluded, we obtain an observation of X = (X1, . . . , Xn) as x = (92, −1120, −381, −262, −1229, 573, −19).

``` r
#register observation
x <- c(92, −1120, −381, −262, −1229, 573, −19)
```

We write a function dlaplace(x, mu, b, log.p = FALSE) that computes the probability density of the Laplace distribution, where the additional argument log.p is used to select either the density (log.p = FALSE), or the log-density (log.p = TRUE).

``` r
dlaplace <- function(x, mu, b, log.p = FALSE){
    if (log.p == FALSE){
     f <- (1/(2*b))*exp(abs(x-mu)/(-b))}
     
     if (log.p == TRUE){
     f <- log((1/(2*b))*exp(abs(x-mu)/(-b)))}
      f} 
```

We write a function that plots the log-likelihood for one value of b (given as an input argument of the function) and values of µ in the range \[−1300, 600\].

``` r
#create a likelihood function
ell2 <- function(mu, b, x){    logx<- dlaplace(x, mu, b, log.p=TRUE) }

#create the required plot
logplot <- function(b){
    logx <- numeric(length(x))
    for(i in 1:length(x)){
    if (-1300<= mu & mu <=600){ logx[i]<-ell2(mu,b,x[i])
    }
    }
    plot(x,logx,main = b)
}
```

We find the maximum likelihood estimate of θ using the optim function (using the L-BFGS-B method), and call the result theta.hat.

We write a function plaplace(x,mu,b,lower.tail=TRUE) to evaluate the probability distribution function, where the additional argument lower.tail is used to select either F(x; µ, b) (lower.tail = TRUE), or 1 − F(x; µ, b) (lower.tail = FALSE).

``` r
plaplace <-function(x,mu,b,lower.tail=TRUE){
         if(lower.tail==TRUE){ 
            f <- 0.5 +0.5*sign(x-mu)*(1-exp(-abs(x-mu)/b)) #Calculates CDF
             }
         if(lower.tail==FALSE){
            f<-     0.5 -0.5*sign(x-mu)*(1-exp(-abs(x-mu)/b)) #Calculates 1-CDF
         }
    f #prints result
}

#try it out
plaplace(x,3,5)
```

    ## [1] 0.99999999 0.00000000 0.00000000 0.00000000 0.00000000 1.00000000
    ## [7] 0.00613867
