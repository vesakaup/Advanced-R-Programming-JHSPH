library(purrr)
library(ggplot2)
library(microbenchmark)
library(dplyr)

factorial_loop<-function(n){
        stopifnot(n>=0) # checking if the input is non-negative
        if (n==0){
                f<-1  #the value of 0! is 1, according to the convention for empty product. 
                print(f)
        } else{
                f<-1
                for(i in 1:n){
                        f<-f*i
                }
                f
        }
}

factorial_reduce <-function(n){
        stopifnot(n>=0)
        if (n==0){
                f<-1
                print(f)
                
        } else {
                reduce(c(1:n),`*`)
        }
}

factorial_func <- function(n) {     
        stopifnot(n >= 0)                ## if the factorial is not 0 or 1
        if (n == 0){                     
                f <- 1  
        } else {                          
                n * factorial_func(n-1)  ## the previous iteration of the problem is multiplied and 
                                        ## the result returned
        } 
}
fac_tbl <- c (1, rep(NA, 24))             ## Creates a simple table or a vector with 1 as a first element and then 
                                        ##    24 NAs
factorial_mem <- function(n) { 
        stopifnot(n >= 0) 
        if (n == 0){                     
                f <- 1       
        } else {                                
                if(!is.na(fac_tbl[n])){               ## checks whether the factorial of n is in fac_tbl-table
                        fac_tbl[n]                          ## ... and returns it if it is
                } else {                              ## Otherwise the function...
                        fac_tbl[n-1] <<- factorial_mem(n-1) ## ... recursively calculates and stores the factorial of n 
                        n * fac_tbl[n-1]                    ##      into the fac_tbl-table 
                }
        }
}


evaluate_perf <- function(n){
        factorial_perf <- microbenchmark(factorial_loop(n), 
                                         factorial_reduce(n),
                                         factorial_func(n),
                                         factorial_mem(n))
        factorial_perf
}
evaluate_perf(0)
evaluate_perf(1)
evaluate_perf(5)
evaluate_perf(12)
evaluate_perf(15)
evaluate_perf(0)
evaluate_perf(1:5)
evaluate_perf(1:10)
evaluate_perf(1:15)


autoplot(microbenchmark(evaluate_perf(12)))
                