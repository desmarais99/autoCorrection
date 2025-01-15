p100 <- function(x) print(x, n=100)
p20 <- function(x) print(x, n=20)
p10 <- function(x) print(x, n=10)
addNoise <- function(x) x+rnorm(length(x), 0, (max(x)-min(x))/50)
coin <- function(m, n=9) m[1:min(n,nrow(m)), 1:min(n,ncol(m))]
