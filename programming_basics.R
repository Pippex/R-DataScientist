library(dslabs)
data(murders)

murders

murders$abb


numbers <- seq(1:100)

numbers <- numbers^2

altman_plot <- function(x, y){
  plot(x-y,x+y)
}

altman_plot(seq(1:10), c(2,4,6,1,6,8,3,7,4,2))

computes_s_n <- function(n){
  result <- seq(1:n)
  sum(result^2)
}

all_s_n <- sapply(seq(1:100), computes_s_n)

plot(seq(1:100),all_s_n)

pro_computes_s_n <- function(n){
  n * (n+1) * (2*n+1)/6
}

all_pro_s_n <- sapply(seq(1:100), pro_computes_s_n)

all_s_n == all_pro_s_n

plot(seq(1:100), all_pro_s_n)

s_n <- vector("numeric", 25)

for(i in 1:length(s_n)){
  s_n[i] <- pro_computes_s_n(i)
}

s_n

