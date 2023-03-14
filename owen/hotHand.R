rep <- 1e6
n <- 4
data <- array(sample(c(0,1), rep*n, replace=TRUE), c(rep,n))
prob <- rep(NA, rep)
for (i in 1:rep){
  heads1 <- data[i,1:(n-1)]==1
  heads2 <- data[i,2:n]==1
  prob[i] <- sum(heads1 & heads2)/sum(heads1)
}

print(mean(prob, na.rm=TRUE))



## this is the proportion of times you get a head, given you got one
## the previous time


## what about given that you got a streak of 2 heads instead, with n = 10

rep <- 1e6
n <- 10
streak_len <- 3
data <- array(sample(c(0,1), rep*n, replace=TRUE), c(rep,n))
prob <- rep(NA, rep)
for (i in 1:rep){
  ## identify sequences of length 2 
  heads <- data[i, ] == 1
  seqs <- sequence(rle(data[i, ])$lengths)
  length_1_seqs <- which(heads[1:n-1] & seqs[1:n-1] >= streak_len)
  length_2_seqs <- which(heads[2:n] & seqs[2:n] >= (streak_len + 1))
  prob[i] <- length(length_2_seqs)/length(length_1_seqs)
  # 
  # heads1 <- data[i,1:(n-1)]==1
  # heads2 <- data[i,2:n]==1
  # prob[i] <- sum(heads1 & heads2)/sum(heads1)
}

print(mean(prob, na.rm=TRUE))
## this calculates it correctly when streak_len = 1


sequence(rle(data[1, ])$lengths)

a <- rle(data[1, ])

rep(a$values, a$lengths)
