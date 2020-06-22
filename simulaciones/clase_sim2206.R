# 2 Estadísticos
# runif(3,-1,1) simula 3 uniformes(-1,1) independientes
# sort() las ordena 

X <- matrix(nrow = 1e5, ncol = 3)
for(i in 1:1e5){
    X[i, ] <- sort( runif(n = 3, min = -1, max = 1) )
}
# a) P(X(1) > -1/2, X(2) < 1/2)
mean(X[,1] > -1/2 & X[,2] < 1/2)
# b) P(X(2) < 1/2, X(3) > 1/2)
mean(X[,2] < 1/2 & X[,3] > 1/2)

# 3 P(X(3) > X(1) + X(2))
X <- matrix(nrow = 1e5, ncol = 3)
for(i in 1:1e5){
    X[i, ] <- sort( runif(n = 3, min = 0, max = 1) )
}
mean(X[,3] > X[,1] + X[,2])

# 8. L, d
L <- 10
d <- 1.5
X <- matrix(nrow = 1e5, ncol = 3)
for(i in 1:1e5){
    X[i, ] <- sort( runif(n = 3, min = 0, max = L) )
}
mean( X[,2] > X[,1] + d & X[,3] > X[,2] + d )

# 7
n <- 10
i <- 3
j <- 8
X <- matrix(nrow = 1e5, ncol = n)
for(k in 1:1e5){
    X[k, ] <- sort( runif(n = n, min = 0, max = 1) )
}
x <- runif(n = 1e5, min = 0, max = 1)
# a) P(X > X(n))
mean(x > X[,n])
# b) P(X > X(1))
mean(x > X[,1])
# c) P(X(i) < X < X(j))
mean(X[,i] < x & x < X[,j])
