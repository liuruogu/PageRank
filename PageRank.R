# This is an R script that demonstrates some of the basic
# calculations for the PageRank.

# a = alpha, should be less than 1
a <- 0.99

# Equally distribute centrality among all nodes (1/4):
initial <- rep(0.25,4)
print(initial)

# Vector filled with ones:
ones <- rep(1,4)
print(ones)

# Adjacency matrix:
A <- matrix(c(0,0,1,1,
              1,0,0,0,
              1,1,0,1,
              1,1,0,0),4,4, byrow= TRUE)

# Diagonal matrix with D_ii being the degree of i:
D <- matrix(c(3,0,0,0,
              0,2,0,0,
              0,0,1,0,
              0,0,0,2),4,4, byrow= TRUE)

# Invert D:
Di <- solve(D)
print(Di)

# Calcualte transition matrix:
transition <- A %*% Di
print(transition)

# Show the eigenvalues and eigenvectors of the transition matrix:
print(eigen(transition))

# Define a custom function to raise a matrix A to the power of n:
"%^%" <- function(A, n) if(n == 1) A else A %*% (A %^% (n-1))

# Estimate the PageRank by power iteration:
# One iteration:
print(transition %*% initial)

# Two iterations:
print(transition %*% transition %*% initial)

# Ten iterations:
print(transition %^% 10 %*% initial)

# Hundred iterations:
print(transition %^% 100 %*% initial)

# Thousand iterations:
print(transition %^% 1000 %*% initial)

# In comarison: calculate the accurate PageRank value:
print(D %*% solve(D - a * A) %*% initial)
print(0.01 * (D %*% solve(D - a * A) %*% initial))