# Homework 6
# 3/17/2021
# Sandra Nnadi

# Assign to the variable n_dims a single random integer between 3 and 10.
n_dims <- sample(3:10,1)
n_dims
# n_dims = 3

# Create a vector of consecutive integers from 1 to n_dims2
my_vec <- c(1:n_dims^2)
print(my_vec)
# my_vec = 1 2 3 4 5 6 7 8 9

# Use the sample function to randomly reshuffle these values
my_vec2 <- sample(my_vec)
print(my_vec2)
# my_vec2 = 7 5 4 8 1 2 3 6 9
my_vec2 <- c(7, 5, 4, 8, 1, 2, 3, 6, 9)
# create a square matrix with these elements
m <- matrix(data = my_vec2,nrow = 3,ncol = 3)
print(m)
#       [,1] [,2] [,3]
# [1,]    7    8    3
# [2,]    5    1    6
# [3,]    4    2    9

# find a function in r to transpose the matrix
m_transpose <- t(m)
print(m_transpose)
#        [,1] [,2] [,3]
# [1,]    7    5    4
# [2,]    8    1    2
# [3,]    3    6    9

# calculate the sum and the mean of the elements in the first row and the last row.
sum(m_transpose[1,]) # 16
mean(m_transpose[1,]) # 5.333333
sum(m_transpose[3,]) # 18
mean(m_transpose[3,]) # 6

# read about the eigen() function and use it on your matrix
eigen(m_transpose, symmetric = TRUE,only.values = FALSE, EISPACK = FALSE)

# eigen() decomposition
# $values
# [1] 17.091561  5.286872 -5.378433

# $vectors
#          [,1]       [,2]       [,3]
# [1,] -0.5959505 -0.6421609  0.4821540
# [2,] -0.5233695 -0.1447774 -0.8397165
# [3,] -0.6090381  0.7527742  0.2498073

# look carefully at the elements of $values and $vectors. What kind of numbers are these?

# dig in with the typeof() function to figure out their type.

new_vec <- eigen(m_transpose, symmetric = TRUE,only.values = FALSE, EISPACK = FALSE)
print(new_vec)
typeof(new_vec) # list

# re-run code
n_dims <- sample(3:10,1)
n_dims
my_vec <- c(1:n_dims^2)
print(my_vec)
my_vec2 <- sample(my_vec)
print(my_vec2)
m <- matrix(data = my_vec2,nrow = 3,ncol = 3)
print(m)
m_transpose <- t(m)
print(m_transpose)
sum(m_transpose[1,])
mean(m_transpose[1,])
sum(m_transpose[3,])
mean(m_transpose[3,])
new_vec <- eigen(m_transpose, symmetric = TRUE,only.values = FALSE, EISPACK = FALSE)
print(new_vec)
typeof(new_vec)


# 2
# Create a List

my_list <- list(my_matrix=matrix(1:20,
                                 nrow=4,
                                 ncol=4,
                                 byrow= TRUE),
                                 my_logical=logical(length = 100),
                my_letters=letters[1:26],pi)
str(my_list)                                 
head(my_list)                                 
# create a new list

my_matrix <- matrix(1:20,
                 nrow=4,
                 ncol=4,
                 byrow= TRUE)

my_logical <- logical(length = 100)

my_letters <- letters[1:26]

new_list <- list(my_matrix[[2,2]],
                 my_logical[[2]],
                 my_letters[[2]])

print(new_list)
typeof(new_list) # "list"

my_vec <- unlist(c(my_matrix[[2,2]],
                   my_logical[[2]],
                   my_letters[[2]]))
print(my_vec) # "6" "FALSE" "b"
typeof(my_vec) # "character"


# 3
# create a data frame

my_unis <- runif(26,0,10)
print(my_unis)
my_letters <- LETTERS[26:1]
print(my_letters)
my_dframe <- data.frame(my_unis,my_letters)
head(my_dframe)

# for the first variable, use a single line of code in R to select 4 random rows and replace the numerical values in those rows with NA.

# for the first variable, write a single line of R code to identify which rows have the missing values.

# for the second variable, sort it in alphabetical order

# calculate the column mean for the first variable.
my_unis[sample(nrow(my_unis), 4), ]

my_unis2 <- replace(my_dframe,my_unis[c( )], "NA")
print(my_unis2)
