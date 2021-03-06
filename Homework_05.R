# Homework 5
# 3/10/2021
# Sandra Nnadi

# 1a
x <- 1.1
a <- 2.2
b <- 3.3
z <- x^a^b
print(z)
# 3.61714

# 1b
z <- (x^a)^b
print(z)
# 1.997611

# 1c
z <- 3*x^3 + 2*x^2 + 1
print(z)
# 7.413

# 2a

c(seq(from=1,to=8,length=8),seq(from=7,to=1))
# 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1

# 2b
my_vec <- c(1,2,3,4,5)
rep(x=my_vec,times=my_vec)
# 1 2 2 3 3 3 4 4 4 4 5 5 5 5 5


# 2c
my_vec <- c(5,4,3,2,1)
rep(x=my_vec,times=my_vec)
# 5 5 5 5 5 4 4 4 4 3 3 3 2 2 1


# 3
runif(n=2,min = 100,max = 1000)
# 493.4485 814.8046
x <- 493.4485
y <- 814.8046
r <- sqrt(x^2 + y^2)
print(r)
# 952.5744
theta <- atan(y/x)
print(theta)
# 1.026267

pol_cor <- c(r,theta)
print(pol_cor)
# 952.574385   1.026267

# 4
queue <- c("sheep","fox","owl","ant")
print(queue)
# "sheep" "fox"   "owl" "ant"

# serpent arrives and gets in line
queue <- c(queue,"serpent")
print(queue)
# "sheep" "fox" "owl" "ant" "serpent"

# the sheep enters the ark
queue <- (queue[-1])
print(queue)
# "fox" "owl" "ant" "serpent"

# donkey arrives and talks his way to the front of the line
queue <- c("donkey",queue)
print(queue)
# "donkey" "fox" "owl" "ant" "serpent"

# the serpent gets impatient and leaves
queue <- (queue[-5])
print(queue)
# "donkey" "fox" "owl" "ant" 

# the owl gets bored and leaves;
queue <- (queue[-3])
print(queue)
# "donkey" "fox" "ant"

# the aphid arrives and the ant invites him to cut in line.
queue <- append(queue,"aphid",after = 2)
print(queue)

# determine the position of the aphid in the line.
which(queue == "aphid")
# 3

# 5
y <- as.integer(1:100)
head(y)
class(y)
is.integer(y)

# subset vector where integers are not divisible by 2
vec_a <- y[which(y %% 2 !=0)]
print(vec_a)
#  1  3  5  7  9 11 13 15
# 17 19 21 23 25 27 29 31
# 33 35 37 39 41 43 45 47
# 49 51 53 55 57 59 61 63
# 65 67 69 71 73 75 77 79
# 81 83 85 87 89 91 93 95
# 97 99

# subset vec_a where integers are not divisible by 3
vec_b <- vec_a[which(vec_a%%3 !=0)]
print(vec_b)
# 1  5  7 11 13 17 19 23
# 25 29 31 35 37 41 43 47
# 49 53 55 59 61 65 67 71
# 73 77 79 83 85 89 91 95
# 97


# subset vec_b where integers are not divisible by 7
vec_c <- vec_b[which(vec_b%%7 !=0)]

# print out vec_c to show integers 1 to 100 that are not divisible by 2, 3 and 7
print(vec_c)
# 1  5 11 13 17 19 23 25 29 31 37
# 41 43 47 53 55 59 61 65 67 71 73
# 79 83 85 89 95 97
