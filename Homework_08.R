# homework 8
# 8 April 2021
# Sandra Nnadi

# Prediction- inoculation of blueberry plants with microbial inoculum increases berry fruit mass
# Null hypothesis (Ho)- there is no significant difference between control and treated blueberry plants. Ho=0
# Alternative hypothesis (Ha)- there is a significant difference between control and treated plants. Ha</>0
# data should follow normal distribution
# X =independent/causal variable we manipulate
# Y = dependent/response variable we measure
# what is the signal of the effect of X on Y
# Predictor is discrete- Treatment groups
# Response is continuous - Berry mass
# I will use an ANOVA to see whether the average of response variable among treatment group differs or not.

#--------------------------------------------------
library(ggplot2)
# Create random dataset
bb_group <- 3 #number of treatment group
bb_name <- c("Control","CommInoc","LocInoc") #name of groups
bb_size <- c(17,15,16) #sample sizes
bb_mean <- c(22,45,32) #mean responses
bb_sd <- c(5,5,5) #standard deviation
ID <- 1:sum(bb_size) #creates unique ID

BerryMass <- c(rnorm(n=bb_size[1],mean = bb_mean[1],sd=bb_sd[1]),
               rnorm(n=bb_size[2],mean = bb_mean[2],sd=bb_sd[2]),
               rnorm(n=bb_size[3],mean = bb_mean[3],sd=bb_sd[3]))

Treatment <- rep(bb_name,bb_size)               

# Construct data frame
ano_data <- data.frame(ID,Treatment,BerryMass)
head(ano_data)

# Analyze data with ANOVA
ano_model <- aov(BerryMass~Treatment,data = ano_data)
print(ano_model)
z <- summary(ano_model)
print(z)

flat_out <- unlist(z)
ano_stats <- list(f_ratio <- unlist(z)[7],f_Pval <- unlist(z)[9])
print(ano_stats)

# Graph ANOVA data
ano_plot <- ggplot(ano_data)+
  aes(x=Treatment,y=BerryMass,fill=Treatment)+
  geom_boxplot()+
  labs(title = "Effect of Inoculation on Berry Mass")+
  theme_minimal()

print(ano_plot)

ggsave(filename = "BerryANOVAplot.pdf",plot = ano_plot,device = "pdf")


# Run analysis again with different set of random numbers
# Create another random dataset
bb_group <- 3 #number of treatment group
bb_name <- c("Control","CommInoc","LocInoc") #name of groups
bb_size <- c(17,15,16) #sample sizes
bb_mean <- c(22,45,32) #mean responses
bb_sd <- c(5,5,5) #standard deviation
ID <- 1:sum(bb_size) #creates unique ID

BerryMass <- c(rnorm(n=bb_size[1],mean = bb_mean[1],sd=bb_sd[1]),
               rnorm(n=bb_size[2],mean = bb_mean[2],sd=bb_sd[2]),
               rnorm(n=bb_size[3],mean = bb_mean[3],sd=bb_sd[3]))

Treatment <- rep(bb_name,bb_size)               

# Construct data frame
ano_data <- data.frame(ID,Treatment,BerryMass)
head(ano_data)

# Analyze data with ANOVA
ano_model <- aov(BerryMass~Treatment,data = ano_data)
print(ano_model)
z <- summary(ano_model)
print(z)

flat_out <- unlist(z)
ano_stats <- list(f_ratio <- unlist(z)[7],f_Pval <- unlist(z)[9])
print(ano_stats)

# Adjusting the means of the three groups
bb_group <- 3 #number of treatment group
bb_name <- c("Control","CommInoc","LocInoc") #name of groups
bb_size <- c(17,15,16) #sample sizes
bb_mean <- c(20,35,30) #mean responses
bb_sd <- c(5,5,5) #standard deviation
ID <- 1:sum(bb_size) #creates unique ID

BerryMass <- c(rnorm(n=bb_size[1],mean = bb_mean[1],sd=bb_sd[1]),
               rnorm(n=bb_size[2],mean = bb_mean[2],sd=bb_sd[2]),
               rnorm(n=bb_size[3],mean = bb_mean[3],sd=bb_sd[3]))

Treatment <- rep(bb_name,bb_size)                

# Construct data frame
ano_data2 <- data.frame(ID,Treatment,BerryMass)
head(ano_data2)

# Analyze data with ANOVA
ano_model2 <- aov(BerryMass~Treatment,data = ano_data2)
print(ano_model2)
str(ano_model2)
z <- summary(ano_model2)
print(z)


# Adjusting the mean again
bb_group <- 2 #number of treatment group
bb_name <- c("Control","CommInoc") #name of groups
bb_size <- c(17,15) #sample sizes
bb_mean <- c(28,32) #mean responses
bb_sd <- c(5,5) #standard deviation
ID <- 1:sum(bb_size) #creates unique ID

BerryMass <- c(rnorm(n=bb_size[1],mean = bb_mean[1],sd=bb_sd[1]),
               rnorm(n=bb_size[2],mean = bb_mean[2],sd=bb_sd[2]))

Treatment <- rep(bb_name,bb_size)               

# Construct data frame
ano_data3 <- data.frame(ID,Treatment,BerryMass)
head(ano_data3)

# Analyze data with ANOVA
ano_model3 <- aov(BerryMass~Treatment,data = ano_data3)
print(ano_model3)
z <- summary(ano_model3)
print(z)


# two way test
library(ggplot2)
# Create random dataset
bb_group <- 2 #number of treatment group
bb_name <- c("Control","CommInoc") #name of groups
bb_size <- c(17,15) #sample sizes
bb_mean <- c(22,45) #mean responses
bb_sd <- c(5,5) #standard deviation
ID <- 1:sum(bb_size) #creates unique ID

BerryMass <- c(rnorm(n=bb_size[1],mean = bb_mean[1],sd=bb_sd[1]),
               rnorm(n=bb_size[2],mean = bb_mean[2],sd=bb_sd[2]))

Treatment <- rep(bb_name,bb_size)               

# Construct data frame
BB_data <- data.frame(ID,Treatment,BerryMass)
head(BB_data)

# Analyze data with Two way T-test
BB_model <- t.test(BerryMass~Treatment,data = BB_data)
print(BB_model)
z <- summary(BB_model)
print(z)

flat_out <- unlist(z)
ano_stats <- list(f_ratio <- unlist(z)[7],f_Pval <- unlist(z)[9])
print(ano_stats)

# Graph t.test data
BB_plot <- ggplot(BB_data)+
  aes(x=Treatment,y=BerryMass,fill=Treatment)+
  geom_boxplot()+
  labs(title = "Effect of Inoculation on Berry Mass")+
  theme_minimal()

print(BB_plot)


# calculate effect size for original data
# η^2 = SStreat / SStotal = 0.82
# SStreat =treatment sum of squares = 4137
# SStotal =total sum of squares = 4137 + 907 = 5044
# Effect size (f) = √((η^2/(1- η^2) = 2.13

# calculate power of test
power.anova.test(groups = 3, n=15,between.var =1,within.var = 2,sig.level = 0.05,power = NULL)

# calculate minimal sample size
power.anova.test(groups = 3, n=NULL,between.var =1,within.var = 2,sig.level = 0.05,power = 0.9)
