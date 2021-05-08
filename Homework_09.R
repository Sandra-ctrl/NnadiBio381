# homework 9
# 18 April 2021
# Sandra Nnadi
# All functions must be declared at the start

#################
# FUNCTION: BB_data
# read in blueberry data
# input: Mean, variance and size of groups
# output: dataframe

BB_data <- function(bb_size=c(17,15,16), bb_mean=c(22,45,32), bb_sd=c(5,5,5)) {
  
  bb_name <- c("Control","CommInoc","LocInoc")
  
  BerryMass <- c(rnorm(n=bb_size[1],mean = bb_mean[1],sd=bb_sd[1]),
                 rnorm(n=bb_size[2],mean = bb_mean[2],sd=bb_sd[2]),
                 rnorm(n=bb_size[3],mean = bb_mean[3],sd=bb_sd[3]))
  
  Treatment <- rep(bb_name,bb_size) 
  
  ID <- 1:sum(bb_size)
  
  ano_data <- data.frame(ID,Treatment,BerryMass)
  
  return(ano_data)
  
}
BB_data()
head(BB_data)
tail(BB_data)

#################
# FUNCTION: BB_Analyze
# Analyze blueberry data
# input: dataframe of vectors
# output: ANOVA data statistics
my_data <- BB_data()
BB_analyze <- function(ano_data=my_data) {
  
  ano_model <- aov(BerryMass~Treatment,data = ano_data)
  
  return(summary(ano_model))
  
}


BB_analyze()

#################
# FUNCTION: BB_Graph
# plots blueberry data
# input: dataframe
# output: boxplot

library(ggplot2)

BB_graph <- function(ano_data=my_data)  {
  ano_plot <- ggplot(ano_data)+
    aes(x=Treatment,y=BerryMass,fill=Treatment)+
    geom_boxplot()+
    labs(title = "Effect of Inoculation on Berry Mass")+
    theme_minimal()
  return(ano_plot)
  
}

BB_graph()

# Preliminaries
library(ggplot2)
set.seed(99)     

#-------------------------------
#Global variables
bb_size=c(17,15,16)
bb_mean=c(22,45,32)
bb_sd=c(5,5,5)

#------------------------------

# Program body
# construct the data frame
temp_1 <- BB_data()
x <- temp_1$Treatment # extract predictor
y <- temp_1$BerryMass # extract response

# fit the regression model
temp_2 <- BB_analyze()

BB_graph()

print(temp_2) #print model summary


# Creating a new function
######################
# FUNCTION: BB_density
# density plot shows the distribution of the Berry mass across treatments
# input: dataframe of vectors
# output: Density plot

library(ggplot2)

BB_density <- function(ano_data=my_data){
  ggplot2::ggplot(data=ano_data, mapping=
                    aes(x=BerryMass , fill=Treatment)) +
    geom_density(alpha=0.8) + 
    labs(title="Density of Berry Mass across treatments", 
         x="BerryMass") + 
    theme_minimal()
}

BB_density()

# ploting data with proper labels 
# And adding line with proper properties
p1 <- ggplot(data=my_data, aes(x=BerryMass, y=..density..)) +
  geom_density(color="grey60",fill="cornsilk",size=0.7) 
print(p1)

# Get maximum likelihood parameters for normal
normPars <- fitdistr(my_data$BerryMass,"normal")
print(normPars)
str(normPars)
normPars$estimate["mean"] # note structure of getting a named attribute

# plot normal probability density
meanML <- normPars$estimate["mean"]
sdML <- normPars$estimate["sd"]

xval <- seq(0,max(my_data$BerryMass),len=length(my_data$BerryMass))

stat <- stat_function(aes(x = xval, y = ..y..), fun = dnorm, colour="red", n = length(my_data$BerryMass), args = list(mean = meanML, sd = sdML))
p1 + stat
