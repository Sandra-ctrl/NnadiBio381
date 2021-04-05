# Homework 7
# March 24 2021
# Sandra Nnadi

#-------------------------------
library(ggplot2) # for graphics
library(MASS) # for maximum likelihood estimation

# read in data vector
z <- rnorm(n=3000,mean = 0.2)
z <- data.frame(1:3000,z)
names(z)<- list("ID","myVar")
z <- z[z$myVar>0,]
str(z)
summary(z$myVar)

# plot histogram of data
p1 <- ggplot(data=z, aes(x=myVar, y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) 
print(p1)

# add empirical density curve
p1 <-  p1 +  geom_density(linetype="dotted",size=0.75)
print(p1)

# Get maximum likelihood parameters for normal
normPars <- fitdistr(z$myVar,"normal")
print(normPars)
str(normPars)
normPars$estimate["mean"] # note structure of getting a named attribute

# plot normal probability density
meanML <- normPars$estimate["mean"]
sdML <- normPars$estimate["sd"]

xval <- seq(0,max(z$myVar),len=length(z$myVar))

stat <- stat_function(aes(x = xval, y = ..y..), fun = dnorm, colour="red", n = length(z$myVar), args = list(mean = meanML, sd = sdML))
p1 + stat

# plot exponential probability density
expoPars <- fitdistr(z$myVar,"exponential")
rateML <- expoPars$estimate["rate"]

stat2 <- stat_function(aes(x = xval, y = ..y..), fun = dexp, colour="blue", n = length(z$myVar), args = list(rate=rateML))
p1 + stat + stat2

# plot uniform probability density
stat3 <- stat_function(aes(x = xval, y = ..y..), fun = dunif, colour="darkgreen", n = length(z$myVar), args = list(min=min(z$myVar), max=max(z$myVar)))
p1 + stat + stat2 + stat3

# plot gamma probability density
gammaPars <- fitdistr(z$myVar,"gamma")
shapeML <- gammaPars$estimate["shape"]
rateML <- gammaPars$estimate["rate"]

stat4 <- stat_function(aes(x = xval, y = ..y..), fun = dgamma, colour="brown", n = length(z$myVar), args = list(shape=shapeML, rate=rateML))
p1 + stat + stat2 + stat3 + stat4

# plot beta probability density
pSpecial <- ggplot(data=z, aes(x=myVar/(max(myVar + 0.1)), y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) + 
  xlim(c(0,1)) +
  geom_density(size=0.75,linetype="dotted")

betaPars <- fitdistr(x=z$myVar/max(z$myVar + 0.1),start=list(shape1=1,shape2=2),"beta")
shape1ML <- betaPars$estimate["shape1"]
shape2ML <- betaPars$estimate["shape2"]

statSpecial <- stat_function(aes(x = xval, y = ..y..), fun = dbeta, colour="orchid", n = length(z$myVar), args = list(shape1=shape1ML,shape2=shape2ML))
pSpecial + statSpecial


# working with real data

library(ggplot2) # for graphics
library(MASS) # for maximum likelihood estimation

z <- read.table("Gorischek_pollen_seed.csv",header=TRUE,sep=",", stringsAsFactors=FALSE)
str(z)
summary(z)
summary(z$Bud_Number)

# plot histogram of data
p1 <- ggplot(data=z, aes(x=Bud_Number, y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) 
print(p1)

# add empirical density curve
p1 <-  p1 +  geom_density(linetype="dotted",size=0.75)
print(p1)

# Get maximum likelihood parameters for normal
normPars <- fitdistr(z$Bud_Number,"normal")
print(normPars)
str(normPars)
normPars$estimate["mean"] # note structure of getting a named attribute

# plot normal probability density
meanML <- normPars$estimate["mean"]
sdML <- normPars$estimate["sd"]

xval <- seq(0,max(z$Bud_Number),len=length(z$Bud_Number))

stat <- stat_function(aes(x = xval, y = ..y..), fun = dnorm, colour="red", n = length(z$Bud_Number), args = list(mean = meanML, sd = sdML))
p1 + stat

# plot exponential probability density
expoPars <- fitdistr(z$Bud_Number,"exponential")
rateML <- expoPars$estimate["rate"]

stat2 <- stat_function(aes(x = xval, y = ..y..), fun = dexp, colour="blue", n = length(z$Bud_Number), args = list(rate=rateML))
p1 + stat + stat2

# plot uniform probability density
stat3 <- stat_function(aes(x = xval, y = ..y..), fun = dunif, colour="darkgreen", n = length(z$Bud_Number), args = list(min=min(z$Bud_Number), max=max(z$Bud_Number)))
p1 + stat + stat2 + stat3

# plot gamma probability density
gammaPars <- fitdistr(z$Bud_Number,"gamma")
shapeML <- gammaPars$estimate["shape"]
rateML <- gammaPars$estimate["rate"]

stat4 <- stat_function(aes(x = xval, y = ..y..), fun = dgamma, colour="brown", n = length(z$Bud_Number), args = list(shape=shapeML, rate=rateML))
p1 + stat + stat2 + stat3 + stat4

# plot beta probability density
pSpecial <- ggplot(data=z, aes(x=Bud_Number/(max(Bud_Number + 0.1)), y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) + 
  xlim(c(0,1)) +
  geom_density(size=0.75,linetype="dotted")

betaPars <- fitdistr(x=z$Bud_Number/max(z$Bud_Number + 0.1),start=list(shape1=1,shape2=2),"beta")
shape1ML <- betaPars$estimate["shape1"]
shape2ML <- betaPars$estimate["shape2"]

statSpecial <- stat_function(aes(x = xval, y = ..y..), fun = dbeta, colour="orchid", n = length(z$Bud_Number), args = list(shape1=shape1ML,shape2=shape2ML))
pSpecial + statSpecial


# Due to the errors encountered when plotting the gamma and beta distributions.
# i decided to find another dataset with values that would fit the gamma and beta distribution.

# working with second real data

library(ggplot2) # for graphics
library(MASS) # for maximum likelihood estimation

b <- read.table("Diploid_phenotypic_data .csv",header=TRUE,sep=",", stringsAsFactors=FALSE)
str(b)
summary(b)
summary(b$firmness)

# plot histogram of data
p1 <- ggplot(data=b, aes(x=firmness, y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) 
print(p1)

# add empirical density curve
p1 <-  p1 +  geom_density(linetype="dotted",size=0.75)
print(p1)

# Get maximum likelihood parameters for normal
normPars <- fitdistr(b$firmness,"normal")
print(normPars)
str(normPars)
normPars$estimate["mean"] # note structure of getting a named attribute

# plot normal probability density
meanML <- normPars$estimate["mean"]
sdML <- normPars$estimate["sd"]

xval <- seq(0,max(b$firmness),len=length(b$firmness))

stat <- stat_function(aes(x = xval, y = ..y..), fun = dnorm, colour="red", n = length(b$firmness), args = list(mean = meanML, sd = sdML))
p1 + stat

# plot exponential probability density
expoPars <- fitdistr(b$firmness,"exponential")
rateML <- expoPars$estimate["rate"]

stat2 <- stat_function(aes(x = xval, y = ..y..), fun = dexp, colour="blue", n = length(b$firmness), args = list(rate=rateML))
p1 + stat + stat2

# plot uniform probability density
stat3 <- stat_function(aes(x = xval, y = ..y..), fun = dunif, colour="darkgreen", n = length(b$firmness), args = list(min=min(b$firmness), max=max(b$firmness)))
p1 + stat + stat2 + stat3

# plot gamma probability density
gammaPars <- fitdistr(b$firmness,"gamma")
shapeML <- gammaPars$estimate["shape"]
rateML <- gammaPars$estimate["rate"]

stat4 <- stat_function(aes(x = xval, y = ..y..), fun = dgamma, colour="purple", n = length(b$firmness), args = list(shape=shapeML, rate=rateML))
p1 + stat + stat2 + stat3 + stat4

# plot beta probability density
pSpecial <- ggplot(data=b, aes(x=firmness/(max(firmness + 0.1)), y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) + 
  xlim(c(0,1)) +
  geom_density(size=0.75,linetype="dotted")

betaPars <- fitdistr(x=b$firmness/max(b$firmness + 0.1),start=list(shape1=1,shape2=2),"beta")
shape1ML <- betaPars$estimate["shape1"]
shape2ML <- betaPars$estimate["shape2"]

statSpecial <- stat_function(aes(x = xval, y = ..y..), fun = dbeta, colour="orchid", n = length(b$firmness), args = list(shape1=shape1ML,shape2=shape2ML))
pSpecial + statSpecial
