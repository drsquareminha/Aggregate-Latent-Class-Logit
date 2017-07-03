##################################################################################################
####
####
#### Aggregagte latent class logit demand model 
####
#### By Minha Hwang (minha@alum.mit.edu)
####
####
##################################################################################################

library(maxLik)

## Load the data set
lcl.aggrf <- read.csv("C:/Users/Minha Hwang/Documents/lcl_agg_fake_data-with-availability.csv")

source("C:/Users/Minha Hwang/Documents/ll_adclc_ava.R")

## Number of segments
assign("S", 2, envir = .GlobalEnv) 
## Number of products
assign("J", 4, envir = .GlobalEnv) 
## Number of parameters to estimate
assign("K", 4, envir = .GlobalEnv) 
## Number of parameters to estimate
assign("T", 50, envir = .GlobalEnv) 


x <- lcl.aggrf[,6:9]
y <- lcl.aggrf[,4]
a <- lcl.aggrf[,5]

assign("y", y, envir = .GlobalEnv) 
assign("x", x, envir = .GlobalEnv) 
assign("a", a, envir = .GlobalEnv) 


par1 <- c(0.5,0.5,0.5,-3.0,1.5,0.5,0.1,-0.5,0.3)

 
sol2 <- maxLik(ll_adclc_ava, start=par1,control=list(printLevel=3),method="NR")

summary(sol2)


