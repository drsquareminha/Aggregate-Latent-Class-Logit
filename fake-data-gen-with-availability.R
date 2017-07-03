##########################################################################
##
##    Aggregate latenat classs logit
##    By Minha Hwang
##    Synthetic data generation (to test estimation code)
##    8/25, 2016
##
##########################################################################

# install.packages("maxLik")
# library(maxLik)

# Step 1: Set up a list of product attributes and levels 
attrib <- list(brand = c("1", "2", "3", "4"))
#pack = c("6pack", "12pack", "24pack"),
pricelevels = c(2, 3, 4, 5, 6)

availability = matrix(c(1,1,1,0, 
                        1,1,0,1,  
                        1,0,1,1, 
                        0,1,1,1), nrow=4,ncol=4,byrow=TRUE) 


profiles <- expand.grid(attrib)
#nrow(profiles)
profiles.coded <- model.matrix(~ brand,data=profiles)[,-1]


# Step 2: Assign demand parameters
coef.names <- NULL
for (a in 1) {
coef.names <- c(coef.names,
                paste(names(attrib)[a], attrib[[a]][-1], sep=""))
}

coef.names <- c(coef.names,"price")

beta1 <- c(1.5,0.5,0.1,-0.5)
beta2 <- c(0.5,0.5,0.5,-3.0)
phi <- 0.3 

names(beta1) <-coef.names
names(beta2) <-coef.names
names(phi) <- "seg1size"


# Step 3: Initialize required parameters
nWk <- 500
nProd <- 4
nPar <- nProd + 1 - 1
nInd <- 500
Ind.id <- 1:nInd        # Generate Individual IDs
Ind.seg <- sample(c(1,2),size=length(Ind.id), replace = TRUE, prob = c(phi,(1 - phi)))  # Assign segment membership 

# Assign demand parameters for each individual based on segment membership                   
coefs  <- matrix(,nrow=nInd,ncol=nPar)

for (i in Ind.id) {
 if (Ind.seg[i] == 1) coefs[i,] <- beta1
 else coefs[i,] <- beta2
}

# coefsf <- matrix(rep(coefs,each=nProd),nrow=nInd*nProd,ncol=nPar)
# Set product availability 
# ava <- ones(nProd,nWk)

# Step 4: Generate required keys
wk.id <- 1:nWk

#Ind.idf <- cbind(rep(Ind.id,nWk))
#seg <- cbind(rep(Ind.seg,nWk))
#productd <- cbind(rep())



# Step 5: Generate simulated dataset - in a "long" format
lcl.df <- data.frame(NULL)

for (m in seq_along(wk.id)) {
  price.m <- sample(pricelevels, size=nProd)
  availability.m <- availability[sample(nrow(availability),size=1),]
  profiles.f <- cbind(profiles.coded,price.m)
  
  utility.1 <- coefs %*% profiles.f[1,]
  utility.2 <- coefs %*% profiles.f[2,]
  utility.3 <- coefs %*% profiles.f[3,]
  utility.4 <- coefs %*% profiles.f[4,]
  wide.util <- cbind(utility.1, utility.2, utility.3, utility.4)  
  
  availability.e <- matrix(rep(t(availability.m),nInd),ncol=nProd,byrow=TRUE)

  probs <- availability.e *exp(wide.util) / rowSums(availability.e * exp(wide.util))
  choice <- apply(probs, 1, function(x) sample(1:nProd, size=1, prob=x))
  choice.expand <- rep(choice,each=nProd)
  
  wkkey = rep(m,nInd*nProd)
  indkey = rep(Ind.id,each=nProd)
  prodkey = rep(1:nProd,nInd)
  avaind = matrix(rep(availability.m),nInd,byrow=TRUE)
  
  profiles.expand = matrix(rep(t(profiles.f),nInd),ncol=ncol(profiles.f),byrow=TRUE)
  choicef = as.numeric(choice.expand==prodkey)

  data.i <- data.frame(wkkey,
                       indkey,
                       prodkey,
                       avaind, 
                       profiles.expand,
                       choicef)
                       
  lcl.df <- rbind(lcl.df, data.i)
}

# Step 6: Store individual-level dataset
names(lcl.df) <- c("wkkey","indkey","prodkey","avaind","brand2","brand3","brand4","price","choice")
write.csv(lcl.df,file="lcl_ind_fake_data-with-availability-v2.csv")



# Step 7: Aggregate across individuals and store dataset 

lcl.agg <- aggregate(choice ~ prodkey + wkkey,data=lcl.df, FUN=sum)
lcl.agg2 <- aggregate(cbind(avaind,brand2,brand3,brand4,price) ~ prodkey + wkkey,data=lcl.df,FUN=mean)

lcl.aggf <- merge(lcl.agg,lcl.agg2,by=c("wkkey","prodkey"))
lcl.aggrf <- lcl.aggf[with(lcl.aggf, order(wkkey,prodkey)),]
write.csv(lcl.aggrf,file="lcl_agg_fake_data-with-availability-v2.csv")

