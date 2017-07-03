ll_adclc_ava <- function(theta) {
  
  beta  <- matrix(theta[1L:(K * S)], nrow = K, ncol = S)
  rownames(beta) <- colnames(x[[1]]); colnames(beta) <- paste("class", 1:S, sep = ':')
  gamma0 <- theta[-c(1L:(K * S))]
  #gamma<-exp(gamma0)/(1+exp(gamma0))
  
  # Rearrange y: T x J 
  yran <- aperm(array(y,c(J,T))) 
  
  ### Calcualte segemnt membership probability (i.e. weights )
  # Need to impose restrictions regarding the value of segment sizes
  
  if (S==1) {
    sw <-1
  }  else  {
    #sw<-c(1,ifelse(exp(gamma0)==Inf,1,exp(gamma0)))
    sw<-c(1,exp(gamma0))
    sw<-sw/sum(sw)                 # this is a (1 x S) vector of segment membership probabilities
    #sw[is.na(sw)] <- 0.5
    sw[is.na(sw)] <- 0.1
    }
  
  ### Calculate Multinomial Choice Probability 
  Utility <- as.matrix(x) %*% beta      # JT x S matrix
    
  # Rearrange this as T x J x S array
  Ua <- array(NA,dim= c(T,J,S))
  probs <- array(NA,dim=c(T,J,S))
  wprobs <- array(NA,dim=c(T,J,S))
  ava <- array(NA,dim=c(T,J,S))
  
  for (s in 1:S) {
    
    Ua[,,s] <- aperm(array(Utility[,s],c(J,T)))
    ava[,,s] <-aperm(array(a,c(J,T)))
    probs[,,s] <- ava[,,s] * exp(Ua[,,s]) / rowSums(ava[,,s] * exp(Ua[,,s]))
    wprobs[,,s] <- sw[s]*probs[,,s]
  }
  
  fprobs <- apply(wprobs,c(1,2),sum)
  #fprobs[is.na(fprobs)] <- 0.5
  fprobs[is.na(fprobs)] <- 0.1
  
  
  ### Calculuate log-likelihood 
  
  #llff <- sum(log(fprobs[which(fprobs!=0)]) * yran[which(yran!=0)])
  
  ll <- log(fprobs)
  #ll[which(ll==-Inf)] <- 0
  llf <- ll*yran
  llff <- sum(rowSums(llf,na.rm=TRUE))
  #llff <- sum(rowSums(llf))
  
  
  return(llff)
}
  
  
  
  