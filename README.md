########################################################################################################
#### Aggregate-Latent-Class-Logit: Demand Model 
#### R code for estimation of latent class logit model with aggregate data
#### By Minha Hwang (minha@alum.mit.edu)
########################################################################################################


#### There are 3 R codes and 1 supporting data set. 
## 1. lc-agglogit-main-availablity.R: Main R code - Note that this accomodate varying choice sets 
##
## 2. ll_adclc.ava.R: Log-Likelihood functoin for maximization
##
## 3. fake-data-gen-with-availability.R: Synthetic data generation code (for code validation)
##
## 4. lcl_agg_fake_data-with-availability.csv: Synthetic data generated from the code above

#### Reference: 1. A Segment-Level Model of Category Volume and Brand Choice
####            William R. Dillon and Sunil Gupta 
####            Marketing Science, Vol. 15, No. 1 (1996), pp 38-59
####     
####            2. The Recoverability of Segmentation Structure from Store-level Aggregate Data
####            Anand V. Bodapati, and Sachin Gupta     
####            Journal of Marketing Research, August 2004 

#### In case of quesitons, please reach out to minha@alum.mit.edu

