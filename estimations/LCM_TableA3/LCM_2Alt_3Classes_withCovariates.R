####################################################################
####################################################################
#####
#####            LCM, with covariates in the allocation probabilities
#####
####################################################################
####################################################################

################################################
# 
# The script: LCM ESTIMATES WITH 2 ALTERNATIVES
# 
################################################

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

# set seed
set.seed(2345)

# set working directory
setwd(".") 

### Clear memory
rm(list = ls())

#### Loading R packages
library(apollo)   # Load Apollo library
library(bgw)      # Load bgw library

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

# Reading the database 
data.MNL.full <- read.table("../../PrefMatWideFormatFullBiogeme.txt", header=TRUE)

# Choose treatment (the treatment that we want to test)
# There are 5 possible treatments: 
#     - treatment 1: 2 alternatives (statu-quo + alternative  1)
#     - treatment 2: 3 alternatives (statu-quo + alternatives 1,2)
#     - treatment 3: 4 alternatives (statu-quo + alternatives 1,2,3)
#     - treatment 4: 5 alternatives (statu-quo + alternatives 1,2,3,4)
#     - treatment 5: 6 alternatives (statu-quo + alternatives 1,2,3,4,5)
choose.treatment <- 1   ## the treatment that we choose to test
Data.1 <- data.MNL.full[-which(data.MNL.full$treat != choose.treatment),]

# only task <=8  (we only consider the task from 1 to 8, so we drop those above 8)
Data <- Data.1[-which(Data.1$task > 8),]
length(Data[,1])

# availability of the choices in each treatment
Data$av_1 <- 1
Data$av_2 <- 1
Data$av_3 <- 0
Data$av_4 <- 0
Data$av_5 <- 0
Data$av_6 <- 0

Data$av_3[Data$treat == 2 | Data$treat == 3 | Data$treat == 4 | Data$treat == 5] <- 1
Data$av_4[Data$treat == 3 | Data$treat == 4 | Data$treat == 5] <- 1
Data$av_5[Data$treat == 4 | Data$treat == 5] <- 1
Data$av_6[Data$treat == 5] <- 1

# treatment indicator
Data$tr_1 <- 0
Data$tr_2 <- 0
Data$tr_3 <- 0
Data$tr_4 <- 0
Data$tr_5 <- 0

Data$tr_1[Data$treat == 1] <- 1
Data$tr_2[Data$treat == 2] <- 1
Data$tr_3[Data$treat == 3] <- 1
Data$tr_4[Data$treat == 4] <- 1
Data$tr_5[Data$treat == 5] <- 1

##
# Socio-Demographic covariates
##
# Age
Data$age <- 2016 - Data$v371
# Gender
Data$female <- 0
Data$female[Data$v372==1] <- 1
# Education
table(Data$v375)  # How many observations are there in each category of ‘Education’?
# There are very few observations with category 6, so we eliminate them from the sample
# Therefore, we will only put categories 2, 3 and 4 in the model. And we consider category 1 as the base category. 
Data <- Data[-which(Data$v375 ==6),]
Data$educ1 <- 0
Data$educ1[Data$v375==1] <- 1
Data$educ2 <- 0
Data$educ2[Data$v375==2] <- 1
Data$educ3 <- 0
Data$educ3[Data$v375==3] <- 1
Data$educ4 <- 0
Data$educ4[Data$v375==4] <- 1

length(Data[,1])

##
# Setting parameters
##
n.choices              <- 8                              # Number of choice occasions of one individual 
n.individuals          <- length(Data[,1])/n.choices     # Number of individuals
n.rows                 <- n.individuals * n.choices      # Number of rows

# Rename attributes
# in the original data, the alternatives are named from 0 to 5. We will rename them from 1 to 6 as follows:
colnames(Data)[which(colnames(Data[1,]) == "clar5") ] <- "clar6"
colnames(Data)[which(colnames(Data[1,]) == "fish5") ] <- "fish6"
colnames(Data)[which(colnames(Data[1,]) == "bio5")  ] <- "bio6"
colnames(Data)[which(colnames(Data[1,]) == "coast5")] <- "coast6"
colnames(Data)[which(colnames(Data[1,]) == "lit5")  ] <- "lit6"
colnames(Data)[which(colnames(Data[1,]) == "cost5") ] <- "cost6"

colnames(Data)[which(colnames(Data[1,]) == "clar4") ] <- "clar5"
colnames(Data)[which(colnames(Data[1,]) == "fish4") ] <- "fish5"
colnames(Data)[which(colnames(Data[1,]) == "bio4")  ] <- "bio5"
colnames(Data)[which(colnames(Data[1,]) == "coast4")] <- "coast5"
colnames(Data)[which(colnames(Data[1,]) == "lit4")  ] <- "lit5"
colnames(Data)[which(colnames(Data[1,]) == "cost4") ] <- "cost5"

colnames(Data)[which(colnames(Data[1,]) == "clar3") ] <- "clar4"
colnames(Data)[which(colnames(Data[1,]) == "fish3") ] <- "fish4"
colnames(Data)[which(colnames(Data[1,]) == "bio3")  ] <- "bio4"
colnames(Data)[which(colnames(Data[1,]) == "coast3")] <- "coast4"
colnames(Data)[which(colnames(Data[1,]) == "lit3")  ] <- "lit4"
colnames(Data)[which(colnames(Data[1,]) == "cost3") ] <- "cost4"

colnames(Data)[which(colnames(Data[1,]) == "clar2") ] <- "clar3"
colnames(Data)[which(colnames(Data[1,]) == "fish2") ] <- "fish3"
colnames(Data)[which(colnames(Data[1,]) == "bio2")  ] <- "bio3"
colnames(Data)[which(colnames(Data[1,]) == "coast2")] <- "coast3"
colnames(Data)[which(colnames(Data[1,]) == "lit2")  ] <- "lit3"
colnames(Data)[which(colnames(Data[1,]) == "cost2") ] <- "cost3"

colnames(Data)[which(colnames(Data[1,]) == "clar1") ] <- "clar2"
colnames(Data)[which(colnames(Data[1,]) == "fish1") ] <- "fish2"
colnames(Data)[which(colnames(Data[1,]) == "bio1")  ] <- "bio2"
colnames(Data)[which(colnames(Data[1,]) == "coast1")] <- "coast2"
colnames(Data)[which(colnames(Data[1,]) == "lit1")  ] <- "lit2"
colnames(Data)[which(colnames(Data[1,]) == "cost1") ] <- "cost2"

colnames(Data)[which(colnames(Data[1,]) == "clar0") ] <- "clar1"
colnames(Data)[which(colnames(Data[1,]) == "fish0") ] <- "fish1"
colnames(Data)[which(colnames(Data[1,]) == "bio0")  ] <- "bio1"
colnames(Data)[which(colnames(Data[1,]) == "coast0")] <- "coast1"
colnames(Data)[which(colnames(Data[1,]) == "lit0")  ] <- "lit1"
colnames(Data)[which(colnames(Data[1,]) == "cost0") ] <- "cost1"

# ################################################## #
####    INITIALISE APOLLO                       ####
# ################################################## #

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "LC_2Alt_3classes_with_covariates",
  modelDescr      = "Simple LC model, with covariates in class allocation model; The effect of number of alternatives",
  indivID         = "id",
  nCores          = 1,
  outputDirectory = "output.LCM.2Alt_3classes.withCov.Bgw_estimate"
  )

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = Data

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(asc_1_a     = -0.03423189,
                asc_1_b     = 2.332829,
                asc_1_c     = 1.357034,
                #
                b_clar_a = 0.3865367,
                b_clar_b = -0.03973034,
                b_clar_c = 0.6205324,
                #
                b_fish_a = 1.70898,
                b_fish_b = -0.3205511,
                b_fish_c = 0.4997553,
                #
                b_bio_a = 0.3193924,
                b_bio_b = -0.08455519,
                b_bio_c = 0.5831015,
                #
                b_coast_a = 0.2925837,
                b_coast_b = 0.07257364,
                b_coast_c = -0.03872418,
                #
                b_lit_a = 0.6060667,
                b_lit_b = -0.3218113,
                b_lit_c = 0.8465453,
                #
                b_cost_a = -0.00682608,
                b_cost_b = -0.003098652,
                b_cost_c = -0.01991408,
                #
                delta_a   = 0.1136862,
                delta_b   = -1.059228,
                delta_c   = 0.0,
                #
                gamma_age_a = 0.008179743,
                gamma_age_b = 0.02383836,
                gamma_age_c = 0.0,
                #
                gamma_female_a = 0.1459879,
                gamma_female_b = -0.5666726,
                gamma_female_c =0.0,
                #
                gamma_educ2_a = -0.7638183,
                gamma_educ2_b = -0.7207282,
                gamma_educ2_c =0.0,
                #
                gamma_educ3_a = -0.9070469,
                gamma_educ3_b = -1.175055,
                gamma_educ3_c = 0.0,
                #
                gamma_educ4_a = -0.3869011,
                gamma_educ4_b = -1.10286,
                gamma_educ4_c = 0.0  )  

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("delta_c", "gamma_age_c", "gamma_female_c", "gamma_educ2_c", "gamma_educ3_c", "gamma_educ4_c" )

# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  
  lcpars[["asc_1"]]   = list(asc_1_a, asc_1_b, asc_1_c)
 
  lcpars[["b_clar"]]  = list(b_clar_a  , b_clar_b , b_clar_c  )
  lcpars[["b_fish"]]  = list(b_fish_a  , b_fish_b , b_fish_c  )
  lcpars[["b_bio"]]   = list(b_bio_a   , b_bio_b  , b_bio_c   )
  lcpars[["b_coast"]] = list(b_coast_a , b_coast_b, b_coast_c )
  lcpars[["b_lit"]]   = list(b_lit_a   , b_lit_b  , b_lit_c   )
  lcpars[["b_cost"]]  = list(b_cost_a  , b_cost_b , b_cost_c  )
  
  V=list()
  V[["class_a"]] = delta_a + gamma_age_a * age + gamma_female_a * female + gamma_educ2_a * educ2 + gamma_educ3_a * educ3 + gamma_educ4_a * educ4
  V[["class_b"]] = delta_b + gamma_age_b * age + gamma_female_b * female + gamma_educ2_b * educ2 + gamma_educ3_b * educ3 + gamma_educ4_b * educ4
  V[["class_c"]] = delta_c + gamma_age_c * age + gamma_female_c * female + gamma_educ2_c * educ2 + gamma_educ3_c * educ3 + gamma_educ4_c * educ4
  
  classAlloc_settings = list(
    classes      = c(class_a=1, class_b=2, class_c=3), 
    utilities    = V
  )
  
  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)
  
  return(lcpars)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2 ),
    #avail        = list(alt1=av_1, alt2=av_2, alt3=av_3, alt4=av_4, alt5=av_5, alt6=av_6),
    choiceVar    = choibio
  )
  
  ### Loop over classes
  for(s in 1:3){                     # This should be "for(s in 1:"number of classes"){" when we use "bgw" in the estimation model
    
    ### Compute class-specific utilities
    V=list()
    V[["alt1"]] = asc_1[[s]] + b_clar[[s]]*clar1 + b_fish[[s]]*fish1 + b_bio[[s]]*bio1 + b_coast[[s]]*coast1 + b_lit[[s]]*lit1 + b_cost[[s]]*cost1
    V[["alt2"]] =              b_clar[[s]]*clar2 + b_fish[[s]]*fish2 + b_bio[[s]]*bio2 + b_coast[[s]]*coast2 + b_lit[[s]]*lit2 + b_cost[[s]]*cost2
   
    mnl_settings$utilities = V
    mnl_settings$componentName = paste0("Class_",s)
    
    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
  }
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

## Estimate model
model = apollo_estimate(apollo_beta, apollo_fixed,
                       apollo_probabilities, apollo_inputs,
                       estimate_settings = list(writeIter = FALSE,
                                                silent    = FALSE,
                                                iterMax   = 500,
                                                estimationRoutine = "bgw"))

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# Model name                                  : LC_2Alt_3classes_with_covariates
# Model description                           : Simple LC model, no covariates in class allocation model; The effect of number of alternatives
# Model run at                                : 2025-03-22 13:33:05.686681
# Estimation method                           : bgw
# Model diagnosis                             : Relative function convergence
# Optimisation diagnosis                      : Maximum found
# hessian properties                     : Negative definite
# maximum eigenvalue                     : -0.657965
# reciprocal of condition number         : 2.6567e-07
# Number of individuals                       : 305
# Number of rows in database                  : 2440
# Number of modelled outcomes                 : 2440
# 
# Number of cores used                        :  1 
# Model without mixing
# 
# LL(start)                                   : -1139.29
# LL (whole model) at equal shares, LL(0)     : -1691.28
# LL (whole model) at observed shares, LL(C)  : -1680.04
# LL(final, whole model)                      : -1139.29
# Rho-squared vs equal shares                  :  0.3264 
# Adj.Rho-squared vs equal shares              :  0.3069 
# Rho-squared vs observed shares               :  0.3219 
# Adj.Rho-squared vs observed shares           :  0.304 
# AIC                                         :  2344.59 
# BIC                                         :  2535.98 
# 
# LL(0,Class_1)                    : -1691.28
# LL(final,Class_1)                : -4786.28
# LL(0,Class_2)                    : -1691.28
# LL(final,Class_2)                : -2419.07
# LL(0,Class_3)                    : -1691.28
# LL(final,Class_3)                : -1931.08
# 
# Estimated parameters                        : 33
# Time taken (hh:mm:ss)                       :  00:00:18.03 
# pre-estimation                         :  00:00:2.13 
# estimation                             :  00:00:1.15 
# post-estimation                        :  00:00:14.76 
# Iterations                                  :  9  
# 
# Unconstrained optimisation.
# 
# Estimates:
#   Estimate        s.e.   t.rat.(0)    Rob.s.e. Rob.t.rat.(0)
# asc_1_a           2.332200    0.996794      2.3397    1.575655       1.48015
# asc_1_b          -0.033222    0.236882     -0.1402    0.255791      -0.12988
# asc_1_c           1.356111    0.341111      3.9756    0.450593       3.00961
# b_clar_a         -0.040311    0.320028     -0.1260    0.378393      -0.10653
# b_clar_b          0.386641    0.086636      4.4628    0.089277       4.33081
# b_clar_c          0.620837    0.103554      5.9953    0.127817       4.85723
# b_fish_a         -0.321337    0.792638     -0.4054    1.006373      -0.31930
# b_fish_b          1.706911    0.281738      6.0585    0.284598       5.99763
# b_fish_c          0.499152    0.208294      2.3964    0.272948       1.82874
# b_bio_a          -0.084962    0.324753     -0.2616    0.320984      -0.26469
# b_bio_b           0.319434    0.092703      3.4458    0.091336       3.49736
# b_bio_c           0.583275    0.102491      5.6910    0.113833       5.12398
# b_coast_a         0.072772    0.688707      0.1057    0.850046       0.08561
# b_coast_b         0.292057    0.220869      1.3223    0.228301       1.27926
# b_coast_c        -0.039686    0.194512     -0.2040    0.191090      -0.20768
# b_lit_a          -0.321610    0.338057     -0.9513    0.366039      -0.87862
# b_lit_b           0.605991    0.091883      6.5953    0.096790       6.26089
# b_lit_c           0.846990    0.124987      6.7767    0.172217       4.91816
# b_cost_a         -0.003083    0.004897     -0.6296    0.005959      -0.51739
# b_cost_b         -0.006825    0.001561     -4.3727    0.001432      -4.76457
# b_cost_c         -0.019937    0.001889    -10.5535    0.002895      -6.88662
# delta_a          -1.057354    0.900851     -1.1737    0.967605      -1.09275
# delta_b           0.118786    0.676932      0.1755    0.718681       0.16528
# delta_c           0.000000          NA          NA          NA            NA
# gamma_age_a       0.023776    0.015854      1.4997    0.015829       1.50202
# gamma_age_b       0.008047    0.011668      0.6896    0.012562       0.64058
# gamma_age_c       0.000000          NA          NA          NA            NA
# gamma_female_a   -0.566265    0.392186     -1.4439    0.465914      -1.21539
# gamma_female_b    0.145049    0.283270      0.5121    0.294386       0.49272
# gamma_female_c    0.000000          NA          NA          NA            NA
# gamma_educ2_a    -0.718352    0.556758     -1.2902    0.568176      -1.26431
# gamma_educ2_b    -0.758878    0.469624     -1.6159    0.516245      -1.47000
# gamma_educ2_c     0.000000          NA          NA          NA            NA
# gamma_educ3_a    -1.173361    0.700528     -1.6750    0.838494      -1.39937
# gamma_educ3_b    -0.905496    0.493109     -1.8363    0.508642      -1.78022
# gamma_educ3_c     0.000000          NA          NA          NA            NA
# gamma_educ4_a    -1.100172    0.632697     -1.7389    0.640986      -1.71638
# gamma_educ4_b    -0.384078    0.485499     -0.7911    0.512190      -0.74987
# gamma_educ4_c     0.000000          NA          NA          NA            NA
# 
# 
# Summary of class allocation for model component :
#   Mean prob.
# Class_1      0.1642
# Class_2      0.4132
# Class_3      0.4226

