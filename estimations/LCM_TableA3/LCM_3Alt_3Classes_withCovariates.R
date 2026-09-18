####################################################################
####################################################################
#####
#####            LCM, no covariates in the allocation probabilities
#####
####################################################################
####################################################################


################################################
# 
# The script: LCM ESTIMATES WITH 3 ALTERNATIVES
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
choose.treatment <- 2   ## the treatment that we choose to test
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
# in the original data, the alternatives a named from 0 to 5. We will rename them from 1 to 6 as follows:
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
  modelName       = "LC_3Alt_3classes_with_covariates",
  modelDescr      = "Simple LC model, with covariates in class allocation model; The effect of number of alternatives",
  indivID         = "id",
  nCores          = 1,
  outputDirectory = "output.LCM.3Alt_3classes.withCov.Bgw_estimate"
 )

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = Data

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(asc_1_a     = 3.401797,
                asc_1_b     = -0.8528952,
                asc_1_c     = -1.577652,
                asc_2_a     = 0.7804651,
                asc_2_b     = 0.1395651,
                asc_2_c     = 0.07629375,
                #
                b_clar_a = 0.1312619,
                b_clar_b = 0.3736446,
                b_clar_c = 0.1738175,
                #
                b_fish_a = -0.1853453,
                b_fish_b = 0.7229548,
                b_fish_c = 0.0591752,
                #
                b_bio_a = 0.2011139,
                b_bio_b = 0.5424095,
                b_bio_c = 0.2209665,
                #
                b_coast_a = 0.1776894,
                b_coast_b = 0.179802,
                b_coast_c = -0.3967838,
                #
                b_lit_a = 0.05682273,
                b_lit_b = 0.6270214,
                b_lit_c = 0.1749331,
                #
                b_cost_a = -0.006372523,
                b_cost_b = -0.006120624,
                b_cost_c = -0.01362302,
                #
                delta_a   = 0.7118996,
                delta_b   = -0.06643528,
                delta_c   = 0.0,
                #
                gamma_age_a = -0.03482853,
                gamma_age_b = 0.01489801,
                gamma_age_c = 0.0,
                #
                gamma_female_a = -0.542799,
                gamma_female_b = 0.3814076,
                gamma_female_c = 0.0,
                #
                gamma_educ2_a = 0.9335167,
                gamma_educ2_b = -0.2636703,
                gamma_educ2_c = 0.0,
                #
                gamma_educ3_a = -1.187704,
                gamma_educ3_b = -0.5013148,
                gamma_educ3_c = 0.0,
                #
                gamma_educ4_a = 0.623665,
                gamma_educ4_b = -0.2951392,
                gamma_educ4_c = 0.0 )  

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("delta_c", "gamma_age_c", "gamma_female_c", "gamma_educ2_c", "gamma_educ3_c", "gamma_educ4_c" )

# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  
  lcpars[["asc_1"]]   = list(asc_1_a, asc_1_b, asc_1_c)
  lcpars[["asc_2"]]   = list(asc_2_a, asc_2_b, asc_2_c)
  lcpars[["b_clar"]]  = list(b_clar_a  , b_clar_b  ,b_clar_c   )
  lcpars[["b_fish"]]  = list(b_fish_a  , b_fish_b  ,b_fish_c   )
  lcpars[["b_bio"]]   = list(b_bio_a   , b_bio_b   ,b_bio_c    )
  lcpars[["b_coast"]] = list(b_coast_a , b_coast_b ,b_coast_c  )
  lcpars[["b_lit"]]   = list(b_lit_a   , b_lit_b   ,b_lit_c    )
  lcpars[["b_cost"]]  = list(b_cost_a  , b_cost_b  ,b_cost_c   )
  
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
    alternatives = c(alt1=1, alt2=2, alt3=3 ),
    #avail        = list(alt1=av_1, alt2=av_2, alt3=av_3, alt4=av_4, alt5=av_5, alt6=av_6),
    choiceVar    = choibio
  )
  
  ### Loop over classes
  for(s in 1:3){                     # This should be "for(s in 1:"number of classes"){" when we use "bgw" in the estimation model
    
    ### Compute class-specific utilities
    V=list()
    V[["alt1"]] = asc_1[[s]] + b_clar[[s]]*clar1 + b_fish[[s]]*fish1 + b_bio[[s]]*bio1 + b_coast[[s]]*coast1 + b_lit[[s]]*lit1 + b_cost[[s]]*cost1
    V[["alt2"]] = asc_2[[s]] + b_clar[[s]]*clar2 + b_fish[[s]]*fish2 + b_bio[[s]]*bio2 + b_coast[[s]]*coast2 + b_lit[[s]]*lit2 + b_cost[[s]]*cost2
    V[["alt3"]] =              b_clar[[s]]*clar3 + b_fish[[s]]*fish3 + b_bio[[s]]*bio3 + b_coast[[s]]*coast3 + b_lit[[s]]*lit3 + b_cost[[s]]*cost3
    

       
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

# Model name                                  : LC_3Alt_3classes_with_covariates
# Model description                           : Simple LC model, no covariates in class allocation model; The effect of number of alternatives
# Model run at                                : 2025-03-20 17:12:51.407236
# Estimation method                           : bgw
# Model diagnosis                             : Relative function convergence
# Optimisation diagnosis                      : Maximum found
# hessian properties                     : Negative definite
# maximum eigenvalue                     : -0.486126
# reciprocal of condition number         : 1.28886e-07
# Number of individuals                       : 301
# Number of rows in database                  : 2408
# Number of modelled outcomes                 : 2408
# 
# Number of cores used                        :  1 
# Model without mixing
# 
# LL(start)                                   : -1852.81
# LL (whole model) at equal shares, LL(0)     : -2645.46
# LL (whole model) at observed shares, LL(C)  : -2573.85
# LL(final, whole model)                      : -1852.81
# Rho-squared vs equal shares                  :  0.2996 
# Adj.Rho-squared vs equal shares              :  0.286 
# Rho-squared vs observed shares               :  0.2801 
# Adj.Rho-squared vs observed shares           :  0.2685 
# AIC                                         :  3777.61 
# BIC                                         :  3985.93 
# 
# LL(0,Class_1)                    : -2645.46
# LL(final,Class_1)                : -6083.86
# LL(0,Class_2)                    : -2645.46
# LL(final,Class_2)                : -3314.15
# LL(0,Class_3)                    : -2645.46
# LL(final,Class_3)                : -3020.56
# 
# Estimated parameters                        : 36
# Time taken (hh:mm:ss)                       :  00:00:17.25 
# pre-estimation                         :  00:00:1.37 
# estimation                             :  00:00:1.03 
# post-estimation                        :  00:00:14.85 
# Iterations                                  :  14  
# 
# Unconstrained optimisation.
# 
# Estimates:
#   Estimate        s.e.   t.rat.(0)    Rob.s.e. Rob.t.rat.(0)
# asc_1_a           3.397094    0.700424     4.85005    0.770442       4.40928
# asc_1_b          -0.852064    0.287603    -2.96264    0.528037      -1.61365
# asc_1_c          -1.575658    0.328629    -4.79464    0.770569      -2.04480
# asc_2_a           0.781392    0.376288     2.07658    0.393311       1.98670
# asc_2_b           0.139944    0.068084     2.05547    0.065988       2.12075
# asc_2_c           0.075967    0.106130     0.71580    0.102345       0.74226
# b_clar_a          0.130232    0.191092     0.68152    0.197314       0.66002
# b_clar_b          0.373675    0.055961     6.67737    0.079870       4.67855
# b_clar_c          0.174239    0.070519     2.47080    0.122159       1.42633
# b_fish_a         -0.184579    0.362656    -0.50896    0.426741      -0.43253
# b_fish_b          0.723826    0.082319     8.79291    0.096184       7.52542
# b_fish_c          0.059792    0.116282     0.51420    0.159512       0.37485
# b_bio_a           0.200311    0.177772     1.12678    0.170808       1.17272
# b_bio_b           0.542890    0.061179     8.87374    0.107825       5.03492
# b_bio_c           0.221507    0.084816     2.61161    0.191544       1.15643
# b_coast_a         0.177589    0.383626     0.46292    0.509274       0.34871
# b_coast_b         0.180066    0.079854     2.25494    0.108932       1.65302
# b_coast_c        -0.395112    0.150040    -2.63338    0.327558      -1.20624
# b_lit_a           0.056511    0.176739     0.31974    0.104542       0.54056
# b_lit_b           0.627392    0.061906    10.13460    0.104279       6.01649
# b_lit_c           0.175789    0.096261     1.82617    0.230260       0.76344
# b_cost_a         -0.006385    0.003137    -2.03526    0.003837      -1.66396
# b_cost_b         -0.006117    0.001450    -4.21919    0.002946      -2.07664
# b_cost_c         -0.013616    0.001188   -11.46279    0.001644      -8.28138
# delta_a           0.701144    0.962949     0.72812    1.194645       0.58691
# delta_b          -0.081469    0.825519    -0.09869    1.322690      -0.06159
# delta_c           0.000000          NA          NA          NA            NA
# gamma_age_a      -0.034718    0.016300    -2.12998    0.017708      -1.96063
# gamma_age_b       0.015083    0.012717     1.18604    0.016574       0.91008
# gamma_age_c       0.000000          NA          NA          NA            NA
# gamma_female_a   -0.542977    0.408482    -1.32925    0.405468      -1.33913
# gamma_female_b    0.381899    0.303470     1.25844    0.310433       1.23021
# gamma_female_c    0.000000          NA          NA          NA            NA
# gamma_educ2_a     0.938622    0.662133     1.41757    0.744122       1.26138
# gamma_educ2_b    -0.258481    0.469786    -0.55021    0.562147      -0.45981
# gamma_educ2_c     0.000000          NA          NA          NA            NA
# gamma_educ3_a    -1.184045    0.849063    -1.39453    0.866811      -1.36598
# gamma_educ3_b    -0.500490    0.497610    -1.00579    0.505489      -0.99011
# gamma_educ3_c     0.000000          NA          NA          NA            NA
# gamma_educ4_a     0.627242    0.677379     0.92598    0.736628       0.85150
# gamma_educ4_b    -0.292296    0.494156    -0.59151    0.541399      -0.53989
# gamma_educ4_c     0.000000          NA          NA          NA            NA
# 
# 
# Summary of class allocation for model component :
#   Mean prob.
# Class_1      0.1589
# Class_2      0.5312
# Class_3      0.3099# 

