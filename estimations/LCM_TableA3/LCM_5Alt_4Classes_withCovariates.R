####################################################################
####################################################################
#####
#####            LCM, no covariates in the allocation probabilities
#####
####################################################################
####################################################################


################################################
# 
# The script: LCM ESTIMATES WITH 6 ALTERNATIVES
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
choose.treatment <- 4   ## the treatment that we choose to test
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
  modelName       = "LC_5Alt_4classes_with_covariates",
  modelDescr      = "Simple LC model, no covariates in class allocation model; The effect of number of alternatives",
  indivID         = "id",
  nCores          = 1,
  outputDirectory = "output.LCM.5Alt_4classes.withCov.Bgw_estimate"
  )

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = Data

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(asc_1_a     = 3.579772,
                asc_1_b     = 2.692504,
                asc_1_c     = -0.2586175,
                asc_1_d     = -0.8976326,
                asc_2_a     = 0.9420837,
                asc_2_b     = -0.3364309,
                asc_2_c     = 0.333517,
                asc_2_d     = -0.05712442,
                asc_3_a     = 0.7293424,
                asc_3_b     = 0.1747368,
                asc_3_c     = 0.2732646,
                asc_3_d     = 0.2684315,
                asc_4_a     = 0.06505296,
                asc_4_b     = 0.4668565,
                asc_4_c     = 0.3181107,
                asc_4_d     = 0.3521881,
                #
                b_clar_a = 0.3552325,
                b_clar_b = 0.2611479,
                b_clar_c = 0.2734578,
                b_clar_d = 0.2489574,
                #
                b_fish_a = -0.02055902,
                b_fish_b = 0.8390413,
                b_fish_c = 0.2422817,
                b_fish_d = 0.5855997,
                #
                b_bio_a = 0.04314907,
                b_bio_b = 0.9061032,
                b_bio_c = 0.3345263,
                b_bio_d = 0.4908102,
                #
                b_coast_a = -0.3776539,
                b_coast_b = 0.2508964,
                b_coast_c = 0.0909121,
                b_coast_d = 0.1812021,
                #
                b_lit_a = -0.114343,
                b_lit_b = 1.287535,
                b_lit_c = 0.3706243,
                b_lit_d = 0.5100484,
                #
                b_cost_a = -0.0001005994,
                b_cost_b = 6.6752e-05,
                b_cost_c = -2.151382e-05,
                b_cost_d = -0.0001939542,
                #
                delta_a   = -0.4533784,
                delta_b   = -2.859651,
                delta_c   = 1.772702,
                delta_d   = 0.0,
                #
                gamma_age_a = -0.0008440545,
                gamma_age_b = 0.02093495,
                gamma_age_c = -0.02416032,
                gamma_age_d = 0.0,
                #
                gamma_female_a = -0.275801,
                gamma_female_b = 0.5395779,
                gamma_female_c = -0.08762988,
                gamma_female_d = 0.0,
                #
                gamma_educ2_a = 0.1557803,
                gamma_educ2_b = 1.691801,
                gamma_educ2_c = 0.05022385,
                gamma_educ2_d = 0.0,
                #
                gamma_educ3_a = -0.8095055,
                gamma_educ3_b = 1.879931,
                gamma_educ3_c = 0.2091027,
                gamma_educ3_d = 0.0,
                #
                gamma_educ4_a = 0.2645258,
                gamma_educ4_b = 2.057616,
                gamma_educ4_c = -0.7053506,
                gamma_educ4_d = 0.0 ) 

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("delta_d", "gamma_age_d", "gamma_female_d", "gamma_educ2_d", "gamma_educ3_d", "gamma_educ4_d" )

# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  
  lcpars[["asc_1"]]   = list(asc_1_a, asc_1_b, asc_1_c, asc_1_d)
  lcpars[["asc_2"]]   = list(asc_2_a, asc_2_b, asc_2_c, asc_2_d)
  lcpars[["asc_3"]]   = list(asc_3_a, asc_3_b, asc_3_c, asc_3_d)
  lcpars[["asc_4"]]   = list(asc_4_a, asc_4_b, asc_4_c, asc_4_d)
  lcpars[["b_clar"]]  = list(b_clar_a  , b_clar_b  ,b_clar_c  ,b_clar_d  )
  lcpars[["b_fish"]]  = list(b_fish_a  , b_fish_b  ,b_fish_c  ,b_fish_d  )
  lcpars[["b_bio"]]   = list(b_bio_a   , b_bio_b   ,b_bio_c   ,b_bio_d   )
  lcpars[["b_coast"]] = list(b_coast_a , b_coast_b ,b_coast_c ,b_coast_d )
  lcpars[["b_lit"]]   = list(b_lit_a   , b_lit_b   ,b_lit_c   ,b_lit_d   )
  lcpars[["b_cost"]]  = list(b_cost_a  , b_cost_b  ,b_cost_c  ,b_cost_d  )
  
  V=list()
  V[["class_a"]] = delta_a + gamma_age_a * age + gamma_female_a * female + gamma_educ2_a * educ2 + gamma_educ3_a * educ3 + + gamma_educ4_a * educ4
  V[["class_b"]] = delta_b + gamma_age_b * age + gamma_female_b * female + gamma_educ2_b * educ2 + gamma_educ3_b * educ3 + + gamma_educ4_b * educ4
  V[["class_c"]] = delta_c + gamma_age_c * age + gamma_female_c * female + gamma_educ2_c * educ2 + gamma_educ3_c * educ3 + + gamma_educ4_c * educ4
  V[["class_d"]] = delta_d + gamma_age_d * age + gamma_female_d * female + gamma_educ2_d * educ2 + gamma_educ3_d * educ3 + + gamma_educ4_d * educ4
  
  classAlloc_settings = list(
    classes      = c(class_a=1, class_b=2, class_c=3, class_d=4), 
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
    alternatives = c(alt1=1, alt2=2, alt3=3, alt4=4, alt5=5 ),
    #avail        = list(alt1=av_1, alt2=av_2, alt3=av_3, alt4=av_4, alt5=av_5, alt6=av_6),
    choiceVar    = choibio
  )
  
  ### Loop over classes
  for(s in 1:4){                     # This should be "for(s in 1:"number of classes"){" when we use "bgw" in the estimation model
    
    ### Compute class-specific utilities
    V=list()
    V[["alt1"]] = asc_1[[s]] + b_clar[[s]]*clar1 + b_fish[[s]]*fish1 + b_bio[[s]]*bio1 + b_coast[[s]]*coast1 + b_lit[[s]]*lit1 + b_cost[[s]]*cost1
    V[["alt2"]] = asc_2[[s]] + b_clar[[s]]*clar2 + b_fish[[s]]*fish2 + b_bio[[s]]*bio2 + b_coast[[s]]*coast2 + b_lit[[s]]*lit2 + b_cost[[s]]*cost2
    V[["alt3"]] = asc_3[[s]] + b_clar[[s]]*clar3 + b_fish[[s]]*fish3 + b_bio[[s]]*bio3 + b_coast[[s]]*coast3 + b_lit[[s]]*lit3 + b_cost[[s]]*cost3
    V[["alt4"]] = asc_4[[s]] + b_clar[[s]]*clar4 + b_fish[[s]]*fish4 + b_bio[[s]]*bio4 + b_coast[[s]]*coast4 + b_lit[[s]]*lit4 + b_cost[[s]]*cost4
    V[["alt5"]] =              b_clar[[s]]*clar5 + b_fish[[s]]*fish5 + b_bio[[s]]*bio5 + b_coast[[s]]*coast5 + b_lit[[s]]*lit5 + b_cost[[s]]*cost5
    

       
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


# Model name                                  : LC_5Alt_4classes_with_covariates
# Model description                           : Simple LC model, no covariates in class allocation model; The effect of number of alternatives
# Model run at                                : 2025-04-03 18:10:56.461783
# Estimation method                           : bgw
# Model diagnosis                             : Relative function convergence
# Optimisation diagnosis                      : Maximum found
# hessian properties                     : Negative definite
# maximum eigenvalue                     : -0.2705
# reciprocal of condition number         : 6.55275e-08
# Number of individuals                       : 304
# Number of rows in database                  : 2432
# Number of modelled outcomes                 : 2432
# 
# Number of cores used                        :  1 
# Model without mixing
# 
# LL(start)                                   : -2687.19
# LL (whole model) at equal shares, LL(0)     : -3914.15
# LL (whole model) at observed shares, LL(C)  : -3876.24
# LL(final, whole model)                      : -2687.19
# Rho-squared vs equal shares                  :  0.3135 
# Adj.Rho-squared vs equal shares              :  0.2986 
# Rho-squared vs observed shares               :  0.3068 
# Adj.Rho-squared vs observed shares           :  0.2959 
# AIC                                         :  5490.38 
# BIC                                         :  5826.58 
# 
# LL(0,Class_1)                    : -3914.15
# LL(final,Class_1)                : -8851.25
# LL(0,Class_2)                    : -3914.15
# LL(final,Class_2)                : -5739.14
# LL(0,Class_3)                    : -3914.15
# LL(final,Class_3)                : -3789.41
# LL(0,Class_4)                    : -3914.15
# LL(final,Class_4)                : -5036.77
# 
# Estimated parameters                        : 58
# Time taken (hh:mm:ss)                       :  00:01:9.22 
# pre-estimation                         :  00:00:5.68 
# estimation                             :  00:00:3.45 
# post-estimation                        :  00:01:0.09 
# Iterations                                  :  17  
# 
# Unconstrained optimisation.
# 
# Estimates:
#   Estimate        s.e.   t.rat.(0)    Rob.s.e. Rob.t.rat.(0)
# asc_1_a           3.578230    0.784908     4.55879    0.605225       5.91223
# asc_1_b           2.706028    0.785340     3.44568    1.066957       2.53621
# asc_1_c          -0.264666    0.268064    -0.98732    0.375412      -0.70500
# asc_1_d          -0.896462    0.272067    -3.29501    0.345976      -2.59111
# asc_2_a           0.941673    0.595403     1.58157    0.496434       1.89687
# asc_2_b          -0.334276    0.228300    -1.46419    0.261791      -1.27688
# asc_2_c           0.329515    0.123575     2.66651    0.210687       1.56401
# asc_2_d          -0.053543    0.200693    -0.26679    0.296362      -0.18067
# asc_3_a           0.731958    0.612245     1.19553    0.594033       1.23218
# asc_3_b           0.175830    0.196641     0.89417    0.216902       0.81064
# asc_3_c           0.271560    0.117119     2.31866    0.156242       1.73807
# asc_3_d           0.269450    0.176862     1.52350    0.187261       1.43890
# asc_4_a           0.065379    0.679523     0.09621    0.569172       0.11487
# asc_4_b           0.469800    0.202789     2.31669    0.224042       2.09693
# asc_4_c           0.314561    0.122916     2.55915    0.192766       1.63183
# asc_4_d           0.359381    0.215931     1.66434    0.448089       0.80203
# b_clar_a          0.354501    0.165465     2.14245    0.211416       1.67679
# b_clar_b          0.262071    0.092288     2.83972    0.135010       1.94113
# b_clar_c          0.273385    0.036881     7.41259    0.053318       5.12744
# b_clar_d          0.248519    0.050916     4.88094    0.058803       4.22628
# b_fish_a         -0.019593    0.387392    -0.05058    0.401728      -0.04877
# b_fish_b          0.842856    0.195168     4.31863    0.264550       3.18599
# b_fish_c          0.240292    0.095738     2.50989    0.153188       1.56861
# b_fish_d          0.590425    0.151616     3.89422    0.323181       1.82692
# b_bio_a           0.043605    0.156115     0.27931    0.125441       0.34761
# b_bio_b           0.907651    0.115817     7.83697    0.181440       5.00247
# b_bio_c           0.334571    0.039396     8.49256    0.060839       5.49926
# b_bio_d           0.491850    0.061856     7.95159    0.095997       5.12360
# b_coast_a        -0.376057    0.397048    -0.94713    0.397636      -0.94573
# b_coast_b         0.251943    0.158169     1.59287    0.169319       1.48798
# b_coast_c         0.091348    0.084529     1.08067    0.101580       0.89927
# b_coast_d         0.180339    0.117911     1.52946    0.130907       1.37762
# b_lit_a          -0.114238    0.183516    -0.62250    0.184129      -0.62043
# b_lit_b           1.287698    0.128725    10.00345    0.194643       6.61569
# b_lit_c           0.372037    0.046705     7.96567    0.097911       3.79974
# b_lit_d           0.509504    0.055534     9.17463    0.081653       6.23986
# b_cost_a         -0.010075    0.003416    -2.94930    0.004352      -2.31513
# b_cost_b          0.006694    0.001276     5.24390    0.001733       3.86227
# b_cost_c         -0.002172  8.1678e-04    -2.65922    0.001828      -1.18827
# b_cost_d         -0.019449    0.001803   -10.78884    0.003701      -5.25566
# delta_a          -0.452831    1.078471    -0.41988    1.191914      -0.37992
# delta_b          -2.860256    1.255061    -2.27898    1.338358      -2.13714
# delta_c           1.766254    0.860403     2.05282    0.981141       1.80020
# delta_d           0.000000          NA          NA          NA            NA
# gamma_age_a    -7.6705e-04    0.018369    -0.04176    0.020178      -0.03801
# gamma_age_b       0.021029    0.016413     1.28124    0.017043       1.23385
# gamma_age_c      -0.023958    0.015412    -1.55456    0.020704      -1.15716
# gamma_age_d       0.000000          NA          NA          NA            NA
# gamma_female_a   -0.273745    0.427638    -0.64013    0.445920      -0.61389
# gamma_female_b    0.540721    0.368247     1.46837    0.385245       1.40358
# gamma_female_c   -0.080277    0.363327    -0.22095    0.432602      -0.18557
# gamma_female_d    0.000000          NA          NA          NA            NA
# gamma_educ2_a     0.155990    0.607914     0.25660    0.622251       0.25069
# gamma_educ2_b     1.692962    0.878843     1.92635    0.931434       1.81759
# gamma_educ2_c     0.051917    0.506774     0.10245    0.531718       0.09764
# gamma_educ2_d     0.000000          NA          NA          NA            NA
# gamma_educ3_a    -0.806622    0.846914    -0.95243    0.896247      -0.90000
# gamma_educ3_b     1.874950    0.939459     1.99578    0.998861       1.87709
# gamma_educ3_c     0.215104    0.593983     0.36214    0.626467       0.34336
# gamma_educ3_d     0.000000          NA          NA          NA            NA
# gamma_educ4_a     0.259933    0.663693     0.39165    0.686846       0.37844
# gamma_educ4_b     2.054655    0.913555     2.24908    0.988482       2.07860
# gamma_educ4_c    -0.708614    0.611909    -1.15804    0.643997      -1.10034
# gamma_educ4_d     0.000000          NA          NA          NA            NA
# 
# 
# Summary of class allocation for model component :
#   Mean prob.
# Class_1      0.1252
# Class_2      0.2631
# Class_3      0.3890
# Class_4      0.2226

