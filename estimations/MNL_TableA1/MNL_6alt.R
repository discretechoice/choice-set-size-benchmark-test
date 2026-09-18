####################################################################
####################################################################
#####
#####           MNL - 6 alternatives - treatment 5
#####
####################################################################
####################################################################


################################################
# 
# The script: MNL ESTIMATES WITH 6 ALTERNATIVES
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
choose.treatment <- 5   ## the treatment that we choose to test
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
n.alternatives         <- choose.treatment + 1           # the number of alternatives in the treatment that we are testing is equal to "choose.treatment + 1"
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


# ############################################# #
####      INITIALISE APOLLO                  ####
# ############################################# #

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MNL_treatment_6alt",
  modelDescr      = "MNL model; The effect of number of alternatives",
  indivID         = "id",  
  # mixing          = TRUE,
  nCores          = 4,
  outputDirectory = "output.MNL6alt"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = Data

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

apollo_beta=c(b_asc1        = 0,
              b_asc2        = 0,
              b_asc3        = 0,
              b_asc4        = 0,
              b_asc5        = 0,
              # Mean parameters
              b_clar      =  0, 
              b_fish      =  0, 
              b_bio       =  0, 
              b_coast     =  0, 
              b_lit       =  0, 
              b_cost      =  0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

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
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["alt1"]]  = b_asc1  + b_clar * clar1 + b_fish * fish1 + b_bio * bio1 + b_coast * coast1 + b_lit * lit1 + b_cost * cost1 
  V[["alt2"]]  = b_asc2  + b_clar * clar2 + b_fish * fish2 + b_bio * bio2 + b_coast * coast2 + b_lit * lit2 + b_cost * cost2 
  V[["alt3"]]  = b_asc3  + b_clar * clar3 + b_fish * fish3 + b_bio * bio3 + b_coast * coast3 + b_lit * lit3 + b_cost * cost3 
  V[["alt4"]]  = b_asc4  + b_clar * clar4 + b_fish * fish4 + b_bio * bio4 + b_coast * coast4 + b_lit * lit4 + b_cost * cost4 
  V[["alt5"]]  = b_asc5  + b_clar * clar5 + b_fish * fish5 + b_bio * bio5 + b_coast * coast5 + b_lit * lit5 + b_cost * cost5 
  V[["alt6"]]  =           b_clar * clar6 + b_fish * fish6 + b_bio * bio6 + b_coast * coast6 + b_lit * lit6 + b_cost * cost6 
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3, alt4=4, alt5=5, alt6=6), 
    # avail         = list(alt1=1, alt2=2, alt3=3, alt4=4, alt5=5, alt6=6), 
    choiceVar     = choibio,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)


# Model name                                  : MNL_treatment_6alt
# Model description                           : MNL model; The effect of number of alternatives
# Model run at                                : 2026-02-04 16:18:27.426112
# Estimation method                           : bgw
# Model diagnosis                             : Relative function convergence
# Optimisation diagnosis                      : Maximum found
# hessian properties                     : Negative definite
# maximum eigenvalue                     : -42.910156
# reciprocal of condition number         : 2.21264e-06
# Number of individuals                       : 281
# Number of rows in database                  : 2248
# Number of modelled outcomes                 : 2248
# 
# Number of cores used                        :  4 
# Model without mixing
# 
# LL(start)                                   : -4027.88
# LL at equal shares, LL(0)                   : -4027.88
# LL at observed shares, LL(C)                : -4007.2
# LL(final)                                   : -3536.44
# Rho-squared vs equal shares                  :  0.122 
# Adj.Rho-squared vs equal shares              :  0.1193 
# Rho-squared vs observed shares               :  0.1175 
# Adj.Rho-squared vs observed shares           :  0.116 
# AIC                                         :  7094.88 
# BIC                                         :  7157.78 
# 
# Estimated parameters                        : 11
# Time taken (hh:mm:ss)                       :  00:00:10.57 
# pre-estimation                         :  00:00:8.84 
# estimation                             :  00:00:0.42 
# post-estimation                        :  00:00:1.32 
# Iterations                                  :  8  
# 
# Unconstrained optimisation.
# 
# Estimates:
#   Estimate        s.e.   t.rat.(0)    Rob.s.e. Rob.t.rat.(0)
# b_asc1     1.626978     0.11745      13.852     0.18478         8.805
# b_asc2     0.186757     0.07991       2.337     0.07460         2.503
# b_asc3     0.197735     0.07957       2.485     0.07951         2.487
# b_asc4     0.303448     0.07846       3.868     0.08006         3.790
# b_asc5     0.153245     0.07945       1.929     0.07152         2.143
# b_clar     0.182195     0.02200       8.280     0.02599         7.010
# b_fish     0.622292     0.05115      12.165     0.05631        11.051
# b_bio      0.426048     0.02152      19.801     0.02638        16.152
# b_coast    0.134939     0.05117       2.637     0.05439         2.481
# b_lit      0.471473     0.02114      22.305     0.02921        16.139
# b_cost    -0.003487  3.2285e-04     -10.800  5.2286e-04        -6.668


#######
## SAVE RESULTS
#######

apollo_saveOutput(model)

