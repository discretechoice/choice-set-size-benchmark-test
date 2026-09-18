####################################################################
####################################################################
#####
#####           RPL correlated, no socio-dem - 6 alternatives - treatment 5
#####
####################################################################
####################################################################


################################################
# 
# The script: RPL ESTIMATES WITH 6 ALTERNATIVES
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
  modelName       = "RPL_corr_CostLogNormal_treatment_6alt",
  modelDescr      = "Correlated RPL model; The effect of number of alternatives",
  indivID         = "id",  
  mixing          = TRUE,
  nCores          = 4,
  outputDirectory = "output.RPL.corr.CostLogNormal6alt"
)


# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = Data

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(mu_asc1        = 0.2260,
              mu_asc2        = 0.3340,
              mu_asc3        = 0.4490,
              mu_asc4        = 0.1870,
              mu_asc5        = 0.0477,
              # Mean parameters
              mu_clar      =  0.601645, 
              mu_fish      =  1.722720, 
              mu_bio       =  0.489784, 
              mu_coast     = -0.005196, 
              mu_lit       =  0.846038, 
              mu_cost      = -4.078199, 
              # Cholesky 
              sd_asc1        = 0.20018,
              sd_asc2        = 0.20018,
              sd_asc3        = 0.20018,
              sd_asc4        = 0.20018,
              sd_asc5        = 0.20018,
              sd_clar        = 0.08223, 
              sd_fish        = 0.22340, 
              sd_bio         = 0.07842, 
              sd_coast       = 0.15984,
              sd_lit         = 0.09826,  
              sd_cost        = 0.12489,
              
              sd_asc1_asc2   = rnorm(1),
              sd_asc1_asc3   = rnorm(1),
              sd_asc1_asc4   = rnorm(1),
              sd_asc1_asc5   = rnorm(1),
              sd_asc1_clar   = rnorm(1),
              sd_asc1_fish   = rnorm(1),  
              sd_asc1_bio    = rnorm(1),  
              sd_asc1_coast  = rnorm(1),
              sd_asc1_lit    = rnorm(1),
              sd_asc1_cost   = rnorm(1),
              
              sd_asc2_asc3   = rnorm(1),
              sd_asc2_asc4   = rnorm(1),
              sd_asc2_asc5   = rnorm(1),
              sd_asc2_clar   = rnorm(1),
              sd_asc2_fish   = rnorm(1),  
              sd_asc2_bio    = rnorm(1),  
              sd_asc2_coast  = rnorm(1),
              sd_asc2_lit    = rnorm(1),
              sd_asc2_cost   = rnorm(1),
              
              sd_asc3_asc4   = rnorm(1),
              sd_asc3_asc5   = rnorm(1),
              sd_asc3_clar   = rnorm(1),
              sd_asc3_fish   = rnorm(1),  
              sd_asc3_bio    = rnorm(1),  
              sd_asc3_coast  = rnorm(1),
              sd_asc3_lit    = rnorm(1),
              sd_asc3_cost   = rnorm(1),
              
              sd_asc4_asc5   = rnorm(1),
              sd_asc4_clar   = rnorm(1),
              sd_asc4_fish   = rnorm(1),  
              sd_asc4_bio    = rnorm(1),  
              sd_asc4_coast  = rnorm(1),
              sd_asc4_lit    = rnorm(1),
              sd_asc4_cost   = rnorm(1),
              
              sd_asc5_clar   = rnorm(1),
              sd_asc5_fish   = rnorm(1),  
              sd_asc5_bio    = rnorm(1),  
              sd_asc5_coast  = rnorm(1),
              sd_asc5_lit    = rnorm(1),
              sd_asc5_cost   = rnorm(1),
              
              sd_clar_fish   = rnorm(1),  
              sd_clar_bio    = rnorm(1),  
              sd_clar_coast  = rnorm(1),  
              sd_clar_lit    = rnorm(1),  
              sd_clar_cost   = rnorm(1),
              
              sd_fish_bio    = rnorm(1),  
              sd_fish_coast  = rnorm(1),  
              sd_fish_lit    = rnorm(1),  
              sd_fish_cost   = rnorm(1),
              
              sd_bio_coast   = rnorm(1),  
              sd_bio_lit     = rnorm(1),  
              sd_bio_cost    = rnorm(1),
              
              sd_coast_lit    = rnorm(1),
              sd_coast_cost   = rnorm(1),
              
              sd_lit_cost     = rnorm(1))

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  # interNDraws    = 500,
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_asc1","draws_asc2","draws_asc3","draws_asc4","draws_asc5","draws_clar","draws_fish","draws_bio","draws_coast","draws_lit","draws_cost"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_asc1"]]    =       ( mu_asc1    + sd_asc1        * draws_asc1 )
  randcoeff[["b_asc2"]]    =       ( mu_asc2    + sd_asc1_asc2   * draws_asc1 + sd_asc2        * draws_asc2 )
  randcoeff[["b_asc3"]]    =       ( mu_asc3    + sd_asc1_asc3   * draws_asc1 + sd_asc2_asc3   * draws_asc2 + sd_asc3        * draws_asc3 )
  randcoeff[["b_asc4"]]    =       ( mu_asc4    + sd_asc1_asc4   * draws_asc1 + sd_asc2_asc4   * draws_asc2 + sd_asc3_asc4   * draws_asc3 + sd_asc4        * draws_asc4 )
  randcoeff[["b_asc5"]]    =       ( mu_asc5    + sd_asc1_asc5   * draws_asc1 + sd_asc2_asc5   * draws_asc2 + sd_asc3_asc5   * draws_asc3 + sd_asc4_asc5   * draws_asc4 + sd_asc5        * draws_asc5 )
  randcoeff[["b_clar"]]    =       ( mu_clar    + sd_asc1_clar   * draws_asc1 + sd_asc2_clar   * draws_asc2 + sd_asc3_clar   * draws_asc3 + sd_asc4_clar   * draws_asc4 + sd_asc5_clar   * draws_asc5 + sd_clar        * draws_clar )
  randcoeff[["b_fish"]]    =       ( mu_fish    + sd_asc1_fish   * draws_asc1 + sd_asc2_fish   * draws_asc2 + sd_asc3_fish   * draws_asc3 + sd_asc4_fish   * draws_asc4 + sd_asc5_fish   * draws_asc5 + sd_clar_fish   * draws_clar + sd_fish        * draws_fish )
  randcoeff[["b_bio"]]     =       ( mu_bio     + sd_asc1_bio    * draws_asc1 + sd_asc2_bio    * draws_asc2 + sd_asc3_bio    * draws_asc3 + sd_asc4_bio    * draws_asc4 + sd_asc5_bio    * draws_asc5 + sd_clar_bio    * draws_clar + sd_fish_bio    * draws_fish + sd_bio            * draws_bio)
  randcoeff[["b_coast"]]   =       ( mu_coast   + sd_asc1_coast  * draws_asc1 + sd_asc2_coast  * draws_asc2 + sd_asc3_coast  * draws_asc3 + sd_asc4_coast  * draws_asc4 + sd_asc5_coast  * draws_asc5 + sd_clar_coast  * draws_clar + sd_fish_coast  * draws_fish + sd_bio_coast      * draws_bio + sd_coast      * draws_coast)
  randcoeff[["b_lit"]]     =       ( mu_lit     + sd_asc1_lit    * draws_asc1 + sd_asc2_lit    * draws_asc2 + sd_asc3_lit    * draws_asc3 + sd_asc4_lit    * draws_asc4 + sd_asc5_lit    * draws_asc5 + sd_clar_lit    * draws_clar + sd_fish_lit    * draws_fish + sd_bio_lit        * draws_bio + sd_coast_lit  * draws_coast + sd_lit       * draws_lit)
  randcoeff[["b_cost"]]    = -exp( ( mu_cost    + sd_asc1_cost   * draws_asc1 + sd_asc2_cost   * draws_asc2 + sd_asc3_cost   * draws_asc3 + sd_asc4_cost   * draws_asc4 + sd_asc5_cost   * draws_asc5 + sd_clar_cost   * draws_clar + sd_fish_cost   * draws_fish + sd_bio_cost       * draws_bio + sd_coast_cost * draws_coast + sd_lit_cost  * draws_lit + sd_cost       * draws_cost) )  # if "b_cost" is log-normally distributed
  
  return(randcoeff)
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
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs,
                        estimate_settings = list(writeIter = FALSE,
                                                 silent    = FALSE,
                                                 maxIterations = 500,
                                                 #iterMax   = 500,
                                                 estimationRoutine = "bgw"))

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)



# Model run by bcpareca using Apollo 0.3.4 on R 4.4.2 for Windows.
# Please acknowledge the use of Apollo by citing Hess & Palma (2019)
# DOI 10.1016/j.jocm.2019.100170
# www.ApolloChoiceModelling.com
# 
# Model name                                  : RPL_corr_CostLogNormal_treatment_6alt
# Model description                           : Correlated RPL model; The effect of number of alternatives
# Model run at                                : 2026-02-05 17:17:29.758017
# Estimation method                           : bgw
# Model diagnosis                             : Relative function convergence
# Optimisation diagnosis                      : Maximum found
# hessian properties                     : Negative definite
# maximum eigenvalue                     : -2.066188
# reciprocal of condition number         : 0.0019628
# Number of individuals                       : 281
# Number of rows in database                  : 2248
# Number of modelled outcomes                 : 2248
# 
# Number of cores used                        :  4 
# Number of inter-individual draws            : 1000 (halton)
# 
# LL(start)                                   : -3471.5
# LL at equal shares, LL(0)                   : -4027.88
# LL at observed shares, LL(C)                : -4007.2
# LL(final)                                   : -2648.95
# Rho-squared vs equal shares                  :  0.3423 
# Adj.Rho-squared vs equal shares              :  0.3232 
# Rho-squared vs observed shares               :  0.339 
# Adj.Rho-squared vs observed shares           :  0.321 
# AIC                                         :  5451.91 
# BIC                                         :  5892.18 
# 
# Estimated parameters                        : 77
# Time taken (hh:mm:ss)                       :  03:23:34.12 
# pre-estimation                         :  00:02:8.7 
# estimation                             :  00:23:44.04 
# post-estimation                        :  02:57:41.38 
# Iterations                                  :  55  
# 
# Unconstrained optimisation.
# 
# Estimates:
#   Estimate        s.e.   t.rat.(0)    Rob.s.e. Rob.t.rat.(0)
# mu_asc1         -1.074161     0.41441    -2.59200     0.38489      -2.79082
# mu_asc2          0.110077     0.12874     0.85502     0.12746       0.86365
# mu_asc3          0.369245     0.12683     2.91130     0.12528       2.94736
# mu_asc4          0.427446     0.12963     3.29739     0.12768       3.34782
# mu_asc5          0.110221     0.12835     0.85877     0.11594       0.95063
# mu_clar          0.249632     0.04288     5.82166     0.04084       6.11269
# mu_fish          0.781123     0.10342     7.55294     0.09995       7.81539
# mu_bio           0.696729     0.05661    12.30753     0.05964      11.68273
# mu_coast        -0.001183     0.09379    -0.01262     0.08574      -0.01380
# mu_lit           0.732562     0.05673    12.91372     0.05897      12.42237
# mu_cost         -8.379574     0.42366   -19.77919     0.36185     -23.15781
# sd_asc1          2.862239     0.41351     6.92186     0.41680       6.86721
# sd_asc2          0.753862     0.18043     4.17809     0.24276       3.10542
# sd_asc3         -0.664382     0.16505    -4.02544     0.24295      -2.73470
# sd_asc4         -0.212165     0.15930    -1.33188     0.17007      -1.24753
# sd_asc5          0.138718     0.14874     0.93261     0.12937       1.07224
# sd_clar         -0.015196     0.06595    -0.23042     0.07495      -0.20274
# sd_fish         -0.239521     0.14627    -1.63749     0.11087      -2.16039
# sd_bio           0.297157     0.08247     3.60323     0.08843       3.36020
# sd_coast         0.460372     0.14391     3.19910     0.12764       3.60676
# sd_lit           0.041186     0.08968     0.45925     0.09602       0.42893
# sd_cost          0.182238     0.10736     1.69738     0.07271       2.50653
# sd_asc1_asc2    -0.368619     0.19986    -1.84441     0.20239      -1.82132
# sd_asc1_asc3     0.072039     0.21961     0.32804     0.24146       0.29835
# sd_asc1_asc4    -0.136148     0.21218    -0.64165     0.19432      -0.70065
# sd_asc1_asc5    -0.413014     0.18271    -2.26045     0.14730      -2.80381
# sd_asc1_clar     0.053072     0.07104     0.74702     0.08183       0.64856
# sd_asc1_fish    -0.031333     0.16641    -0.18829     0.19900      -0.15745
# sd_asc1_bio      0.182922     0.07014     2.60812     0.06861       2.66611
# sd_asc1_coast    0.090530     0.14114     0.64143     0.15394       0.58809
# sd_asc1_lit      0.091754     0.07120     1.28875     0.06784       1.35251
# sd_asc1_cost     0.710149     0.15075     4.71089     0.14214       4.99629
# sd_asc2_asc3     0.475918     0.20222     2.35350     0.26898       1.76931
# sd_asc2_asc4     0.113223     0.18805     0.60209     0.19095       0.59296
# sd_asc2_asc5    -0.114828     0.17028    -0.67435     0.14703      -0.78100
# sd_asc2_clar     0.079106     0.05471     1.44599     0.05433       1.45595
# sd_asc2_fish     0.050329     0.13598     0.37012     0.12055       0.41748
# sd_asc2_bio     -0.194028     0.05685    -3.41289     0.05406      -3.58896
# sd_asc2_coast   -0.214853     0.12877    -1.66854     0.14601      -1.47152
# sd_asc2_lit     -0.303283     0.06384    -4.75048     0.05456      -5.55839
# sd_asc2_cost     2.067593     0.21456     9.63656     0.19849      10.41648
# sd_asc3_asc4    -0.889640     0.13230    -6.72461     0.14337      -6.20529
# sd_asc3_asc5    -0.429530     0.13422    -3.20018     0.10663      -4.02810
# sd_asc3_clar     0.241911     0.05318     4.54931     0.05083       4.75893
# sd_asc3_fish     0.103507     0.12165     0.85084     0.10021       1.03293
# sd_asc3_bio      0.259337     0.05351     4.84666     0.04189       6.19122
# sd_asc3_coast    0.158417     0.11445     1.38413     0.10423       1.51983
# sd_asc3_lit      0.396461     0.05672     6.98975     0.04886       8.11469
# sd_asc3_cost     0.152293     0.08985     1.69504     0.07809       1.95035
# sd_asc4_asc5    -0.094790     0.15510    -0.61115     0.17468      -0.54265
# sd_asc4_clar    -0.292624     0.05250    -5.57400     0.05277      -5.54554
# sd_asc4_fish     0.055934     0.15662     0.35713     0.16072       0.34803
# sd_asc4_bio      0.187041     0.05784     3.23404     0.05516       3.39061
# sd_asc4_coast    0.106054     0.13252     0.80031     0.13816       0.76764
# sd_asc4_lit      0.028717     0.08427     0.34076     0.10412       0.27580
# sd_asc4_cost    -0.579723     0.08775    -6.60636     0.06933      -8.36208
# sd_asc5_clar    -0.187594     0.05912    -3.17290     0.05540      -3.38635
# sd_asc5_fish    -0.439566     0.16800    -2.61644     0.21397      -2.05430
# sd_asc5_bio      0.054799     0.08617     0.63592     0.11698       0.46845
# sd_asc5_coast   -0.232683     0.14028    -1.65876     0.12782      -1.82038
# sd_asc5_lit      0.249701     0.07458     3.34804     0.07790       3.20535
# sd_asc5_cost     3.382075     0.24196    13.97755     0.19544      17.30510
# sd_clar_fish     0.821826     0.13127     6.26050     0.13284       6.18652
# sd_clar_bio      0.186235     0.06368     2.92474     0.06966       2.67366
# sd_clar_coast   -0.404485     0.14399    -2.80913     0.17464      -2.31610
# sd_clar_lit      0.261125     0.06762     3.86187     0.07488       3.48742
# sd_clar_cost    -2.505765     0.21963   -11.40886     0.19817     -12.64463
# sd_fish_bio     -0.326458     0.05458    -5.98144     0.04470      -7.30256
# sd_fish_coast   -0.176690     0.14264    -1.23868     0.11549      -1.52993
# sd_fish_lit     -0.028832     0.07053    -0.40878     0.06119      -0.47119
# sd_fish_cost    -0.594930     0.08603    -6.91520     0.05820     -10.22183
# sd_bio_coast     0.259238     0.16431     1.57775     0.15368       1.68689
# sd_bio_lit       0.006753     0.11659     0.05793     0.15371       0.04394
# sd_bio_cost     -3.195859     0.31664   -10.09319     0.32178      -9.93194
# sd_coast_lit     0.127707     0.08744     1.46058     0.09464       1.34933
# sd_coast_cost    0.175160     0.12445     1.40749     0.11428       1.53275
# sd_lit_cost      2.583424     0.23678    10.91083     0.21921      11.78489

#######
## SAVE RESULTS
#######

apollo_saveOutput(model)



#######
##  STANDARD DEVIATIONS OF THE RANDOM PARAMETERS AND THEIR STANDARD ERRORS
#######

choleski.cov <- t(matrix (c(model$estimate["sd_asc1"]      , 0                              , 0                             , 0                              , 0                              , 0                              , 0                               , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_asc2"] , model$estimate["sd_asc2"]      , 0                             , 0                              , 0                              , 0                              , 0                               , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_asc3"] , model$estimate["sd_asc2_asc3"] ,model$estimate["sd_asc3"]      , 0                              , 0                              , 0                              , 0                               , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_asc4"] , model$estimate["sd_asc2_asc4"] ,model$estimate["sd_asc3_asc4"] , model$estimate["sd_asc4"]      , 0                              , 0                              , 0                               , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_asc5"] , model$estimate["sd_asc2_asc5"] ,model$estimate["sd_asc3_asc5"] , model$estimate["sd_asc4_asc5"] , model$estimate["sd_asc5"]      , 0                              , 0                               , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_clar"] , model$estimate["sd_asc2_clar"] ,model$estimate["sd_asc3_clar"] , model$estimate["sd_asc4_clar"] , model$estimate["sd_asc5_clar"] , model$estimate["sd_clar"]      , 0                               , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_fish"] , model$estimate["sd_asc2_fish"] ,model$estimate["sd_asc3_fish"] , model$estimate["sd_asc4_fish"] , model$estimate["sd_asc5_fish"] , model$estimate["sd_clar_fish"] , model$estimate["sd_fish"]       , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_bio"]  , model$estimate["sd_asc2_bio"]  ,model$estimate["sd_asc3_bio"]  , model$estimate["sd_asc4_bio"]  , model$estimate["sd_asc5_bio"]  , model$estimate["sd_clar_bio"]  , model$estimate["sd_fish_bio"]   , model$estimate["sd_bio"]      , 0                              , 0                             , 0                        ,     
                            model$estimate["sd_asc1_coast"], model$estimate["sd_asc2_coast"],model$estimate["sd_asc3_coast"], model$estimate["sd_asc4_coast"], model$estimate["sd_asc5_coast"], model$estimate["sd_clar_coast"], model$estimate["sd_fish_coast"] , model$estimate["sd_bio_coast"], model$estimate["sd_coast"]     , 0                             , 0                        ,
                            model$estimate["sd_asc1_lit"]  , model$estimate["sd_asc2_lit"]  ,model$estimate["sd_asc3_lit"]  , model$estimate["sd_asc4_lit"]  , model$estimate["sd_asc5_lit"]  , model$estimate["sd_clar_lit"]  , model$estimate["sd_fish_lit"]   , model$estimate["sd_bio_lit"]  , model$estimate["sd_coast_lit"] , model$estimate["sd_lit"]      , 0                        ,      
                            model$estimate["sd_asc1_cost"] , model$estimate["sd_asc2_cost"] ,model$estimate["sd_asc3_cost"] , model$estimate["sd_asc4_cost"] , model$estimate["sd_asc5_cost"] , model$estimate["sd_clar_cost"] , model$estimate["sd_fish_cost"]  , model$estimate["sd_bio_cost"] , model$estimate["sd_coast_cost"], model$estimate["sd_lit_cost"] , model$estimate["sd_cost"]), 11))

colnames(choleski.cov) <- c("mu_asc1","mu_asc2","mu_asc3","mu_asc4","mu_asc5","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
rownames(choleski.cov) <- c("mu_asc1","mu_asc2","mu_asc3","mu_asc4","mu_asc5","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
choleski.cov
#              mu_asc1     mu_asc2    mu_asc3     mu_asc4     mu_asc5     mu_clar     mu_fish       mu_bio  mu_coast     mu_lit  mu_cost
# mu_asc1   2.86223926  0.00000000  0.0000000  0.00000000  0.00000000  0.00000000  0.00000000  0.000000000 0.0000000 0.00000000 0.000000
# mu_asc2  -0.36861889  0.75386185  0.0000000  0.00000000  0.00000000  0.00000000  0.00000000  0.000000000 0.0000000 0.00000000 0.000000
# mu_asc3   0.07203925  0.47591765 -0.6643825  0.00000000  0.00000000  0.00000000  0.00000000  0.000000000 0.0000000 0.00000000 0.000000
# mu_asc4  -0.13614824  0.11322278 -0.8896398 -0.21216474  0.00000000  0.00000000  0.00000000  0.000000000 0.0000000 0.00000000 0.000000
# mu_asc5  -0.41301400 -0.11482773 -0.4295302 -0.09479006  0.13871805  0.00000000  0.00000000  0.000000000 0.0000000 0.00000000 0.000000
# mu_clar   0.05307181  0.07910551  0.2419113 -0.29262355 -0.18759411 -0.01519621  0.00000000  0.000000000 0.0000000 0.00000000 0.000000
# mu_fish  -0.03133303  0.05032925  0.1035067  0.05593421 -0.43956554  0.82182607 -0.23952071  0.000000000 0.0000000 0.00000000 0.000000
# mu_bio    0.18292175 -0.19402754  0.2593367  0.18704051  0.05479867  0.18623454 -0.32645826  0.297156543 0.0000000 0.00000000 0.000000
# mu_coast  0.09052963 -0.21485271  0.1584172  0.10605423 -0.23268350 -0.40448526 -0.17668963  0.259237622 0.4603720 0.00000000 0.000000
# mu_lit    0.09175350 -0.30328323  0.3964606  0.02871695  0.24970053  0.26112496 -0.02883195  0.006753439 0.1277073 0.04118619 0.000000
# mu_cost   0.71014930  2.06759343  0.1522935 -0.57972313  3.38207467 -2.50576462 -0.59492958 -3.195859016 0.1751604 2.58342437 0.182238

var.cov           <- choleski.cov %*% t(choleski.cov)  # variance-covariance matriz of the random parameters
colnames(var.cov) <- c("mu_asc1","mu_asc2","mu_asc3","mu_asc4","mu_asc5","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
rownames(var.cov) <- c("mu_asc1","mu_asc2","mu_asc3","mu_asc4","mu_asc5","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
var.cov             # variance-covariance matriz of the random parameters

# mu_asc1     mu_asc2     mu_asc3    mu_asc4     mu_asc5     mu_clar     mu_fish      mu_bio    mu_coast      mu_lit     mu_cost
# mu_asc1   8.19241355 -1.05507546  0.20619357 -0.3896888 -1.18214488  0.15190423 -0.08968262  0.52356583  0.25911747  0.26262047  2.03261721
# mu_asc2  -1.05507546  0.70418758  0.33222113  0.1355411  0.06568051  0.04007135  0.04949124 -0.21369837 -0.19534020 -0.26245573  1.29690537
# mu_asc3   0.20619357  0.33222113  0.67309132  0.6351378  0.20097060 -0.11925069 -0.04707267 -0.25146236 -0.20098013 -0.40112948  0.93398171
# mu_asc4  -0.38968884  0.13554115  0.63513778  0.8678286  0.44546832 -0.15139863 -0.09398660 -0.31727248 -0.20008690 -0.40563052  0.12492355
# mu_asc5  -1.18214488  0.06568051  0.20097060  0.4454683  0.39649006 -0.13319603 -0.10357516 -0.17479049 -0.12309423 -0.14144610 -0.07202653
# mu_clar   0.15190423  0.04007135 -0.11925069 -0.1513986 -0.13319603  0.18864641  0.08096146 -0.01074659  0.04489419  0.01757276 -0.18865006
# mu_fish  -0.08968262  0.04949124 -0.04707267 -0.0939866 -0.10357516  0.08096146  0.94334320  0.22896661 -0.17913670  0.13624906 -3.33830190
# mu_bio    0.52356583 -0.21369837 -0.25146236 -0.3172725 -0.17479049 -0.01074659  0.22896661  0.40590995  0.16580309  0.25755004 -1.37698217
# mu_coast  0.25911747 -0.19534020 -0.20098013 -0.2000869 -0.12309423  0.04489419 -0.17913670  0.16580309  0.61881652  0.04123492 -0.83343284
# mu_lit    0.26262047 -0.26245573 -0.40112948 -0.4056305 -0.14144610  0.01757276  0.13624906  0.25755004  0.04123492  0.40782407 -0.20364845
# mu_cost   2.03261721  1.29690537  0.93398171  0.1249235 -0.07202653 -0.18865006 -3.33830190 -1.37698217 -0.83343284 -0.20364845 40.16124166

sd.rand.param     <- sqrt(diag(var.cov))               # standard deviations of of the random parameters
sd.rand.param
# mu_asc1   mu_asc2   mu_asc3   mu_asc4   mu_asc5   mu_clar   mu_fish    mu_bio  mu_coast    mu_lit   mu_cost 
# 2.8622393 0.8391589 0.8204214 0.9315732 0.6296746 0.4343344 0.9712586 0.6371106 0.7866489 0.6386110 6.3372898 

## Standard deviation of the random parameters and their standard errors ----
deltaMethod_settings <-
  list(
    expression = c(
      rob_se_sd_asc1  = "sqrt(sd_asc1^2 )",       
      rob_se_sd_asc2  = "sqrt(sd_asc1_asc2^2   + sd_asc2^2)",        
      rob_se_sd_asc3  = "sqrt(sd_asc1_asc3^2   + sd_asc2_asc3^2   + sd_asc3^2)",        
      rob_se_sd_asc4  = "sqrt(sd_asc1_asc4^2   + sd_asc2_asc4^2   + sd_asc3_asc4^2   + sd_asc4^2)",        
      rob_se_sd_asc5  = "sqrt(sd_asc1_asc5^2   + sd_asc2_asc5^2   + sd_asc3_asc5^2   + sd_asc4_asc5^2   + sd_asc5^2)",        
      rob_se_sd_clar  = "sqrt(sd_asc1_clar^2   + sd_asc2_clar^2   + sd_asc3_clar^2   + sd_asc4_clar^2   + sd_asc5_clar^2   + sd_clar^2)",        
      rob_se_sd_fish  = "sqrt(sd_asc1_fish^2   + sd_asc2_fish^2   + sd_asc3_fish^2   + sd_asc4_fish^2   + sd_asc5_fish^2   + sd_clar_fish^2   + sd_fish^2)",       
      rob_se_sd_bio   = "sqrt(sd_asc1_bio^2    + sd_asc2_bio^2    + sd_asc3_bio^2    + sd_asc4_bio^2    + sd_asc5_bio^2    + sd_clar_bio^2    + sd_fish_bio^2    + sd_bio^2)",            
      rob_se_sd_coast = "sqrt(sd_asc1_coast^2  + sd_asc2_coast^2  + sd_asc3_coast^2  + sd_asc4_coast^2  + sd_asc5_coast^2  + sd_clar_coast^2  + sd_fish_coast^2  + sd_bio_coast^2      + sd_coast^2)",      
      rob_se_sd_lit   = "sqrt(sd_asc1_lit^2    + sd_asc2_lit^2    + sd_asc3_lit^2    + sd_asc4_lit^2    + sd_asc5_lit^2    + sd_clar_lit^2    + sd_fish_lit^2    + sd_bio_lit^2        + sd_coast_lit^2  + sd_lit^2)",      
      rob_se_sd_cost  = "sqrt(sd_asc1_cost^2   + sd_asc2_cost^2   + sd_asc3_cost^2   + sd_asc4_cost^2   + sd_asc5_cost^2   + sd_clar_cost^2   + sd_fish_cost^2   + sd_bio_cost^2       + sd_coast_cost^2 + sd_lit_cost^2 + sd_cost^2)"      
    ),
    varcov="robust"
  )


est_sd <- apollo_deltaMethod(model, deltaMethod_settings)
est_sd      # standard deviations of the random parameters and their standar errors 
#         Expression  Value   s.e. t-ratio (0)
# 1   rob_se_sd_asc1 2.8622 0.4168        6.87
# 2   rob_se_sd_asc2 0.8392 0.2245        3.74
# 3   rob_se_sd_asc3 0.8204 0.1944        4.22
# 4   rob_se_sd_asc4 0.9316 0.1423        6.54
# 5   rob_se_sd_asc5 0.6297 0.1294        4.87
# 6   rob_se_sd_clar 0.4343 0.0563        7.71
# 7   rob_se_sd_fish 0.9713 0.1416        6.86
# 8    rob_se_sd_bio 0.6371 0.0617       10.32
# 9  rob_se_sd_coast 0.7866 0.1378        5.71
# 10   rob_se_sd_lit 0.6386 0.0564       11.33
# 11  rob_se_sd_cost 6.3373 0.4877       13.00


