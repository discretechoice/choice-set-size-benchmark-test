####################################################################
####################################################################
#####
#####            RPL correlated, no socio-dem - 2 alternatives - treatment 1
#####
####################################################################
####################################################################


################################################
# 
# The script: RPL ESTIMATES WITH 2 ALTERNATIVES
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
  modelName       = "RPL_corr_CostLogNormal_treatment_2alt",
  modelDescr      = "Correlated RPL model; The effect of number of alternatives",
  indivID         = "id",  
  mixing          = TRUE,
  nCores          = 4,
  outputDirectory = "output.RPL.corr.CostLogNormal.2alt"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = Data

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(# Alternative specific constants
              mu_asc1        = 0.547408,
              # Mean parameters
              mu_clar      =  0.601645, 
              mu_fish      =  1.722720, 
              mu_bio       =  0.489784, 
              mu_coast     = -0.005196, 
              mu_lit       =  0.846038, 
              mu_cost      = -4.078199, 
              # Cholesky 
              sd_asc1        = 0.20018, 
              sd_clar        = 0.08223, 
              sd_fish        = 0.22340, 
              sd_bio         = 0.07842, 
              sd_coast       = 0.15984,
              sd_lit         = 0.09826,  
              sd_cost        = 0.12489,
              
              sd_asc1_clar   = rnorm(1), 
              sd_asc1_fish   = rnorm(1),  
              sd_asc1_bio    = rnorm(1),  
              sd_asc1_coast  = rnorm(1),
              sd_asc1_lit    = rnorm(1),
              sd_asc1_cost   = rnorm(1),
              
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
  #interNDraws    = 500,
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_asc1","draws_clar","draws_fish","draws_bio","draws_coast","draws_lit","draws_cost"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_asc1"]]    =       ( mu_asc1    + sd_asc1        * draws_asc1 )
  randcoeff[["b_clar"]]    =       ( mu_clar    + sd_asc1_clar   * draws_asc1  + sd_clar        * draws_clar )
  randcoeff[["b_fish"]]    =       ( mu_fish    + sd_asc1_fish   * draws_asc1  + sd_clar_fish   * draws_clar + sd_fish        * draws_fish )
  randcoeff[["b_bio"]]     =       ( mu_bio     + sd_asc1_bio    * draws_asc1  + sd_clar_bio    * draws_clar + sd_fish_bio    * draws_fish + sd_bio            * draws_bio)
  randcoeff[["b_coast"]]   =       ( mu_coast   + sd_asc1_coast  * draws_asc1  + sd_clar_coast  * draws_clar + sd_fish_coast  * draws_fish + sd_bio_coast      * draws_bio + sd_coast      * draws_coast)
  randcoeff[["b_lit"]]     =       ( mu_lit     + sd_asc1_lit    * draws_asc1  + sd_clar_lit    * draws_clar + sd_fish_lit    * draws_fish + sd_bio_lit        * draws_bio + sd_coast_lit  * draws_coast + sd_lit       * draws_lit)
  randcoeff[["b_cost"]]    = -exp( ( mu_cost    + sd_asc1_cost   * draws_asc1  + sd_clar_cost   * draws_clar + sd_fish_cost   * draws_fish + sd_bio_cost       * draws_bio + sd_coast_cost * draws_coast + sd_lit_cost  * draws_lit + sd_cost       * draws_cost) )  # if "b_cost" is log-normally distributed
  
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
  V[["alt2"]]  =           b_clar * clar2 + b_fish * fish2 + b_bio * bio2 + b_coast * coast2 + b_lit * lit2 + b_cost * cost2 
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2), 
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
                                                iterMax   = 500,
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
# Model name                                  : RPL_corr_CostLogNormal_treatment_2alt
# Model description                           : Correlated RPL model; The effect of number of alternatives
# Model run at                                : 2026-02-04 16:39:03.710984
# Estimation method                           : bgw
# Model diagnosis                             : Relative function convergence
# Optimisation diagnosis                      : Maximum found
# hessian properties                     : Negative definite
# maximum eigenvalue                     : -3.035931
# reciprocal of condition number         : 0.00519011
# Number of individuals                       : 305
# Number of rows in database                  : 2440
# Number of modelled outcomes                 : 2440
# 
# Number of cores used                        :  4 
# Number of inter-individual draws            : 1000 (halton)
# 
# LL(start)                                   : -1241.44
# LL at equal shares, LL(0)                   : -1691.28
# LL at observed shares, LL(C)                : -1680.04
# LL(final)                                   : -1132.77
# Rho-squared vs equal shares                  :  0.3302 
# Adj.Rho-squared vs equal shares              :  0.3095 
# Rho-squared vs observed shares               :  0.3257 
# Adj.Rho-squared vs observed shares           :  0.3055 
# AIC                                         :  2335.55 
# BIC                                         :  2538.54 
# 
# Estimated parameters                        : 35
# Time taken (hh:mm:ss)                       :  00:29:40.48 
# pre-estimation                         :  00:00:58.58 
# estimation                             :  00:05:57.96 
# post-estimation                        :  00:22:43.94 
# Iterations                                  :  42  
# 
# Unconstrained optimisation.
# 
# Estimates:
#   Estimate        s.e.   t.rat.(0)    Rob.s.e. Rob.t.rat.(0)
# mu_asc1          0.496908     0.20470    2.427521     0.22116      2.246830
# mu_clar          0.731339     0.09977    7.330403     0.10361      7.058803
# mu_fish          1.917042     0.28547    6.715346     0.26267      7.298212
# mu_bio           0.566663     0.09850    5.753144     0.09437      6.005002
# mu_coast         0.089702     0.20000    0.448509     0.19610      0.457421
# mu_lit           0.862617     0.12039    7.165128     0.11373      7.584933
# mu_cost         -4.078784     0.14947  -27.287966     0.13367    -30.513516
# sd_asc1          0.378657     0.37651    1.005710     0.51537      0.734726
# sd_clar          0.200838     0.15708    1.278591     0.15102      1.329916
# sd_fish          0.544650     0.51639    1.054715     0.36786      1.480602
# sd_bio          -0.187499     0.16747   -1.119608     0.14345     -1.307062
# sd_coast        -0.218607     0.46076   -0.474448     0.31749     -0.688539
# sd_lit           0.157570     0.23882    0.659795     0.19287      0.816991
# sd_cost         -0.080612     0.08752   -0.921033     0.02927     -2.754282
# sd_asc1_clar    -0.395388     0.13741   -2.877394     0.15074     -2.622955
# sd_asc1_fish    -1.063860     0.36893   -2.883646     0.32184     -3.305593
# sd_asc1_bio     -0.219660     0.11649   -1.885617     0.12214     -1.798469
# sd_asc1_coast   -0.273579     0.30988   -0.882853     0.26064     -1.049625
# sd_asc1_lit     -0.364113     0.14699   -2.477161     0.15541     -2.342865
# sd_asc1_cost    -1.068193     0.10807   -9.883834     0.08034    -13.295855
# sd_clar_fish    -0.585732     0.50468   -1.160603     0.51939     -1.127735
# sd_clar_bio     -0.050855     0.15870   -0.320456     0.15510     -0.327876
# sd_clar_coast   -0.989448     0.27304   -3.623760     0.25065     -3.947557
# sd_clar_lit      0.015681     0.20996    0.074687     0.17629      0.088952
# sd_clar_cost     1.000708     0.18480    5.415102     0.17357      5.765375
# sd_fish_bio      0.001069     0.21700    0.004926     0.16764      0.006376
# sd_fish_coast   -0.067844     0.50431   -0.134528     0.32143     -0.211070
# sd_fish_lit     -0.175618     0.30768   -0.570782     0.29335     -0.598657
# sd_fish_cost     0.266657     0.12400    2.150485     0.05867      4.544887
# sd_bio_coast    -0.101254     0.39919   -0.253647     0.25118     -0.403106
# sd_bio_lit       0.382575     0.18836    2.031094     0.16124      2.372731
# sd_bio_cost     -0.496174     0.10550   -4.703010     0.05282     -9.394094
# sd_coast_lit    -0.022803     0.22303   -0.102245     0.11303     -0.201754
# sd_coast_cost   -0.727730     0.12256   -5.937955     0.05750    -12.655340
# sd_lit_cost      0.752713     0.13007    5.787156     0.09590      7.849005


#######
## SAVE RESULTS
#######

apollo_saveOutput(model)


#######
##  STANDARD DEVIATIONS OF THE RANDOM PARAMETERS AND THEIR STANDARD ERRORS
#######

choleski.cov <- t(matrix (c(model$estimate["sd_asc1"]      , 0                              , 0                               , 0                             , 0                              ,0                              , 0                        ,
                            model$estimate["sd_asc1_clar"] , model$estimate["sd_clar"]      , 0                               , 0                             , 0                              ,0                              , 0                        ,
                            model$estimate["sd_asc1_fish"] , model$estimate["sd_clar_fish"] , model$estimate["sd_fish"]       , 0                             , 0                              ,0                              , 0                        ,
                            model$estimate["sd_asc1_bio"]  , model$estimate["sd_clar_bio"]  , model$estimate["sd_fish_bio"]   , model$estimate["sd_bio"]      , 0                              ,0                              , 0                        ,     
                            model$estimate["sd_asc1_coast"], model$estimate["sd_clar_coast"], model$estimate["sd_fish_coast"] , model$estimate["sd_bio_coast"], model$estimate["sd_coast"]     ,0                              , 0                        ,
                            model$estimate["sd_asc1_lit"]  , model$estimate["sd_clar_lit"]  , model$estimate["sd_fish_lit"]   , model$estimate["sd_bio_lit"]  , model$estimate["sd_coast_lit"] , model$estimate["sd_lit"]      , 0                        ,      
                            model$estimate["sd_asc1_cost"] , model$estimate["sd_clar_cost"] , model$estimate["sd_fish_cost"]  , model$estimate["sd_bio_cost"] , model$estimate["sd_coast_cost"], model$estimate["sd_lit_cost"] , model$estimate["sd_cost"]), 7))
colnames(choleski.cov) <- c("mu_asc1","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
rownames(choleski.cov) <- c("mu_asc1","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
choleski.cov
#            [,1]        [,2]         [,3]       [,4]        [,5]      [,6]        [,7]
# [1,]  0.3786570  0.00000000  0.000000000  0.0000000  0.00000000 0.0000000  0.00000000
# [2,] -0.3953876  0.20083763  0.000000000  0.0000000  0.00000000 0.0000000  0.00000000
# [3,] -1.0638598 -0.58573200  0.544649583  0.0000000  0.00000000 0.0000000  0.00000000
# [4,] -0.2196596 -0.05085491  0.001068928 -0.1874991  0.00000000 0.0000000  0.00000000
# [5,] -0.2735790 -0.98944844 -0.067843891 -0.1012535 -0.21860737 0.0000000  0.00000000
# [6,] -0.3641128  0.01568131 -0.175618096  0.3825746 -0.02280328 0.1575696  0.00000000
# [7,] -1.0681935  1.00070830  0.266656806 -0.4961744 -0.72772958 0.7527126 -0.08061224

var.cov           <- choleski.cov %*% t(choleski.cov)  # variance-covariance matriz of the random parameters
colnames(var.cov) <- c("mu_asc1","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
rownames(var.cov) <- c("mu_asc1","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
var.cov             # variance-covariance matriz of the random parameters
#             mu_asc1     mu_clar    mu_fish       mu_bio    mu_coast       mu_lit    mu_cost
# mu_asc1   0.14338111 -0.14971626 -0.4028379 -0.083175648 -0.10359261 -0.137873867 -0.4044789
# mu_clar  -0.14971626  0.19666708  0.3029999  0.076637101 -0.09054874  0.147115082  0.6233303
# mu_fish  -0.40283793  0.30299989  1.7715227  0.264056562  0.83365019  0.282529625  0.6954957
# mu_bio   -0.08317565  0.07663710  0.2640566  0.085993639  0.12932500  0.007263281  0.2770654
# mu_coast -0.10359261 -0.09054874  0.8336502  0.129324999  1.11649796  0.062260347 -0.5066785
# mu_lit   -0.13787387  0.14711508  0.2825296  0.007263281  0.06226035  0.335377274  0.3031811
# mu_cost  -0.40447892  0.62333030  0.6954957  0.277065361 -0.50667852  0.303181135  3.5624142
sd.rand.param     <- sqrt(diag(var.cov))               # standard deviations of of the random parameters
sd.rand.param
#  mu_asc1   mu_clar   mu_fish    mu_bio  mu_coast    mu_lit   mu_cost 
#  0.3786570 0.4434716 1.3309856 0.2932467 1.0566447 0.5791177 1.8874359

## Standard deviation of the random parameters and their standard errors ----
deltaMethod_settings <-
  list(
    expression = c(
      rob_se_sd_asc1  = "sqrt(sd_asc1^2 )",       
      rob_se_sd_clar  = "sqrt(sd_asc1_clar^2   + sd_clar^2)",        
      rob_se_sd_fish  = "sqrt(sd_asc1_fish^2   + sd_clar_fish^2   + sd_fish^2)",        
      rob_se_sd_bio   = "sqrt(sd_asc1_bio^2    + sd_clar_bio^2    + sd_fish_bio^2    + sd_bio^2)",            
      rob_se_sd_coast = "sqrt(sd_asc1_coast^2  + sd_clar_coast^2  + sd_fish_coast^2  + sd_bio_coast^2      + sd_coast^2)",      
      rob_se_sd_lit   = "sqrt(sd_asc1_lit^2    + sd_clar_lit^2    + sd_fish_lit^2    + sd_bio_lit^2        + sd_coast_lit^2  + sd_lit^2)",       
      rob_se_sd_cost  = "sqrt(sd_asc1_cost^2   + sd_clar_cost^2   + sd_fish_cost^2   + sd_bio_cost^2       + sd_coast_cost^2 + sd_lit_cost^2  + sd_cost^2)" 
    ),
    varcov="robust"
  )


est_sd       <- apollo_deltaMethod(model, deltaMethod_settings)
est_sd      # standard deviations of the random parameters and their standar errors 
#       Expression  Value   s.e. t-ratio (0)
# 1  rob_se_sd_asc1 0.3787 0.5154        0.73
# 2  rob_se_sd_clar 0.4435 0.1242        3.57
# 3  rob_se_sd_fish 1.3310 0.3595        3.70
# 4   rob_se_sd_bio 0.2932 0.1443        2.03
# 5 rob_se_sd_coast 1.0566 0.2620        4.03
# 6   rob_se_sd_lit 0.5791 0.1491        3.88
# 7  rob_se_sd_cost 1.8874 0.1626       11.61


