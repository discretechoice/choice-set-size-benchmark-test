####################################################################
####################################################################
#####
#####            RPL correlated, no socio-dem - 3 alternatives - treatment 2
#####
####################################################################
####################################################################


################################################
# 
# The script: RPL ESTIMATES WITH 3 ALTERNATIVES
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
  modelName       = "RPL_corr_CostLogNormal_treatment_3alt",
  modelDescr      = "Correlated RPL model; The effect of number of alternatives",
  indivID         = "id",  
  mixing          = TRUE,
  nCores          = 4,
  outputDirectory = "output.RPL.corr.CostLogNormal3alt"
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
              sd_clar        = 0.08223, 
              sd_fish        = 0.22340, 
              sd_bio         = 0.07842, 
              sd_coast       = 0.15984,
              sd_lit         = 0.09826,  
              sd_cost        = 0.12489,
              
              sd_asc1_asc2   = rnorm(1),
              sd_asc1_clar   = rnorm(1),
              sd_asc1_fish   = rnorm(1),  
              sd_asc1_bio    = rnorm(1),  
              sd_asc1_coast  = rnorm(1),
              sd_asc1_lit    = rnorm(1),
              sd_asc1_cost   = rnorm(1),
              
              sd_asc2_clar   = rnorm(1),
              sd_asc2_fish   = rnorm(1),  
              sd_asc2_bio    = rnorm(1),  
              sd_asc2_coast  = rnorm(1),
              sd_asc2_lit    = rnorm(1),
              sd_asc2_cost   = rnorm(1),
              
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
  interNormDraws = c("draws_asc1","draws_asc2","draws_clar","draws_fish","draws_bio","draws_coast","draws_lit","draws_cost"),
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
  randcoeff[["b_clar"]]    =       ( mu_clar    + sd_asc1_clar   * draws_asc1 + sd_asc2_clar   * draws_asc2 + sd_clar        * draws_clar )
  randcoeff[["b_fish"]]    =       ( mu_fish    + sd_asc1_fish   * draws_asc1 + sd_asc2_fish   * draws_asc2 + sd_clar_fish   * draws_clar + sd_fish        * draws_fish )
  randcoeff[["b_bio"]]     =       ( mu_bio     + sd_asc1_bio    * draws_asc1 + sd_asc2_bio    * draws_asc2 + sd_clar_bio    * draws_clar + sd_fish_bio    * draws_fish + sd_bio            * draws_bio)
  randcoeff[["b_coast"]]   =       ( mu_coast   + sd_asc1_coast  * draws_asc1 + sd_asc2_coast  * draws_asc2 + sd_clar_coast  * draws_clar + sd_fish_coast  * draws_fish + sd_bio_coast      * draws_bio + sd_coast      * draws_coast)
  randcoeff[["b_lit"]]     =       ( mu_lit     + sd_asc1_lit    * draws_asc1 + sd_asc2_lit    * draws_asc2 + sd_clar_lit    * draws_clar + sd_fish_lit    * draws_fish + sd_bio_lit        * draws_bio + sd_coast_lit  * draws_coast + sd_lit       * draws_lit)
  randcoeff[["b_cost"]]    = -exp( ( mu_cost    + sd_asc1_cost   * draws_asc1 + sd_asc2_cost   * draws_asc2 + sd_clar_cost   * draws_clar + sd_fish_cost   * draws_fish + sd_bio_cost       * draws_bio + sd_coast_cost * draws_coast + sd_lit_cost  * draws_lit + sd_cost       * draws_cost) )  # if "b_cost" is log-normally distributed
  

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
  V[["alt3"]]  =           b_clar * clar3 + b_fish * fish3 + b_bio * bio3 + b_coast * coast3 + b_lit * lit3 + b_cost * cost3 
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3), 
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
# Model name                                  : RPL_corr_CostLogNormal_treatment_3alt
# Model description                           : Correlated RPL model; The effect of number of alternatives
# Model run at                                : 2026-02-04 17:26:06.238329
# Estimation method                           : bgw
# Model diagnosis                             : Relative function convergence
# Optimisation diagnosis                      : Maximum found
# hessian properties                     : Negative definite
# maximum eigenvalue                     : -3.237189
# reciprocal of condition number         : 0.00255075
# Number of individuals                       : 301
# Number of rows in database                  : 2408
# Number of modelled outcomes                 : 2408
# 
# Number of cores used                        :  4 
# Number of inter-individual draws            : 1000 (halton)
# 
# LL(start)                                   : -2258.72
# LL at equal shares, LL(0)                   : -2645.46
# LL at observed shares, LL(C)                : -2573.85
# LL(final)                                   : -1787.7
# Rho-squared vs equal shares                  :  0.3242 
# Adj.Rho-squared vs equal shares              :  0.3076 
# Rho-squared vs observed shares               :  0.3054 
# Adj.Rho-squared vs observed shares           :  0.2891 
# AIC                                         :  3663.4 
# BIC                                         :  3918 
# 
# Estimated parameters                        : 44
# Time taken (hh:mm:ss)                       :  00:53:48.09 
# pre-estimation                         :  00:01:2.78 
# estimation                             :  00:08:54.72 
# post-estimation                        :  00:43:50.59 
# Iterations                                  :  48  
# 
# Unconstrained optimisation.
# 
# Estimates:
#   Estimate        s.e.   t.rat.(0)    Rob.s.e. Rob.t.rat.(0)
# mu_asc1          -1.16029     0.37105     -3.1271     0.34606       -3.3529
# mu_asc2           0.26791     0.08224      3.2576     0.08074        3.3182
# mu_clar           0.47020     0.06691      7.0273     0.07193        6.5367
# mu_fish           0.79074     0.13142      6.0167     0.13361        5.9185
# mu_bio            0.67645     0.07646      8.8469     0.07837        8.6320
# mu_coast          0.08804     0.11067      0.7955     0.11141        0.7903
# mu_lit            0.65300     0.07530      8.6720     0.07320        8.9202
# mu_cost          -5.10991     0.31601    -16.1699     0.54826       -9.3203
# sd_asc1           4.29046     0.44470      9.6480     0.54514        7.8704
# sd_asc2          -0.36019     0.12071     -2.9839     0.11873       -3.0336
# sd_clar           0.34541     0.09658      3.5763     0.13409        2.5760
# sd_fish           0.85204     0.21223      4.0147     0.36358        2.3435
# sd_bio           -0.18404     0.09001     -2.0446     0.09321       -1.9745
# sd_coast         -0.31004     0.31464     -0.9854     0.40767       -0.7605
# sd_lit           -0.01446     0.09640     -0.1500     0.06317       -0.2289
# sd_cost           0.32212     0.14925      2.1583     0.20113        1.6015
# sd_asc1_asc2      0.18983     0.11834      1.6041     0.11379        1.6682
# sd_asc1_clar      0.35148     0.09195      3.8228     0.13471        2.6092
# sd_asc1_fish      0.37102     0.22264      1.6665     0.34235        1.0837
# sd_asc1_bio       0.43828     0.10983      3.9906     0.18294        2.3957
# sd_asc1_coast     0.52207     0.16830      3.1020     0.25484        2.0486
# sd_asc1_lit       0.38571     0.11823      3.2624     0.20810        1.8535
# sd_asc1_cost      0.84895     0.29256      2.9018     0.61318        1.3845
# sd_asc2_clar      0.22728     0.09080      2.5031     0.10136        2.2423
# sd_asc2_fish      0.24579     0.23230      1.0581     0.25630        0.9590
# sd_asc2_bio      -0.11611     0.10572     -1.0983     0.11947       -0.9718
# sd_asc2_coast     0.05552     0.19454      0.2854     0.24800        0.2239
# sd_asc2_lit      -0.23163     0.10660     -2.1729     0.13758       -1.6836
# sd_asc2_cost      0.91968     0.23811      3.8625     0.37063        2.4814
# sd_clar_fish      0.82131     0.24342      3.3740     0.34626        2.3719
# sd_clar_bio       0.47433     0.10386      4.5672     0.11924        3.9781
# sd_clar_coast     0.42486     0.17613      2.4122     0.21173        2.0066
# sd_clar_lit       0.48124     0.09472      5.0805     0.09270        5.1911
# sd_clar_cost      0.32540     0.17065      1.9069     0.26304        1.2371
# sd_fish_bio       0.27159     0.14133      1.9216     0.25180        1.0786
# sd_fish_coast     0.63245     0.18799      3.3643     0.24407        2.5913
# sd_fish_lit       0.15634     0.10539      1.4834     0.13607        1.1489
# sd_fish_cost      0.55838     0.15570      3.5861     0.22176        2.5179
# sd_bio_coast      0.32903     0.16966      1.9394     0.15497        2.1232
# sd_bio_lit        0.06101     0.09674      0.6306     0.08123        0.7511
# sd_bio_cost      -1.16134     0.14983     -7.7511     0.23577       -4.9258
# sd_coast_lit     -0.16137     0.10351     -1.5590     0.12825       -1.2582
# sd_coast_cost     0.06907     0.09441      0.7316     0.10126        0.6821
# sd_lit_cost       0.93875     0.16925      5.5466     0.27594        3.4020




#######
## SAVE RESULTS
#######

apollo_saveOutput(model)


#######
##  STANDARD DEVIATIONS OF THE RANDOM PARAMETERS AND THEIR STANDARD ERRORS
#######

choleski.cov <- t(matrix (c(model$estimate["sd_asc1"]      , 0                              , 0                             , 0                               , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_asc2"] , model$estimate["sd_asc2"]      , 0                             , 0                               , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_clar"] , model$estimate["sd_asc2_clar"] ,model$estimate["sd_clar"]      , 0                               , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_fish"] , model$estimate["sd_asc2_fish"] ,model$estimate["sd_clar_fish"] , model$estimate["sd_fish"]       , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_bio"]  , model$estimate["sd_asc2_bio"]  ,model$estimate["sd_clar_bio"]  , model$estimate["sd_fish_bio"]   , model$estimate["sd_bio"]      , 0                              , 0                             , 0                        ,     
                            model$estimate["sd_asc1_coast"], model$estimate["sd_asc2_coast"],model$estimate["sd_clar_coast"], model$estimate["sd_fish_coast"] , model$estimate["sd_bio_coast"], model$estimate["sd_coast"]     , 0                             , 0                        ,
                            model$estimate["sd_asc1_lit"]  , model$estimate["sd_asc2_lit"]  ,model$estimate["sd_clar_lit"]  , model$estimate["sd_fish_lit"]   , model$estimate["sd_bio_lit"]  , model$estimate["sd_coast_lit"] , model$estimate["sd_lit"]      , 0                        ,      
                            model$estimate["sd_asc1_cost"] , model$estimate["sd_asc2_cost"] ,model$estimate["sd_clar_cost"] , model$estimate["sd_fish_cost"]  , model$estimate["sd_bio_cost"] , model$estimate["sd_coast_cost"], model$estimate["sd_lit_cost"] , model$estimate["sd_cost"]), 8))

colnames(choleski.cov) <- c("mu_asc1","mu_asc2","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
rownames(choleski.cov) <- c("mu_asc1","mu_asc2","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
choleski.cov
#           mu_asc1     mu_asc2   mu_clar   mu_fish      mu_bio    mu_coast      mu_lit   mu_cost
# mu_asc1  4.2904643  0.00000000 0.0000000 0.0000000  0.00000000  0.00000000  0.00000000 0.0000000
# mu_asc2  0.1898287 -0.36019131 0.0000000 0.0000000  0.00000000  0.00000000  0.00000000 0.0000000
# mu_clar  0.3514848  0.22727742 0.3454141 0.0000000  0.00000000  0.00000000  0.00000000 0.0000000
# mu_fish  0.3710239  0.24579371 0.8213051 0.8520398  0.00000000  0.00000000  0.00000000 0.0000000
# mu_bio   0.4382805 -0.11610798 0.4743342 0.2715873 -0.18403834  0.00000000  0.00000000 0.0000000
# mu_coast 0.5220658  0.05551644 0.4248615 0.6324541  0.32903289 -0.31004291  0.00000000 0.0000000
# mu_lit   0.3857104 -0.23163020 0.4812363 0.1563407  0.06101053 -0.16137030 -0.01446199 0.0000000
# mu_cost  0.8489542  0.91967573 0.3253998 0.5583763 -1.16134130  0.06907059  0.93875282 0.3221178

var.cov           <- choleski.cov %*% t(choleski.cov)  # variance-covariance matriz of the random parameters
colnames(var.cov) <- c("mu_asc1","mu_asc2","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
rownames(var.cov) <- c("mu_asc1","mu_asc2","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
var.cov             # variance-covariance matriz of the random parameters
#             mu_asc1     mu_asc2     mu_clar     mu_fish    mu_bio   mu_coast    mu_lit    mu_cost
# mu_asc1  18.4080838  0.81445325  1.50803277  1.59186490 1.8804269 2.23990457 1.6548766  3.6424078
# mu_asc2   0.8144533  0.16577271 -0.01514146 -0.01810177 0.1250193 0.07910653 0.1566501 -0.1701033
# mu_clar   1.5080328 -0.01514146  0.29450748  0.46996301 0.2915019 0.34286898 0.2491528  0.6198137
# mu_fish   1.5918649 -0.01810177  0.46996301  1.59858715 0.7550502 1.09516148 0.6146249  1.2840442
# mu_bio    1.8804269  0.12501931  0.29150194  0.75505022 0.5381936 0.53510353 0.4554422  0.7850259
# mu_coast  2.2399046  0.07910653  0.34286898  1.09516148 0.5351035 1.06052950 0.5619502  0.5821300
# mu_lit    1.6548766  0.15665008  0.24915282  0.61462486 0.4554422 0.56195020 0.4884276  0.2627407
# mu_cost   3.6424078 -0.17010333  0.61981370  1.28404418 0.7850259 0.58212998 0.2627407  4.3226970
sd.rand.param     <- sqrt(diag(var.cov))               # standard deviations of of the random parameters
sd.rand.param
# mu_asc1   mu_asc2   mu_clar   mu_fish    mu_bio  mu_coast    mu_lit   mu_cost 
# 4.2904643 0.4071520 0.5426854 1.2643525 0.7336168 1.0298201 0.6988760 2.0791097

## Standard deviation of the random parameters and their standard errors ----
deltaMethod_settings <-
  list(
    expression = c(
      rob_se_sd_asc1  = "sqrt(sd_asc1^2 )",       
      rob_se_sd_asc2  = "sqrt(sd_asc1_asc2^2   + sd_asc2^2)",        
      rob_se_sd_clar  = "sqrt(sd_asc1_clar^2   + sd_asc2_clar^2   + sd_clar^2)",        
      rob_se_sd_fish  = "sqrt(sd_asc1_fish^2   + sd_asc2_fish^2   + sd_clar_fish^2   + sd_fish^2)",        
      rob_se_sd_bio   = "sqrt(sd_asc1_bio^2    + sd_asc2_bio^2    + sd_clar_bio^2    + sd_fish_bio^2    + sd_bio^2)",            
      rob_se_sd_coast = "sqrt(sd_asc1_coast^2  + sd_asc2_coast^2  + sd_clar_coast^2  + sd_fish_coast^2  + sd_bio_coast^2      + sd_coast^2)",      
      rob_se_sd_lit   = "sqrt(sd_asc1_lit^2    + sd_asc2_lit^2    + sd_clar_lit^2    + sd_fish_lit^2    + sd_bio_lit^2        + sd_coast_lit^2  + sd_lit^2)",       
      rob_se_sd_cost  = "sqrt(sd_asc1_cost^2   + sd_asc2_cost^2   + sd_clar_cost^2   + sd_fish_cost^2   + sd_bio_cost^2       + sd_coast_cost^2 + sd_lit_cost^2  + sd_cost^2)" 
    ),
    varcov="robust"
  )

est_sd       <- apollo_deltaMethod(model, deltaMethod_settings)
est_sd      # standard deviations of the random parameters and their standar errors 
#        Expression  Value   s.e. t-ratio (0)
# 1  rob_se_sd_asc1 4.2905 0.5451        7.87
# 2  rob_se_sd_asc2 0.4072 0.1249        3.26
# 3  rob_se_sd_clar 0.5427 0.0749        7.25
# 4  rob_se_sd_fish 1.2644 0.1706        7.41
# 5   rob_se_sd_bio 0.7336 0.0900        8.15
# 6 rob_se_sd_coast 1.0298 0.1834        5.61
# 7   rob_se_sd_lit 0.6989 0.0980        7.13
# 8  rob_se_sd_cost 2.0791 0.6006        3.46

