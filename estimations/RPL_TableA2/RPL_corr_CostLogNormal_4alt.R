####################################################################
####################################################################
#####
#####            RPL correlated, no socio-dem - 4 alternatives - treatment 3
#####
####################################################################
####################################################################


################################################
# 
# The script: RPL ESTIMATES WITH 4 ALTERNATIVES
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
choose.treatment <- 3   ## the treatment that we choose to test
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
  modelName       = "RPL_corr_CostLogNormal_treatment_4alt",
  modelDescr      = "Correlated RPL model; The effect of number of alternatives",
  indivID         = "id",  
  mixing          = TRUE,
  nCores          = 4,
  outputDirectory = "output.RPL.corr.CostLogNormal4alt"
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
               sd_clar        = 0.08223, 
               sd_fish        = 0.22340, 
               sd_bio         = 0.07842, 
               sd_coast       = 0.15984,
               sd_lit         = 0.09826,  
               sd_cost        = 0.12489,
               
               sd_asc1_asc2   = rnorm(1),
               sd_asc1_asc3   = rnorm(1),
               sd_asc1_clar   = rnorm(1),
               sd_asc1_fish   = rnorm(1),  
               sd_asc1_bio    = rnorm(1),  
               sd_asc1_coast  = rnorm(1),
               sd_asc1_lit    = rnorm(1),
               sd_asc1_cost   = rnorm(1),
               
               sd_asc2_asc3   = rnorm(1),
               sd_asc2_clar   = rnorm(1),
               sd_asc2_fish   = rnorm(1),  
               sd_asc2_bio    = rnorm(1),  
               sd_asc2_coast  = rnorm(1),
               sd_asc2_lit    = rnorm(1),
               sd_asc2_cost   = rnorm(1),
               
               sd_asc3_clar   = rnorm(1),
               sd_asc3_fish   = rnorm(1),  
               sd_asc3_bio    = rnorm(1),  
               sd_asc3_coast  = rnorm(1),
               sd_asc3_lit    = rnorm(1),
               sd_asc3_cost   = rnorm(1),
               
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
  interNormDraws = c("draws_asc1","draws_asc2","draws_asc3","draws_clar","draws_fish","draws_bio","draws_coast","draws_lit","draws_cost"),
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
  randcoeff[["b_clar"]]    =       ( mu_clar    + sd_asc1_clar   * draws_asc1 + sd_asc2_clar   * draws_asc2 + sd_asc3_clar   * draws_asc3 + sd_clar        * draws_clar )
  randcoeff[["b_fish"]]    =       ( mu_fish    + sd_asc1_fish   * draws_asc1 + sd_asc2_fish   * draws_asc2 + sd_asc3_fish   * draws_asc3 + sd_clar_fish   * draws_clar + sd_fish        * draws_fish )
  randcoeff[["b_bio"]]     =       ( mu_bio     + sd_asc1_bio    * draws_asc1 + sd_asc2_bio    * draws_asc2 + sd_asc3_bio    * draws_asc3 + sd_clar_bio    * draws_clar + sd_fish_bio    * draws_fish + sd_bio            * draws_bio)
  randcoeff[["b_coast"]]   =       ( mu_coast   + sd_asc1_coast  * draws_asc1 + sd_asc2_coast  * draws_asc2 + sd_asc3_coast  * draws_asc3 + sd_clar_coast  * draws_clar + sd_fish_coast  * draws_fish + sd_bio_coast      * draws_bio + sd_coast      * draws_coast)
  randcoeff[["b_lit"]]     =       ( mu_lit     + sd_asc1_lit    * draws_asc1 + sd_asc2_lit    * draws_asc2 + sd_asc3_lit    * draws_asc3 + sd_clar_lit    * draws_clar + sd_fish_lit    * draws_fish + sd_bio_lit        * draws_bio + sd_coast_lit  * draws_coast + sd_lit       * draws_lit)
  randcoeff[["b_cost"]]    = -exp( ( mu_cost    + sd_asc1_cost   * draws_asc1 + sd_asc2_cost   * draws_asc2 + sd_asc3_cost   * draws_asc3 + sd_clar_cost   * draws_clar + sd_fish_cost   * draws_fish + sd_bio_cost       * draws_bio + sd_coast_cost * draws_coast + sd_lit_cost  * draws_lit + sd_cost       * draws_cost) )  # if "b_cost" is log-normally distributed
  
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
  V[["alt4"]]  =           b_clar * clar4 + b_fish * fish4 + b_bio * bio4 + b_coast * coast4 + b_lit * lit4 + b_cost * cost4 
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3, alt4=4), 
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
# Model name                                  : RPL_corr_CostLogNormal_treatment_4alt
# Model description                           : Correlated RPL model; The effect of number of alternatives
# Model run at                                : 2026-02-05 22:00:57.812062
# Estimation method                           : bgw
# Model diagnosis                             : Relative function convergence
# Optimisation diagnosis                      : Maximum found
# hessian properties                     : Negative definite
# maximum eigenvalue                     : -0.688324
# reciprocal of condition number         : 0.000654847
# Number of individuals                       : 305
# Number of rows in database                  : 2440
# Number of modelled outcomes                 : 2440
# 
# Number of cores used                        :  4 
# Number of inter-individual draws            : 1000 (halton)
# 
# LL(start)                                   : -2781.04
# LL at equal shares, LL(0)                   : -3382.56
# LL at observed shares, LL(C)                : -3277.88
# LL(final)                                   : -2258.01
# Rho-squared vs equal shares                  :  0.3325 
# Adj.Rho-squared vs equal shares              :  0.3165 
# Rho-squared vs observed shares               :  0.3111 
# Adj.Rho-squared vs observed shares           :  0.2956 
# AIC                                         :  4624.03 
# BIC                                         :  4937.21 
# 
# Estimated parameters                        : 54
# Time taken (hh:mm:ss)                       :  01:31:48.25 
# pre-estimation                         :  00:01:11.61 
# estimation                             :  00:21:28.85 
# post-estimation                        :  01:09:7.79 
# Iterations                                  :  91  
# 
# Unconstrained optimisation.
# 
# Estimates:
#   Estimate        s.e.   t.rat.(0)    Rob.s.e. Rob.t.rat.(0)
# mu_asc1          -1.85743     0.73190    -2.53784     2.71476      -0.68420
# mu_asc2           0.23968     0.09066     2.64360     0.11256       2.12943
# mu_asc3           0.42282     0.10108     4.18287     0.11281       3.74818
# mu_clar           0.40128     0.05171     7.76065     0.11038       3.63554
# mu_fish           0.73812     0.10179     7.25116     0.19164       3.85152
# mu_bio            0.69037     0.06827    10.11249     0.14541       4.74790
# mu_coast          0.06226     0.09401     0.66227     0.11814       0.52702
# mu_lit            0.73724     0.06572    11.21866     0.07838       9.40577
# mu_cost          -6.26402     0.30224   -20.72529     0.68990      -9.07958
# sd_asc1           2.97966     0.79536     3.74630     2.87538       1.03627
# sd_asc2          -0.09889     0.18691    -0.52909     0.28418      -0.34799
# sd_asc3           0.10571     0.23299     0.45369     0.70196       0.15059
# sd_clar           0.27987     0.07927     3.53056     0.18235       1.53479
# sd_fish           0.43609     0.21441     2.03397     0.60096       0.72566
# sd_bio           -0.47548     0.07544    -6.30250     0.16593      -2.86546
# sd_coast         -0.42736     0.30393    -1.40611     1.03482      -0.41298
# sd_lit            0.32326     0.08332     3.87993     0.14156       2.28363
# sd_cost           1.67726     0.14554    11.52410     0.19229       8.72275
# sd_asc1_asc2      0.01046     0.22752     0.04595     0.53867       0.01941
# sd_asc1_asc3     -0.18782     0.19667    -0.95500     0.26801      -0.70079
# sd_asc1_clar      0.03012     0.08341     0.36108     0.22136       0.13605
# sd_asc1_fish     -0.11365     0.16337    -0.69562     0.26652      -0.42641
# sd_asc1_bio      -0.05418     0.17105    -0.31674     0.64207      -0.08438
# sd_asc1_coast    -0.02104     0.19991    -0.10524     0.61969      -0.03395
# sd_asc1_lit       0.23331     0.17643     1.32240     0.69300       0.33667
# sd_asc1_cost      0.55024     0.14468     3.80327     0.38434       1.43165
# sd_asc2_asc3      0.69916     0.15610     4.47881     0.28432       2.45908
# sd_asc2_clar      0.02071     0.08059     0.25699     0.23320       0.08881
# sd_asc2_fish     -0.28270     0.17495    -1.61589     0.49201      -0.57459
# sd_asc2_bio      -0.04294     0.08300    -0.51733     0.13932      -0.30819
# sd_asc2_coast     0.08353     0.18446     0.45283     0.55469       0.15059
# sd_asc2_lit       0.06396     0.08344     0.76652     0.19168       0.33370
# sd_asc2_cost      0.27357     0.10205     2.68081     0.19810       1.38098
# sd_asc3_clar      0.22140     0.06626     3.34126     0.11214       1.97434
# sd_asc3_fish     -0.28932     0.20688    -1.39849     0.62088      -0.46599
# sd_asc3_bio      -0.06325     0.08163    -0.77488     0.12366      -0.51150
# sd_asc3_coast    -0.32810     0.14278    -2.29788     0.16368      -2.00453
# sd_asc3_lit      -0.03179     0.08857    -0.35895     0.16768      -0.18960
# sd_asc3_cost      2.04165     0.15657    13.03990     0.12659      16.12794
# sd_clar_fish      0.44113     0.15672     2.81471     0.20657       2.13551
# sd_clar_bio       0.28507     0.12532     2.27481     0.24891       1.14527
# sd_clar_coast     0.34184     0.14447     2.36621     0.14793       2.31090
# sd_clar_lit       0.44470     0.08556     5.19735     0.11634       3.82255
# sd_clar_cost      0.59018     0.13325     4.42898     0.37150       1.58863
# sd_fish_bio       0.23145     0.15728     1.47160     0.59037       0.39204
# sd_fish_coast    -0.23289     0.34729    -0.67057     1.27949      -0.18202
# sd_fish_lit       0.14777     0.17760     0.83205     0.68976       0.21424
# sd_fish_cost     -0.87733     0.11292    -7.76970     0.09946      -8.82122
# sd_bio_coast      0.11486     0.17027     0.67455     0.26002       0.44173
# sd_bio_lit       -0.16374     0.12608    -1.29864     0.40267      -0.40663
# sd_bio_cost      -1.81260     0.17075   -10.61560     0.40666      -4.45725
# sd_coast_lit      0.12530     0.10974     1.14175     0.32573       0.38467
# sd_coast_cost    -1.00581     0.09488   -10.60107     0.11162      -9.01125
# sd_lit_cost       0.04662     0.12750     0.36563     0.31449       0.14824


#######
## SAVE RESULTS
#######

apollo_saveOutput(model)


#######
##  STANDARD DEVIATIONS OF THE RANDOM PARAMETERS AND THEIR STANDARD ERRORS
#######

choleski.cov <- t(matrix (c(model$estimate["sd_asc1"]      , 0                              , 0                             , 0                              , 0                               , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_asc2"] , model$estimate["sd_asc2"]      , 0                             , 0                              , 0                               , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_asc3"] , model$estimate["sd_asc2_asc3"] ,model$estimate["sd_asc3"]      , 0                              , 0                               , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_clar"] , model$estimate["sd_asc2_clar"] ,model$estimate["sd_asc3_clar"] , model$estimate["sd_clar"]      , 0                               , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_fish"] , model$estimate["sd_asc2_fish"] ,model$estimate["sd_asc3_fish"] , model$estimate["sd_clar_fish"] , model$estimate["sd_fish"]       , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_bio"]  , model$estimate["sd_asc2_bio"]  ,model$estimate["sd_asc3_bio"]  , model$estimate["sd_clar_bio"]  , model$estimate["sd_fish_bio"]   , model$estimate["sd_bio"]      , 0                              , 0                             , 0                        ,     
                            model$estimate["sd_asc1_coast"], model$estimate["sd_asc2_coast"],model$estimate["sd_asc3_coast"], model$estimate["sd_clar_coast"], model$estimate["sd_fish_coast"] , model$estimate["sd_bio_coast"], model$estimate["sd_coast"]     , 0                             , 0                        ,
                            model$estimate["sd_asc1_lit"]  , model$estimate["sd_asc2_lit"]  ,model$estimate["sd_asc3_lit"]  , model$estimate["sd_clar_lit"]  , model$estimate["sd_fish_lit"]   , model$estimate["sd_bio_lit"]  , model$estimate["sd_coast_lit"] , model$estimate["sd_lit"]      , 0                        ,      
                            model$estimate["sd_asc1_cost"] , model$estimate["sd_asc2_cost"] ,model$estimate["sd_asc3_cost"] , model$estimate["sd_clar_cost"] , model$estimate["sd_fish_cost"]  , model$estimate["sd_bio_cost"] , model$estimate["sd_coast_cost"], model$estimate["sd_lit_cost"] , model$estimate["sd_cost"]), 9))

colnames(choleski.cov) <- c("mu_asc1","mu_asc2","mu_asc3","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
rownames(choleski.cov) <- c("mu_asc1","mu_asc2","mu_asc3","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
choleski.cov
#              mu_asc1     mu_asc2     mu_asc3   mu_clar    mu_fish     mu_bio   mu_coast     mu_lit  mu_cost
# mu_asc1   2.97966226  0.00000000  0.00000000 0.0000000  0.0000000  0.0000000  0.0000000 0.00000000 0.000000
# mu_asc2   0.01045530 -0.09889180  0.00000000 0.0000000  0.0000000  0.0000000  0.0000000 0.00000000 0.000000
# mu_asc3  -0.18781643  0.69915501  0.10570603 0.0000000  0.0000000  0.0000000  0.0000000 0.00000000 0.000000
# mu_clar   0.03011649  0.02071197  0.22140319 0.2798672  0.0000000  0.0000000  0.0000000 0.00000000 0.000000
# mu_fish  -0.11364766 -0.28270392 -0.28932301 0.4411296  0.4360945  0.0000000  0.0000000 0.00000000 0.000000
# mu_bio   -0.05417856 -0.04293701 -0.06324982 0.2850691  0.2314493 -0.4754752  0.0000000 0.00000000 0.000000
# mu_coast -0.02103891  0.08353058 -0.32809990 0.3418441 -0.2328860  0.1148576 -0.4273589 0.00000000 0.000000
# mu_lit    0.23330960  0.06396127 -0.03179174 0.4447004  0.1477721 -0.1637362  0.1252992 0.32326467 0.000000
# mu_cost   0.55024155  0.27357316  2.04165388 0.5901792 -0.8773255 -1.8126017 -1.0058058 0.04661891 1.677258

var.cov           <- choleski.cov %*% t(choleski.cov)  # variance-covariance matriz of the random parameters
colnames(var.cov) <- c("mu_asc1","mu_asc2","mu_asc3","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
rownames(var.cov) <- c("mu_asc1","mu_asc2","mu_asc3","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
var.cov             # variance-covariance matriz of the random parameters
#              mu_asc1      mu_asc2      mu_asc3      mu_clar     mu_fish       mu_bio     mu_coast       mu_lit     mu_cost
# mu_asc1   8.87838721  0.031153264 -0.559629543  0.089736973 -0.33863163 -0.161433815 -0.062688839  0.695183800  1.63953398
# mu_asc2   0.03115326  0.009888901 -0.071104373 -0.001733367  0.02676888  0.003679665 -0.008480458 -0.003885923 -0.02130120
# mu_asc3  -0.55962954 -0.071104373  0.535266511  0.032228162 -0.20689215 -0.026529886  0.027670139 -0.002461114  0.30374077
# mu_clar   0.08973697 -0.001733367  0.032228162  0.128681040  0.05012267  0.063256805  0.024125059  0.125769504  0.63943809
# mu_fish  -0.33863163  0.026768879 -0.206892155  0.050122668  0.56131889  0.263281541  0.122940699  0.225213979 -0.85282270
# mu_bio   -0.16143382  0.003679665 -0.026529886  0.063256805  0.26328154  0.369689296  0.007241488  0.225448706  0.65634073
# mu_coast -0.06268884 -0.008480458  0.027670139  0.024125059  0.12294070  0.007241488  0.481990704  0.056115088 -0.03087619
# mu_lit    0.69518380 -0.003885923 -0.002461114  0.125769504  0.22521398  0.225448706  0.056115088  0.426139583  0.39960784
# mu_cost   1.63953398 -0.021301201  0.303740768  0.639438092 -0.85282270  0.656340735 -0.030876187  0.399607838 12.77650719
sd.rand.param     <- sqrt(diag(var.cov))               # standard deviations of of the random parameters
sd.rand.param
# mu_asc1   mu_asc2   mu_asc3   mu_clar   mu_fish    mu_bio  mu_coast    mu_lit   mu_cost 
# 2.97966226 0.09944295 0.73161910 0.35872140 0.74921218 0.60802080 0.69425550 0.65279368 3.57442404

## Standard deviation of the random parameters and their standard errors ----
deltaMethod_settings <-
  list(
    expression = c(
      rob_se_sd_asc1  = "sqrt(sd_asc1^2)",        
      rob_se_sd_asc2  = "sqrt(sd_asc1_asc2^2   + sd_asc2^2)",        
      rob_se_sd_asc3  = "sqrt(sd_asc1_asc3^2   + sd_asc2_asc3^2   + sd_asc3^2)",        
      rob_se_sd_clar  = "sqrt(sd_asc1_clar^2   + sd_asc2_clar^2   + sd_asc3_clar^2   + sd_clar^2)",        
      rob_se_sd_fish  = "sqrt(sd_asc1_fish^2   + sd_asc2_fish^2   + sd_asc3_fish^2   + sd_clar_fish^2   + sd_fish^2)",        
      rob_se_sd_bio   = "sqrt(sd_asc1_bio^2    + sd_asc2_bio^2    + sd_asc3_bio^2    + sd_clar_bio^2    + sd_fish_bio^2    + sd_bio^2)",            
      rob_se_sd_coast = "sqrt(sd_asc1_coast^2  + sd_asc2_coast^2  + sd_asc3_coast^2  + sd_clar_coast^2  + sd_fish_coast^2  + sd_bio_coast^2      + sd_coast^2)",      
      rob_se_sd_lit   = "sqrt(sd_asc1_lit^2    + sd_asc2_lit^2    + sd_asc3_lit^2    + sd_clar_lit^2    + sd_fish_lit^2    + sd_bio_lit^2        + sd_coast_lit^2  + sd_lit^2)",       
      rob_se_sd_cost  = "sqrt(sd_asc1_cost^2   + sd_asc2_cost^2   + sd_asc3_cost^2   + sd_clar_cost^2   + sd_fish_cost^2   + sd_bio_cost^2       + sd_coast_cost^2 + sd_lit_cost^2  + sd_cost^2)" 
    ),
    varcov="robust"
  )


est_sd <- apollo_deltaMethod(model, deltaMethod_settings) 
est_sd      # standard deviations of the random parameters and their standar errors 
#        Expression  Value   s.e. t-ratio (0)
# 1  rob_se_sd_asc1 2.9797 2.8754        1.04
# 2  rob_se_sd_asc2 0.0994 0.2750        0.36
# 3  rob_se_sd_asc3 0.7316 0.2063        3.55
# 4  rob_se_sd_clar 0.3587 0.1255        2.86
# 5  rob_se_sd_fish 0.7492 0.3218        2.33
# 6   rob_se_sd_bio 0.6080 0.1835        3.31
# 7 rob_se_sd_coast 0.6943 0.2721        2.55
# 8   rob_se_sd_lit 0.6528 0.2314        2.82
# 9  rob_se_sd_cost 3.5744 0.3901        9.16

