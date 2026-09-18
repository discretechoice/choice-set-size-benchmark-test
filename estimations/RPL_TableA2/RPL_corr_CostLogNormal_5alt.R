####################################################################
####################################################################
#####
#####            RPL correlated, no socio-dem - 5 alternatives - treatment 4
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
  modelName       = "RPL_corr_CostLogNormal_treatment_5alt",
  modelDescr      = "Correlated RPL model; The effect of number of alternatives",
  indivID         = "id",  
  mixing          = TRUE,
  nCores          = 4,
  outputDirectory = "output.RPL.corr.CostLogNormal5alt"
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
              sd_clar        = 0.08223, 
              sd_fish        = 0.22340, 
              sd_bio         = 0.07842, 
              sd_coast       = 0.15984,
              sd_lit         = 0.09826,  
              sd_cost        = 0.12489,
              
              sd_asc1_asc2   = rnorm(1),
              sd_asc1_asc3   = rnorm(1),
              sd_asc1_asc4   = rnorm(1),
              sd_asc1_clar   = rnorm(1),
              sd_asc1_fish   = rnorm(1),  
              sd_asc1_bio    = rnorm(1),  
              sd_asc1_coast  = rnorm(1),
              sd_asc1_lit    = rnorm(1),
              sd_asc1_cost   = rnorm(1),
              
              sd_asc2_asc3   = rnorm(1),
              sd_asc2_asc4   = rnorm(1),
              sd_asc2_clar   = rnorm(1),
              sd_asc2_fish   = rnorm(1),  
              sd_asc2_bio    = rnorm(1),  
              sd_asc2_coast  = rnorm(1),
              sd_asc2_lit    = rnorm(1),
              sd_asc2_cost   = rnorm(1),
              
              sd_asc3_asc4   = rnorm(1),
              sd_asc3_clar   = rnorm(1),
              sd_asc3_fish   = rnorm(1),  
              sd_asc3_bio    = rnorm(1),  
              sd_asc3_coast  = rnorm(1),
              sd_asc3_lit    = rnorm(1),
              sd_asc3_cost   = rnorm(1),
              
              sd_asc4_clar   = rnorm(1),
              sd_asc4_fish   = rnorm(1),  
              sd_asc4_bio    = rnorm(1),  
              sd_asc4_coast  = rnorm(1),
              sd_asc4_lit    = rnorm(1),
              sd_asc4_cost   = rnorm(1),
              
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
  interNormDraws = c("draws_asc1","draws_asc2","draws_asc3","draws_asc4","draws_clar","draws_fish","draws_bio","draws_coast","draws_lit","draws_cost"),
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
  randcoeff[["b_clar"]]    =       ( mu_clar    + sd_asc1_clar   * draws_asc1 + sd_asc2_clar   * draws_asc2 + sd_asc3_clar   * draws_asc3 + sd_asc4_clar   * draws_asc4 + sd_clar        * draws_clar )
  randcoeff[["b_fish"]]    =       ( mu_fish    + sd_asc1_fish   * draws_asc1 + sd_asc2_fish   * draws_asc2 + sd_asc3_fish   * draws_asc3 + sd_asc4_fish   * draws_asc4 + sd_clar_fish   * draws_clar + sd_fish        * draws_fish )
  randcoeff[["b_bio"]]     =       ( mu_bio     + sd_asc1_bio    * draws_asc1 + sd_asc2_bio    * draws_asc2 + sd_asc3_bio    * draws_asc3 + sd_asc4_bio    * draws_asc4 + sd_clar_bio    * draws_clar + sd_fish_bio    * draws_fish + sd_bio            * draws_bio)
  randcoeff[["b_coast"]]   =       ( mu_coast   + sd_asc1_coast  * draws_asc1 + sd_asc2_coast  * draws_asc2 + sd_asc3_coast  * draws_asc3 + sd_asc4_coast  * draws_asc4 + sd_clar_coast  * draws_clar + sd_fish_coast  * draws_fish + sd_bio_coast      * draws_bio + sd_coast      * draws_coast)
  randcoeff[["b_lit"]]     =       ( mu_lit     + sd_asc1_lit    * draws_asc1 + sd_asc2_lit    * draws_asc2 + sd_asc3_lit    * draws_asc3 + sd_asc4_lit    * draws_asc4 + sd_clar_lit    * draws_clar + sd_fish_lit    * draws_fish + sd_bio_lit        * draws_bio + sd_coast_lit  * draws_coast + sd_lit       * draws_lit)
  randcoeff[["b_cost"]]    = -exp( ( mu_cost    + sd_asc1_cost   * draws_asc1 + sd_asc2_cost   * draws_asc2 + sd_asc3_cost   * draws_asc3 + sd_asc4_cost   * draws_asc4 + sd_clar_cost   * draws_clar + sd_fish_cost   * draws_fish + sd_bio_cost       * draws_bio + sd_coast_cost * draws_coast + sd_lit_cost  * draws_lit + sd_cost       * draws_cost) )  # if "b_cost" is log-normally distributed
  
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
  V[["alt5"]]  =           b_clar * clar5 + b_fish * fish5 + b_bio * bio5 + b_coast * coast5 + b_lit * lit5 + b_cost * cost5 
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3, alt4=4, alt5=5), 
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
# Model name                                  : RPL_corr_CostLogNormal_treatment_5alt
# Model description                           : Correlated RPL model; The effect of number of alternatives
# Model run at                                : 2026-02-05 13:16:01.558536
# Estimation method                           : bgw
# Model diagnosis                             : Relative function convergence
# Optimisation diagnosis                      : Maximum found
# hessian properties                     : Negative definite
# maximum eigenvalue                     : -2.944178
# reciprocal of condition number         : 0.00253744
# Number of individuals                       : 304
# Number of rows in database                  : 2432
# Number of modelled outcomes                 : 2432
# 
# Number of cores used                        :  4 
# Number of inter-individual draws            : 1000 (halton)
# 
# LL(start)                                   : -3453.85
# LL at equal shares, LL(0)                   : -3914.15
# LL at observed shares, LL(C)                : -3876.24
# LL(final)                                   : -2604.68
# Rho-squared vs equal shares                  :  0.3345 
# Adj.Rho-squared vs equal shares              :  0.3179 
# Rho-squared vs observed shares               :  0.328 
# Adj.Rho-squared vs observed shares           :  0.3123 
# AIC                                         :  5339.37 
# BIC                                         :  5716.14 
# 
# Estimated parameters                        : 65
# Time taken (hh:mm:ss)                       :  02:40:59.05 
# pre-estimation                         :  00:01:55.33 
# estimation                             :  00:21:16.19 
# post-estimation                        :  02:17:47.52 
# Iterations                                  :  54  
# 
# Unconstrained optimisation.
# 
# Estimates:
#   Estimate        s.e.   t.rat.(0)    Rob.s.e. Rob.t.rat.(0)
# mu_asc1         -1.781953     0.42931    -4.15070     0.47575      -3.74553
# mu_asc2          0.105854     0.14011     0.75548     0.13949       0.75887
# mu_asc3          0.508808     0.12059     4.21915     0.13007       3.91179
# mu_asc4          0.487587     0.12340     3.95126     0.11594       4.20556
# mu_clar          0.303056     0.04393     6.89860     0.04664       6.49827
# mu_fish          0.622605     0.10599     5.87438     0.11465       5.43025
# mu_bio           0.634028     0.05329    11.89800     0.05800      10.93246
# mu_coast         0.128749     0.08829     1.45825     0.08786       1.46531
# mu_lit           0.748648     0.06121    12.23013     0.06287      11.90865
# mu_cost         -6.627173     0.23269   -28.48058     0.18301     -36.21150
# sd_asc1          3.604989     0.49506     7.28195     0.59465       6.06235
# sd_asc2         -0.902238     0.18120    -4.97914     0.23975      -3.76323
# sd_asc3          0.191416     0.20775     0.92140     0.32640       0.58644
# sd_asc4          0.391316     0.12515     3.12685     0.12712       3.07834
# sd_clar          0.034524     0.05502     0.62749     0.04599       0.75073
# sd_fish         -0.174344     0.13517    -1.28981     0.12215      -1.42727
# sd_bio           0.158558     0.07366     2.15268     0.05939       2.66971
# sd_coast         0.113069     0.13076     0.86470     0.10861       1.04109
# sd_lit           0.147092     0.08925     1.64807     0.08627       1.70494
# sd_cost          1.037462     0.08947    11.59538     0.06131      16.92020
# sd_asc1_asc2     0.812311     0.19097     4.25365     0.27311       2.97433
# sd_asc1_asc3     0.700856     0.18959     3.69668     0.30299       2.31310
# sd_asc1_asc4     0.495216     0.16762     2.95448     0.21838       2.26767
# sd_asc1_clar    -0.052094     0.06830    -0.76272     0.08981      -0.58006
# sd_asc1_fish    -0.369140     0.17559    -2.10231     0.24326      -1.51749
# sd_asc1_bio     -0.065183     0.08867    -0.73508     0.13645      -0.47771
# sd_asc1_coast   -0.282597     0.13724    -2.05911     0.17101      -1.65247
# sd_asc1_lit     -0.108562     0.08107    -1.33908     0.09594      -1.13157
# sd_asc1_cost     0.510345     0.13198     3.86688     0.13286       3.84131
# sd_asc2_asc3    -0.086949     0.20558    -0.42294     0.27678      -0.31414
# sd_asc2_asc4     0.283025     0.18981     1.49109     0.22173       1.27646
# sd_asc2_clar     0.149203     0.06779     2.20108     0.07829       1.90582
# sd_asc2_fish     0.192008     0.17360     1.10604     0.20638       0.93038
# sd_asc2_bio      0.085110     0.07524     1.13114     0.07954       1.07000
# sd_asc2_coast    0.139002     0.13602     1.02190     0.14697       0.94580
# sd_asc2_lit      0.068396     0.06953     0.98363     0.05929       1.15359
# sd_asc2_cost     2.052089     0.15544    13.20176     0.14031      14.62523
# sd_asc3_asc4    -0.234600     0.18165    -1.29147     0.21101      -1.11179
# sd_asc3_clar    -0.095703     0.06446    -1.48476     0.05228      -1.83075
# sd_asc3_fish    -0.278330     0.14980    -1.85798     0.12343      -2.25498
# sd_asc3_bio     -0.219635     0.06631    -3.31226     0.05528      -3.97336
# sd_asc3_coast   -0.136678     0.14099    -0.96940     0.15263      -0.89550
# sd_asc3_lit     -0.062935     0.07056    -0.89195     0.05949      -1.05799
# sd_asc3_cost     1.986044     0.17017    11.67098     0.16176      12.27798
# sd_asc4_clar    -0.357460     0.05096    -7.01506     0.05579      -6.40762
# sd_asc4_fish    -0.290408     0.14058    -2.06573     0.13432      -2.16210
# sd_asc4_bio     -0.081599     0.06440    -1.26711     0.06774      -1.20466
# sd_asc4_coast   -0.009892     0.11828    -0.08364     0.11333      -0.08729
# sd_asc4_lit     -0.045461     0.06528    -0.69644     0.05229      -0.86934
# sd_asc4_cost     0.293099     0.09907     2.95858     0.06934       4.22685
# sd_clar_fish    -0.824525     0.13710    -6.01384     0.15435      -5.34206
# sd_clar_bio     -0.103563     0.06626    -1.56306     0.05718      -1.81106
# sd_clar_coast   -0.095011     0.12390    -0.76686     0.11442      -0.83037
# sd_clar_lit     -0.153070     0.06795    -2.25274     0.05140      -2.97775
# sd_clar_cost    -1.753533     0.12387   -14.15570     0.08571     -20.45959
# sd_fish_bio      0.495983     0.06117     8.10877     0.05834       8.50196
# sd_fish_coast   -0.058646     0.11297    -0.51915     0.09866      -0.59441
# sd_fish_lit      0.521579     0.06806     7.66358     0.05870       8.88527
# sd_fish_cost    -0.344893     0.08377    -4.11725     0.07164      -4.81403
# sd_bio_coast     0.338255     0.13841     2.44381     0.12492       2.70773
# sd_bio_lit      -0.023455     0.07135    -0.32874     0.05300      -0.44252
# sd_bio_cost     -0.374675     0.09122    -4.10742     0.06367      -5.88510
# sd_coast_lit     0.479954     0.06189     7.75537     0.05936       8.08593
# sd_coast_cost   -0.823746     0.12005    -6.86153     0.09565      -8.61204
# sd_lit_cost     -3.179869     0.19683   -16.15578     0.15128     -21.01936


#######
## SAVE RESULTS
#######

apollo_saveOutput(model)



#######
##  STANDARD DEVIATIONS OF THE RANDOM PARAMETERS AND THEIR STANDARD ERRORS
#######

choleski.cov <- t(matrix (c(model$estimate["sd_asc1"]      , 0                              , 0                             , 0                              , 0                              , 0                               , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_asc2"] , model$estimate["sd_asc2"]      , 0                             , 0                              , 0                              , 0                               , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_asc3"] , model$estimate["sd_asc2_asc3"] ,model$estimate["sd_asc3"]      , 0                              , 0                              , 0                               , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_asc4"] , model$estimate["sd_asc2_asc4"] ,model$estimate["sd_asc3_asc4"] , model$estimate["sd_asc4"]      , 0                              , 0                               , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_clar"] , model$estimate["sd_asc2_clar"] ,model$estimate["sd_asc3_clar"] , model$estimate["sd_asc4_clar"] , model$estimate["sd_clar"]      , 0                               , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_fish"] , model$estimate["sd_asc2_fish"] ,model$estimate["sd_asc3_fish"] , model$estimate["sd_asc4_fish"] , model$estimate["sd_clar_fish"] , model$estimate["sd_fish"]       , 0                             , 0                              , 0                             , 0                        ,
                            model$estimate["sd_asc1_bio"]  , model$estimate["sd_asc2_bio"]  ,model$estimate["sd_asc3_bio"]  , model$estimate["sd_asc4_bio"]  , model$estimate["sd_clar_bio"]  , model$estimate["sd_fish_bio"]   , model$estimate["sd_bio"]      , 0                              , 0                             , 0                        ,     
                            model$estimate["sd_asc1_coast"], model$estimate["sd_asc2_coast"],model$estimate["sd_asc3_coast"], model$estimate["sd_asc4_coast"], model$estimate["sd_clar_coast"], model$estimate["sd_fish_coast"] , model$estimate["sd_bio_coast"], model$estimate["sd_coast"]     , 0                             , 0                        ,
                            model$estimate["sd_asc1_lit"]  , model$estimate["sd_asc2_lit"]  ,model$estimate["sd_asc3_lit"]  , model$estimate["sd_asc4_lit"]  , model$estimate["sd_clar_lit"]  , model$estimate["sd_fish_lit"]   , model$estimate["sd_bio_lit"]  , model$estimate["sd_coast_lit"] , model$estimate["sd_lit"]      , 0                        ,      
                            model$estimate["sd_asc1_cost"] , model$estimate["sd_asc2_cost"] ,model$estimate["sd_asc3_cost"] , model$estimate["sd_asc4_cost"] , model$estimate["sd_clar_cost"] , model$estimate["sd_fish_cost"]  , model$estimate["sd_bio_cost"] , model$estimate["sd_coast_cost"], model$estimate["sd_lit_cost"] , model$estimate["sd_cost"]), 10))

colnames(choleski.cov) <- c("mu_asc1","mu_asc2","mu_asc3","mu_asc4","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
rownames(choleski.cov) <- c("mu_asc1","mu_asc2","mu_asc3","mu_asc4","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
choleski.cov
#              mu_asc1     mu_asc2     mu_asc3      mu_asc4     mu_clar     mu_fish      mu_bio   mu_coast     mu_lit  mu_cost
# mu_asc1   3.60498892  0.00000000  0.00000000  0.000000000  0.00000000  0.00000000  0.00000000  0.0000000  0.0000000 0.000000
# mu_asc2   0.81231127 -0.90223764  0.00000000  0.000000000  0.00000000  0.00000000  0.00000000  0.0000000  0.0000000 0.000000
# mu_asc3   0.70085623 -0.08694898  0.19141604  0.000000000  0.00000000  0.00000000  0.00000000  0.0000000  0.0000000 0.000000
# mu_asc4   0.49521638  0.28302504 -0.23460049  0.391316490  0.00000000  0.00000000  0.00000000  0.0000000  0.0000000 0.000000
# mu_clar  -0.05209408  0.14920343 -0.09570295 -0.357460129  0.03452381  0.00000000  0.00000000  0.0000000  0.0000000 0.000000
# mu_fish  -0.36914002  0.19200839 -0.27832998 -0.290408457 -0.82452489 -0.17434389  0.00000000  0.0000000  0.0000000 0.000000
# mu_bio   -0.06518294  0.08510958 -0.21963526 -0.081598641 -0.10356320  0.49598326  0.15855817  0.0000000  0.0000000 0.000000
# mu_coast -0.28259747  0.13900228 -0.13667772 -0.009892333 -0.09501135 -0.05864641  0.33825477  0.1130687  0.0000000 0.000000
# mu_lit   -0.10856175  0.06839573 -0.06293501 -0.045461190 -0.15306957  0.52157934 -0.02345518  0.4799537  0.1470923 0.000000
# mu_cost   0.51034486  2.05208910  1.98604438  0.293099463 -1.75353325 -0.34489255 -0.37467481 -0.8237463 -3.1798690 1.037462


var.cov           <- choleski.cov %*% t(choleski.cov)  # variance-covariance matriz of the random parameters
colnames(var.cov) <- c("mu_asc1","mu_asc2","mu_asc3","mu_asc4","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
rownames(var.cov) <- c("mu_asc1","mu_asc2","mu_asc3","mu_asc4","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
var.cov             # variance-covariance matriz of the random parameters
#             mu_asc1    mu_asc2     mu_asc3     mu_asc4     mu_clar    mu_fish      mu_bio    mu_coast      mu_lit     mu_cost
# mu_asc1  12.9959451  2.9283731  2.52657894  1.78524958 -0.18779859 -1.3307457 -0.23498376 -1.01876075 -0.39136392  1.83978756
# mu_asc2   2.9283731  1.4738824  0.64776206  0.14691401 -0.17693356 -0.4730938 -0.12973790 -0.35497020 -0.14989514 -1.43691315
# mu_asc3   2.5265789  0.6477621  0.53539968  0.27756045 -0.06780263 -0.3286858 -0.09512577 -0.23630861 -0.09407989  0.55941205
# mu_asc4   1.7852496  0.1469140  0.27756045  0.53350843 -0.10099762 -0.1768063  0.01140413 -0.07241214 -0.03742898  0.48229139
# mu_clar  -0.1877986 -0.1769336 -0.06780263 -0.10099762  0.16310415  0.1498590  0.06270689  0.04879770  0.03284939 -0.07578756
# mu_fish  -1.3307457 -0.4730938 -0.32868584 -0.17680627  0.14985902  1.0451733  0.12415019  0.26048583  0.11920156  1.07369708
# mu_bio   -0.2349838 -0.1297379 -0.09512577  0.01140413  0.06270689  0.1241502  0.34825587  0.09546251  0.30125781 -0.36760238
# mu_coast -1.0187607 -0.3549702 -0.23630861 -0.07241214  0.04879770  0.2604858  0.09546251  0.25762901  0.07952650 -0.16636777
# mu_lit   -0.3913639 -0.1498951 -0.09407989 -0.03742898  0.03284939  0.1192016  0.30125781  0.07952650  0.57050833 -0.81914868
# mu_cost   1.8397876 -1.4369131  0.55941205  0.48229139 -0.07578756  1.0736971 -0.36760238 -0.16636777 -0.81914868 23.70246396
sd.rand.param     <- sqrt(diag(var.cov))               # standard deviations of of the random parameters
sd.rand.param
# mu_asc1   mu_asc2   mu_asc3   mu_asc4   mu_clar   mu_fish    mu_bio  mu_coast    mu_lit   mu_cost 
# 3.6049889 1.2140356 0.7317101 0.7304166 0.4038615 1.0223372 0.5901321 0.5075717 0.7553200 4.8685176

## Standard deviation of the random parameters and their standard errors ----
deltaMethod_settings <-
  list(
    expression = c(
      rob_se_sd_asc1  = "sqrt(sd_asc1^2)",        
      rob_se_sd_asc2  = "sqrt(sd_asc1_asc2^2   + sd_asc2^2)",        
      rob_se_sd_asc3  = "sqrt(sd_asc1_asc3^2   + sd_asc2_asc3^2   + sd_asc3^2)",        
      rob_se_sd_asc4  = "sqrt(sd_asc1_asc4^2   + sd_asc2_asc4^2   + sd_asc3_asc4^2   + sd_asc4^2)",       
      rob_se_sd_clar  = "sqrt(sd_asc1_clar^2   + sd_asc2_clar^2   + sd_asc3_clar^2   + sd_asc4_clar^2   + sd_clar^2)",        
      rob_se_sd_fish  = "sqrt(sd_asc1_fish^2   + sd_asc2_fish^2   + sd_asc3_fish^2   + sd_asc4_fish^2   + sd_clar_fish^2   + sd_fish^2)",        
      rob_se_sd_bio   = "sqrt(sd_asc1_bio^2    + sd_asc2_bio^2    + sd_asc3_bio^2    + sd_asc4_bio^2    + sd_clar_bio^2    + sd_fish_bio^2    + sd_bio^2)",            
      rob_se_sd_coast = "sqrt(sd_asc1_coast^2  + sd_asc2_coast^2  + sd_asc3_coast^2  + sd_asc4_coast^2  + sd_clar_coast^2  + sd_fish_coast^2  + sd_bio_coast^2      + sd_coast^2)",      
      rob_se_sd_lit   = "sqrt(sd_asc1_lit^2    + sd_asc2_lit^2    + sd_asc3_lit^2    + sd_asc4_lit^2    + sd_clar_lit^2    + sd_fish_lit^2    + sd_bio_lit^2        + sd_coast_lit^2  + sd_lit^2)",       
      rob_se_sd_cost  = "sqrt(sd_asc1_cost^2   + sd_asc2_cost^2   + sd_asc3_cost^2   + sd_asc4_cost^2   + sd_clar_cost^2   + sd_fish_cost^2   + sd_bio_cost^2       + sd_coast_cost^2 + sd_lit_cost^2  + sd_cost^2)"      
    ),
    varcov="robust"
  )


est_sd <- apollo_deltaMethod(model, deltaMethod_settings)
est_sd      # standard deviations of the random parameters and their standar errors 
#           Expression  Value   s.e. t-ratio (0)
# 1   rob_se_sd_asc1 3.6050 0.5947        6.06
# 2   rob_se_sd_asc2 1.2140 0.2140        5.67
# 3   rob_se_sd_asc3 0.7317 0.2931        2.50
# 4   rob_se_sd_asc4 0.7304 0.2213        3.30
# 5   rob_se_sd_clar 0.4039 0.0528        7.64
# 6   rob_se_sd_fish 1.0223 0.1362        7.50
# 7    rob_se_sd_bio 0.5901 0.0605        9.75
# 8  rob_se_sd_coast 0.5076 0.1113        4.56
# 9    rob_se_sd_lit 0.7553 0.0646       11.68
# 10  rob_se_sd_cost 4.8685 0.2579       18.88
# 

