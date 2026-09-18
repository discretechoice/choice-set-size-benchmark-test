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
  modelName       = "LC_6Alt_4classes_with_covariates",
  modelDescr      = "Simple LC model, no covariates in class allocation model; The effect of number of alternatives",
  indivID         = "id",
  nCores          = 1,
  outputDirectory = "output.LCM.6Alt_4classes.withCov.Bgw_estimate" 
  )

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = Data

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

apollo_beta = c(asc_1_a     = 0.3492049,
                asc_1_b     = 2.667926,
                asc_1_c     = 0.3622543,
                asc_1_d     = -0.5766841,
                asc_2_a     = 0.4139872,
                asc_2_b     = 1.417355,
                asc_2_c     = -0.06364923,
                asc_2_d     = -0.2102706,
                asc_3_a     = 0.8613119,
                asc_3_b     = -1.353866,
                asc_3_c     = -0.05999089,
                asc_3_d     = 0.2833732,
                asc_4_a     = 1.022322,
                asc_4_b     = 0.793907,
                asc_4_c     = 0.1019766,
                asc_4_d     = 0.007447754,
                asc_5_a     = 0.2242422,
                asc_5_b     = 0.3757841,
                asc_5_c     = 0.05776277,
                asc_5_d     = 0.139613,
                #
                b_clar_a = 0.2853184,
                b_clar_b = -0.1144813,
                b_clar_c = 0.08554741,
                b_clar_d = 0.2630881,
                #
                b_fish_a = 0.4989306,
                b_fish_b = -0.4321381,
                b_fish_c = 0.9465844,
                b_fish_d = 0.5128036,
                #
                b_bio_a = 0.1778487,
                b_bio_b = 0.1371159,
                b_bio_c = 0.8984725,
                b_bio_d = 0.419897,
                #
                b_coast_a = 0.0411226,
                b_coast_b = 0.05384978,
                b_coast_c = -0.0002101815,
                b_coast_d = 0.1033396,
                #
                b_lit_a = 0.110215,
                b_lit_b = 0.2147364,
                b_lit_c = 0.866569,
                b_lit_d = 0.5617078,
                #
                b_cost_a = -0.0005219253,
                b_cost_b = -0.02949596,
                b_cost_c = 0.00311624,
                b_cost_d = -0.01806568,
                #
                delta_a   = 1.573678,
                delta_b   = -1.280909,
                delta_c   = -2.220394,
                delta_d   = 0.0,
                #
                gamma_age_a = -0.02162554,
                gamma_age_b = 0.01339487,
                gamma_age_c = 0.0286762,
                gamma_age_d = 0.0,
                #
                gamma_female_a = -0.08501083,
                gamma_female_b = -0.5865018,
                gamma_female_c = 0.4077142,
                gamma_female_d = 0.0,
                #
                gamma_educ2_a = -0.6040904,
                gamma_educ2_b = 0.1374762,
                gamma_educ2_c = 0.9714457,
                gamma_educ2_d = 0.0,
                #
                gamma_educ3_a = -0.6110297,
                gamma_educ3_b = 0.5465097,
                gamma_educ3_c = 1.698718,
                gamma_educ3_d = 0.0,
                #
                gamma_educ4_a = -0.8053438,
                gamma_educ4_b = -0.02615535,
                gamma_educ4_c = 1.460007,
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
  lcpars[["asc_5"]]   = list(asc_5_a, asc_5_b, asc_5_c, asc_5_d)
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
    alternatives = c(alt1=1, alt2=2, alt3=3, alt4=4, alt5=5, alt6=6 ),
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
    V[["alt5"]] = asc_5[[s]] + b_clar[[s]]*clar5 + b_fish[[s]]*fish5 + b_bio[[s]]*bio5 + b_coast[[s]]*coast5 + b_lit[[s]]*lit5 + b_cost[[s]]*cost5
    V[["alt6"]] =            + b_clar[[s]]*clar6 + b_fish[[s]]*fish6 + b_bio[[s]]*bio6 + b_coast[[s]]*coast6 + b_lit[[s]]*lit6 + b_cost[[s]]*cost6
    

       
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


# Model name                                  : LC_6Alt_4classes_with_covariates
# Model description                           : Simple LC model, no covariates in class allocation model; The effect of number of alternatives
# Model run at                                : 2025-04-07 12:33:15.110573
# Estimation method                           : bgw
# Model diagnosis                             : Relative function convergence
# Optimisation diagnosis                      : Maximum found
# hessian properties                     : Negative definite
# maximum eigenvalue                     : -0.262129
# reciprocal of condition number         : 7.15361e-08
# Number of individuals                       : 281
# Number of rows in database                  : 2248
# Number of modelled outcomes                 : 2248
# 
# Number of cores used                        :  1 
# Model without mixing
# 
# LL(start)                                   : -2741.47
# LL (whole model) at equal shares, LL(0)     : -4027.88
# LL (whole model) at observed shares, LL(C)  : -4007.2
# LL(final, whole model)                      : -2741.38
# Rho-squared vs equal shares                  :  0.3194 
# Adj.Rho-squared vs equal shares              :  0.304 
# Rho-squared vs observed shares               :  0.3159 
# Adj.Rho-squared vs observed shares           :  0.3054 
# AIC                                         :  5606.75 
# BIC                                         :  5961.25 
# 
# LL(0,Class_1)                    : -4027.88
# LL(final,Class_1)                : -4054.93
# LL(0,Class_2)                    : -4027.88
# LL(final,Class_2)                : -11838.86
# LL(0,Class_3)                    : -4027.88
# LL(final,Class_3)                : -4976.58
# LL(0,Class_4)                    : -4027.88
# LL(final,Class_4)                : -4818.2
# 
# Estimated parameters                        : 62
# Time taken (hh:mm:ss)                       :  00:01:16.24 
# pre-estimation                         :  00:00:6.12 
# estimation                             :  00:00:7.31 
# post-estimation                        :  00:01:2.82 
# Iterations                                  :  41  
# 
# Unconstrained optimisation.
# 
# Estimates:
#   Estimate        s.e.   t.rat.(0)    Rob.s.e. Rob.t.rat.(0)
# asc_1_a           0.251540    0.384450    0.654284    0.655803      0.383559
# asc_1_b           2.670740    1.027472    2.599332    0.952588      2.803669
# asc_1_c           0.498685    0.629797    0.791819    0.749912      0.664991
# asc_1_d          -0.440856    0.372187   -1.184500    0.724473     -0.608519
# asc_2_a           0.491002    0.229497    2.139475    0.402321      1.220425
# asc_2_b           1.427740    0.690622    2.067323    0.846622      1.686396
# asc_2_c          -0.058212    0.145037   -0.401362    0.148464     -0.392098
# asc_2_d          -0.241011    0.195572   -1.232335    0.216706     -1.112153
# asc_3_a           0.923479    0.216604    4.263454    0.327448      2.820233
# asc_3_b          -1.349796    1.232840   -1.094867    1.026456     -1.315006
# asc_3_c          -0.051616    0.153399   -0.336480    0.166840     -0.309372
# asc_3_d           0.281935    0.170859    1.650103    0.154367      1.826400
# asc_4_a           1.092870    0.212066    5.153445    0.326379      3.348471
# asc_4_b           0.791668    0.737822    1.072980    0.516953      1.531413
# asc_4_c           0.113433    0.143861    0.788491    0.166059      0.683087
# asc_4_d           0.026156    0.188080    0.139066    0.239516      0.109202
# asc_5_a           0.261940    0.229778    1.139970    0.321150      0.815629
# asc_5_b           0.362337    0.820848    0.441418    1.159449      0.312508
# asc_5_c           0.068781    0.142199    0.483697    0.162652      0.422874
# asc_5_d           0.101261    0.192958    0.524781    0.225836      0.448382
# b_clar_a          0.276739    0.058839    4.703353    0.081452      3.397565
# b_clar_b         -0.111514    0.196529   -0.567420    0.209306     -0.532781
# b_clar_c          0.098844    0.059764    1.653912    0.095227      1.037980
# b_clar_d          0.276128    0.052577    5.251867    0.079002      3.495220
# b_fish_a          0.497656    0.162193    3.068301    0.239797      2.075325
# b_fish_b         -0.438169    0.432849   -1.012290    0.448087     -0.977867
# b_fish_c          0.936540    0.125128    7.484659    0.176454      5.307573
# b_fish_d          0.516783    0.116088    4.451642    0.136671      3.781206
# b_bio_a           0.145288    0.077797    1.867526    0.157120      0.924695
# b_bio_b           0.136565    0.167753    0.814085    0.135929      1.004681
# b_bio_c           0.889774    0.065780   13.526528    0.091905      9.681426
# b_bio_d           0.431621    0.055405    7.790368    0.090068      4.792180
# b_coast_a        -0.022447    0.154820   -0.144990    0.295196     -0.076042
# b_coast_b         0.056319    0.465350    0.121026    0.505673      0.111375
# b_coast_c         0.017159    0.114665    0.149644    0.152464      0.112544
# b_coast_d         0.136933    0.118224    1.158249    0.181781      0.753287
# b_lit_a           0.065895    0.080835    0.815175    0.149104      0.441939
# b_lit_b           0.214604    0.169605    1.265318    0.154388      1.390035
# b_lit_c           0.866900    0.060497   14.329570    0.078922     10.984267
# b_lit_d           0.557178    0.049245   11.314465    0.074448      7.484128
# b_cost_a        1.7630e-04    0.001397    0.126212    0.003013      0.058515
# b_cost_b         -0.029694    0.007958   -3.731400    0.011444     -2.594621
# b_cost_c          0.003007  7.6519e-04    3.930176    0.001169      2.572706
# b_cost_d         -0.017277    0.001600  -10.797115    0.003546     -4.871878
# delta_a           1.772559    1.114934    1.589833    1.406635      1.260142
# delta_b          -1.258999    1.341220   -0.938697    1.256358     -1.002102
# delta_c          -2.227071    1.063433   -2.094228    1.055208     -2.110551
# delta_d           0.000000          NA          NA          NA            NA
# gamma_age_a      -0.026924    0.020481   -1.314601    0.028123     -0.957367
# gamma_age_b       0.011954    0.022329    0.535363    0.022996      0.519816
# gamma_age_c       0.028039    0.016559    1.693283    0.017788      1.576258
# gamma_age_d       0.000000          NA          NA          NA            NA
# gamma_female_a   -0.174535    0.427635   -0.408140    0.509799     -0.342361
# gamma_female_b   -0.605972    0.457767   -1.323757    0.452803     -1.338268
# gamma_female_c    0.392875    0.331960    1.183499    0.340731      1.153033
# gamma_female_d    0.000000          NA          NA          NA            NA
# gamma_educ2_a    -0.719751    0.595865   -1.207911    0.750701     -0.958773
# gamma_educ2_b     0.119612    0.757787    0.157844    0.790288      0.151353
# gamma_educ2_c     0.961636    0.643092    1.495332    0.631357      1.523124
# gamma_educ2_d     0.000000          NA          NA          NA            NA
# gamma_educ3_a    -0.684409    0.734182   -0.932206    0.850732     -0.804494
# gamma_educ3_b     0.527852    0.873539    0.604269    0.916626      0.575865
# gamma_educ3_c     1.676828    0.722299    2.321514    0.730039      2.296903
# gamma_educ3_d     0.000000          NA          NA          NA            NA
# gamma_educ4_a    -0.794411    0.683308   -1.162597    0.796601     -0.997251
# gamma_educ4_b  -8.9864e-04    0.855048   -0.001051    0.843134     -0.001066
# gamma_educ4_c     1.469378    0.694003    2.117249    0.677994      2.167243
# gamma_educ4_d     0.000000          NA          NA          NA            NA
# 
# 
# Summary of class allocation for model component :
#   Mean prob.
# Class_1      0.2103
# Class_2      0.1142
# Class_3      0.4238
# Class_4      0.2517
