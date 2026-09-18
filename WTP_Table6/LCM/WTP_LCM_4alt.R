####################################################################
####################################################################
#####
#####            WTP calculations
#####            LCM - 4 alternatives - treatment 3
#####
####################################################################
####################################################################


# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

set.seed(2345)

# set working directory
setwd(".") 

### Clear memory
rm(list = ls())

#### Loading R packages
library(apollo)   # Load Apollo library
library(bgw)      # Load bgw library

#####################################################################
# ################################################################# #
# Open the data and estimate the model
# ################################################################# #
#####################################################################
    
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
  modelName       = "LC_4Alt_3classes_with_covariates",
  modelDescr      = "Simple LC model, with covariates in class allocation model; The effect of number of alternatives",
  indivID         = "id",
  nCores          = 1,
  outputDirectory = "output.LCM.4Alt_3classes.withCov.Bgw_estimate"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = Data

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(asc_1_a     = -1.25297  ,
                asc_1_b     = 4.977031  ,
                asc_1_c     = -0.4632415  ,
                asc_2_a     = 0.1533052  ,
                asc_2_b     = 0.8421055  ,
                asc_2_c     = 0.1976408  ,
                asc_3_a     = 0.3164073  ,
                asc_3_b     = -0.3124516  ,
                asc_3_c     = 0.4368492  ,
                #
                b_clar_a = 0.2073791,
                b_clar_b = -0.1630026,
                b_clar_c = 0.2846516,
                #
                b_fish_a = 0.5512486,
                b_fish_b = 1.605205,
                b_fish_c = 0.3280813,
                #
                b_bio_a = 0.4213746,
                b_bio_b = 0.4075678,
                b_bio_c = 0.3578163,
                #
                b_coast_a = 0.07863925,
                b_coast_b = -0.9101974,
                b_coast_c = 0.007891287,
                #
                b_lit_a = 0.4449357,
                b_lit_b = 0.09458577,
                b_lit_c = 0.4199524,
                #
                b_cost_a = -1.794094e-05,
                b_cost_b = -3.405521e-05,
                b_cost_c = -0.0001854871,
                #
                delta_a   = 0.8422471,
                delta_b   = -0.3337294,
                delta_c   = 0.0,
                #
                gamma_age_a = -0.0004602528,
                gamma_age_b = 0.01059137,
                gamma_age_c = 0.0,
                #
                gamma_female_a = 0.03534963,
                gamma_female_b = -1.326875,
                gamma_female_c = 0.0,
                #
                gamma_educ2_a = 0.1393792,
                gamma_educ2_b = -0.9739388,
                gamma_educ2_c = 0.0,
                #
                gamma_educ3_a = 0.5864436,
                gamma_educ3_b = 0.1650051,
                gamma_educ3_c = 0.0,
                #
                gamma_educ4_a = -0.1497825,
                gamma_educ4_b = -0.9251268,
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
  lcpars[["asc_3"]]   = list(asc_3_a, asc_3_b, asc_3_c)
  lcpars[["b_clar"]]  = list(b_clar_a  , b_clar_b  ,b_clar_c  )
  lcpars[["b_fish"]]  = list(b_fish_a  , b_fish_b  ,b_fish_c  )
  lcpars[["b_bio"]]   = list(b_bio_a   , b_bio_b   ,b_bio_c   )
  lcpars[["b_coast"]] = list(b_coast_a , b_coast_b ,b_coast_c )
  lcpars[["b_lit"]]   = list(b_lit_a   , b_lit_b   ,b_lit_c   )
  lcpars[["b_cost"]]  = list(b_cost_a  , b_cost_b  ,b_cost_c  )
  
  V=list()
  V[["class_a"]] = delta_a + gamma_age_a * age + gamma_female_a * female + gamma_educ2_a * educ2 + gamma_educ3_a * educ3 + + gamma_educ4_a * educ4
  V[["class_b"]] = delta_b + gamma_age_b * age + gamma_female_b * female + gamma_educ2_b * educ2 + gamma_educ3_b * educ3 + + gamma_educ4_b * educ4
  V[["class_c"]] = delta_c + gamma_age_c * age + gamma_female_c * female + gamma_educ2_c * educ2 + gamma_educ3_c * educ3 + + gamma_educ4_c * educ4
  
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
    alternatives = c(alt1=1, alt2=2, alt3=3, alt4=4 ),
    #avail        = list(alt1=av_1, alt2=av_2, alt3=av_3, alt4=av_4, alt5=av_5, alt6=av_6),
    choiceVar    = choibio
  )
  
  ### Loop over classes
  for(s in 1:3){                     # This should be "for(s in 1:"number of classes"){" when we use "bgw" in the estimation model
    
    ### Compute class-specific utilities
    V=list()
    V[["alt1"]] = asc_1[[s]] + b_clar[[s]]*clar1 + b_fish[[s]]*fish1 + b_bio[[s]]*bio1 + b_coast[[s]]*coast1 + b_lit[[s]]*lit1 + b_cost[[s]]*cost1
    V[["alt2"]] = asc_2[[s]] + b_clar[[s]]*clar2 + b_fish[[s]]*fish2 + b_bio[[s]]*bio2 + b_coast[[s]]*coast2 + b_lit[[s]]*lit2 + b_cost[[s]]*cost2
    V[["alt3"]] = asc_3[[s]] + b_clar[[s]]*clar3 + b_fish[[s]]*fish3 + b_bio[[s]]*bio3 + b_coast[[s]]*coast3 + b_lit[[s]]*lit3 + b_cost[[s]]*cost3
    V[["alt4"]] =              b_clar[[s]]*clar4 + b_fish[[s]]*fish4 + b_bio[[s]]*bio4 + b_coast[[s]]*coast4 + b_lit[[s]]*lit4 + b_cost[[s]]*cost4
    
    
    
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

apollo_modelOutput(model)

# ################################################################# # 
##
#  MEASURE THE DISTRIBUTION OF THE WTP AMONG INDIVIDUALS
##
# ################################################################# #
# UcConditional class allocation probabilities 
UNcond.Cl.all.prob  = apollo_unconditionals(model,apollo_probabilities,apollo_inputs)

UN_wtp_clar_a  <- -(UNcond.Cl.all.prob[["b_clar"]][[1]]/UNcond.Cl.all.prob[["b_cost"]][[1]])
UN_wtp_clar_b  <- -(UNcond.Cl.all.prob[["b_clar"]][[2]]/UNcond.Cl.all.prob[["b_cost"]][[2]])
UN_wtp_clar_c  <- -(UNcond.Cl.all.prob[["b_clar"]][[3]]/UNcond.Cl.all.prob[["b_cost"]][[3]])

UN_wtp_fish_a  <- -(UNcond.Cl.all.prob[["b_fish"]][[1]]/UNcond.Cl.all.prob[["b_cost"]][[1]])
UN_wtp_fish_b  <- -(UNcond.Cl.all.prob[["b_fish"]][[2]]/UNcond.Cl.all.prob[["b_cost"]][[2]])
UN_wtp_fish_c  <- -(UNcond.Cl.all.prob[["b_fish"]][[3]]/UNcond.Cl.all.prob[["b_cost"]][[3]])

UN_wtp_bio_a  <- -(UNcond.Cl.all.prob[["b_bio"]][[1]]/UNcond.Cl.all.prob[["b_cost"]][[1]])
UN_wtp_bio_b  <- -(UNcond.Cl.all.prob[["b_bio"]][[2]]/UNcond.Cl.all.prob[["b_cost"]][[2]])
UN_wtp_bio_c  <- -(UNcond.Cl.all.prob[["b_bio"]][[3]]/UNcond.Cl.all.prob[["b_cost"]][[3]])

UN_wtp_coast_a  <- -(UNcond.Cl.all.prob[["b_coast"]][[1]]/UNcond.Cl.all.prob[["b_cost"]][[1]])
UN_wtp_coast_b  <- -(UNcond.Cl.all.prob[["b_coast"]][[2]]/UNcond.Cl.all.prob[["b_cost"]][[2]])
UN_wtp_coast_c  <- -(UNcond.Cl.all.prob[["b_coast"]][[3]]/UNcond.Cl.all.prob[["b_cost"]][[3]])

UN_wtp_lit_a  <- -(UNcond.Cl.all.prob[["b_lit"]][[1]]/UNcond.Cl.all.prob[["b_cost"]][[1]])
UN_wtp_lit_b  <- -(UNcond.Cl.all.prob[["b_lit"]][[2]]/UNcond.Cl.all.prob[["b_cost"]][[2]])
UN_wtp_lit_c  <- -(UNcond.Cl.all.prob[["b_lit"]][[3]]/UNcond.Cl.all.prob[["b_cost"]][[3]])

summary(as.data.frame(UNcond.Cl.all.prob[["pi_values"]]))

LCM_UN_WTP.clar  <- UNcond.Cl.all.prob[["pi_values"]][[1]] * UN_wtp_clar_a   +  UNcond.Cl.all.prob[["pi_values"]][[2]] * UN_wtp_clar_b   +  UNcond.Cl.all.prob[["pi_values"]][[3]] * UN_wtp_clar_c 
LCM_UN_WTP.fish  <- UNcond.Cl.all.prob[["pi_values"]][[1]] * UN_wtp_fish_a   +  UNcond.Cl.all.prob[["pi_values"]][[2]] * UN_wtp_fish_b   +  UNcond.Cl.all.prob[["pi_values"]][[3]] * UN_wtp_fish_c 
LCM_UN_WTP.bio   <- UNcond.Cl.all.prob[["pi_values"]][[1]] * UN_wtp_bio_a    +  UNcond.Cl.all.prob[["pi_values"]][[2]] * UN_wtp_bio_b    +  UNcond.Cl.all.prob[["pi_values"]][[3]] * UN_wtp_bio_c  
LCM_UN_WTP.coast <- UNcond.Cl.all.prob[["pi_values"]][[1]] * UN_wtp_coast_a  +  UNcond.Cl.all.prob[["pi_values"]][[2]] * UN_wtp_coast_b  +  UNcond.Cl.all.prob[["pi_values"]][[3]] * UN_wtp_coast_c
LCM_UN_WTP.lit   <- UNcond.Cl.all.prob[["pi_values"]][[1]] * UN_wtp_lit_a    +  UNcond.Cl.all.prob[["pi_values"]][[2]] * UN_wtp_lit_b    +  UNcond.Cl.all.prob[["pi_values"]][[3]] * UN_wtp_lit_c  


# ################################################################# #
#  Save results
# ################################################################# #

simu.WTP.Results <- cbind(LCM_UN_WTP.clar ,
                          LCM_UN_WTP.fish ,
                          LCM_UN_WTP.bio  ,
                          LCM_UN_WTP.coast,
                          LCM_UN_WTP.lit    )


median_CI_WTP <- rbind(c(median(LCM_UN_WTP.clar ), quantile(LCM_UN_WTP.clar , 0.025), quantile(LCM_UN_WTP.clar , 0.975)),
                       c(median(LCM_UN_WTP.fish ), quantile(LCM_UN_WTP.fish , 0.025), quantile(LCM_UN_WTP.fish , 0.975)),
                       c(median(LCM_UN_WTP.bio  ), quantile(LCM_UN_WTP.bio  , 0.025), quantile(LCM_UN_WTP.bio  , 0.975)),
                       c(median(LCM_UN_WTP.coast), quantile(LCM_UN_WTP.coast, 0.025), quantile(LCM_UN_WTP.coast, 0.975)),
                       c(median(LCM_UN_WTP.lit  ), quantile(LCM_UN_WTP.lit  , 0.025), quantile(LCM_UN_WTP.lit  , 0.975)))

row.names(median_CI_WTP) <- c("mu_clar","mu_fish","mu_bio","mu_coast","mu_lit")
colnames(median_CI_WTP)  <- c("median", "CI_0.025", "CI_0.975")



write.table(simu.WTP.Results, file="./simu_WTP_4Alt.csv", sep=" ", row.names = TRUE, col.names=TRUE)
write.table(median_CI_WTP, file="./median_CI_WTP_4Alt.csv", sep=" ", row.names = TRUE, col.names=TRUE)


