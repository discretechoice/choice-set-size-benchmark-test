####################################################################
####################################################################
#####
#####            MNL - 6 alternatives - treatment 5
#####            SIMULATION - GENERATE RESPONSES  
#####
####################################################################
####################################################################


set.seed(2345)

# set working directory
setwd(".") 

### Clear memory
rm(list = ls())

#### Loading R packages
library(apollo)
library(evd)
library(dplyr)

library(ggplot2)
library(patchwork)
library(plotrix)


# Reading the database 
data.MNL.full <- read.table("../../PrefMatWideFormatFullBiogeme.txt", header=TRUE)

# Choose treatment (the treatment that we want to test)
choose.treatment <- 5   ## the treatment that we choose to test
Data.1 <- data.MNL.full[-which(data.MNL.full$treat != choose.treatment),]

# only task <=8  (we only consider the task from 1 to 8, so we drop those above 8)
Data <- Data.1[-which(Data.1$task > 8),]
length(Data[,1])

# Socio-Demographic covariates
##
# Age
Data$age <- 2016 - Data$v371
# Gender
Data$female <- 0
Data$female[Data$v372==1] <- 1
# Education
table(Data$v375)  # How many observations are there in each "Education" category?
# Category 6 of v375 has very few observations, so it is removed from the analysis to avoid estimation issues (too few cases per category).
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


##
# Setting parameters
##
n.alternatives         <- choose.treatment + 1           # the number of alternatives in the treatment that we are testing is equal to "choose.treatment + 1"
n.choices              <- 8                              # Number of choice occasions of one individual 
n.individuals          <- length(Data[,1])/n.choices     # Number of individuals
n.rows                 <- n.individuals * n.choices      # Number of rows


NumIter  <- 10000

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


# Creating new variables
Data               <- Data[order(Data$id, Data$task), ]
Data$choice.card   <- rep(seq(1,n.choices)    , n.individuals)     # ID of choice card 
Data$id.individual <- rep(seq(1,n.individuals),each=n.choices)     # ID of individual


############################
# THE PARAMETER VALUES
# from the MNL with 2 alternatives
############################

# asc1 is the SQ 

asc_1     <-  0.776581
b_clar   <-  0.297951
b_fish   <-  0.498575
b_bio    <-  0.267840
b_coast  <-  0.187876
b_lit    <-  0.425977
b_cost   <- -0.006938

#############
##  MATRIX TO SAVE THE RESULTS
##  One matrix for each attribute
##  Each row of the matrix corresponds to one alternative of the attribute. Therefore, the names of the rows are the names of the alternatives
#############

clar.chosen <- matrix(0,NumIter,4)
colnames(clar.chosen) <- c("Turbid_SQ", "Rather turbid", "Rather clear", "Clear")
fish.chosen <- matrix(0,NumIter,2)
colnames(fish.chosen) <- c("Overfished_SQ", "Stable")
bio.chosen <- matrix(0,NumIter,4)
colnames(bio.chosen) <- c("Bad_SQ", "Rather bad", "Rather good", "Good")
coast.chosen <- matrix(0,NumIter,2)
colnames(coast.chosen) <- c("Strong impact_SQ", "Mainly low impact")
lit.chosen <- matrix(0,NumIter,4)
colnames(lit.chosen) <- c("Very much_SQ", "Much", "Little", "Very Little")
cost.chosen <- matrix(0,NumIter,9)
colnames(cost.chosen) <- c("0_SQ", "10", "25", "50", "80", "110", "160", "220", "300")

lowest.cost.no0  <- matrix(0,NumIter,1)
highest.cost.no0 <- matrix(0,NumIter,1)

## Create a FUNCTION "min_no_cero()" to identify the  the minimum value of cost different to cero in the alternatives
min_no_cero <- function(...) {
  valores <- c(...)
  valores <- valores[valores != 0]
  if (length(valores) == 0) return(NA)
  return(min(valores, na.rm = TRUE))
}

## Create a FUNCTION "max_no_cero()" to identify the  maximum value of cost in the alternatives
max_no_cero <- function(...) {
  valores <- c(...)
  valores <- valores[valores != 0]
  if (length(valores) == 0) return(NA)
  return(max(valores, na.rm = TRUE))
}



# Random errors generation Gumbel(0,1)
error1 <- rgumbel(n.individuals * n.choices * NumIter ,loc=0, scale=1)
error2 <- rgumbel(n.individuals * n.choices * NumIter ,loc=0, scale=1)
error3 <- rgumbel(n.individuals * n.choices * NumIter ,loc=0, scale=1)
error4 <- rgumbel(n.individuals * n.choices * NumIter ,loc=0, scale=1)
error5 <- rgumbel(n.individuals * n.choices * NumIter ,loc=0, scale=1)
error6 <- rgumbel(n.individuals * n.choices * NumIter ,loc=0, scale=1)

######
## LOOP TO GENERATE RESPONSES "NumIter" TIMES
######


# OPEM LOOP
for (ite in 1:NumIter){

       Data.ite <- Data  
       
       # Random errors generation Gumbel(0,1) for each alternative (the number of errors that we create depends on the number of alternatives we have (TREATMENT))
       
       Data.ite$error1 <- error1[(((ite-1)*(n.individuals*n.choices))+1):(ite*(n.individuals*n.choices))]
       Data.ite$error2 <- error2[(((ite-1)*(n.individuals*n.choices))+1):(ite*(n.individuals*n.choices))]
       Data.ite$error3 <- error3[(((ite-1)*(n.individuals*n.choices))+1):(ite*(n.individuals*n.choices))]
       Data.ite$error4 <- error4[(((ite-1)*(n.individuals*n.choices))+1):(ite*(n.individuals*n.choices))]
       Data.ite$error5 <- error5[(((ite-1)*(n.individuals*n.choices))+1):(ite*(n.individuals*n.choices))]
       Data.ite$error6 <- error6[(((ite-1)*(n.individuals*n.choices))+1):(ite*(n.individuals*n.choices))]
       
       
       ######## 
       # Utility generation
       ########
       
       # the number of utilities that we create depends on the number of alternatives we have (TREATMENT) 
       
       Data.ite$utility1 <- (                                       # Creating utility 1 
                                asc_1
                              + b_clar   * Data.ite$clar1 
                              + b_fish   * Data.ite$fish1
                              + b_bio    * Data.ite$bio1
                              + b_coast  * Data.ite$coast1
                              + b_lit    * Data.ite$lit1
                              + b_cost   * Data.ite$cost1
                              +               Data.ite$error1
       )
       
       
       
       Data.ite$utility2 <- (                                       # Creating utility 2 
                                # We assume asc = 0 for all the alternatives different from the SQ 
                                b_clar  * Data.ite$clar2 
                              + b_fish  * Data.ite$fish2
                              + b_bio   * Data.ite$bio2
                              + b_coast * Data.ite$coast2
                              + b_lit   * Data.ite$lit2
                              + b_cost  * Data.ite$cost2
                              +              Data.ite$error2
       )
       
       
       Data.ite$utility3 <- (                                       # Creating utility 3  
                                # We assume asc = 0 for all the alternatives different from the SQ 
                                b_clar  * Data.ite$clar3 
                              + b_fish  * Data.ite$fish3
                              + b_bio   * Data.ite$bio3
                              + b_coast * Data.ite$coast3
                              + b_lit   * Data.ite$lit3
                              + b_cost  * Data.ite$cost3
                              +              Data.ite$error3
       )
       
       
       Data.ite$utility4 <- (                                       # Creating utility 4 
                                # We assume asc = 0 for all the alternatives different from the SQ 
                                b_clar  * Data.ite$clar4 
                              + b_fish  * Data.ite$fish4
                              + b_bio   * Data.ite$bio4
                              + b_coast * Data.ite$coast4
                              + b_lit   * Data.ite$lit4
                              + b_cost  * Data.ite$cost4
                              +              Data.ite$error4
       )
       
       Data.ite$utility5 <- (                                       # Creating utility 4 
                                # We assume asc = 0 for all the alternatives different from the SQ 
                                b_clar  * Data.ite$clar5 
                              + b_fish  * Data.ite$fish5
                              + b_bio   * Data.ite$bio5
                              + b_coast * Data.ite$coast5
                              + b_lit   * Data.ite$lit5
                              + b_cost  * Data.ite$cost5
                              +              Data.ite$error5
       )
       
       Data.ite$utility6 <- (                                       # Creating utility 4 
                                # We assume asc = 0 for all the alternatives different from the SQ 
                                b_clar  * Data.ite$clar6 
                              + b_fish  * Data.ite$fish6
                              + b_bio   * Data.ite$bio6
                              + b_coast * Data.ite$coast6
                              + b_lit   * Data.ite$lit6
                              + b_cost  * Data.ite$cost6
                              +              Data.ite$error6
       )
       
       
       
       # we have to change this depending on the number of alternatives (TREATMENT), 
       # Generate choice
       
       Data.ite$choice   <- apply(cbind(Data.ite$utility1, Data.ite$utility2, Data.ite$utility3, Data.ite$utility4, Data.ite$utility5, Data.ite$utility6),1,which.max)    # Generating choice
       
       
       Data.ite$ch.clar <- ifelse(Data.ite$choice == 1, Data.ite$clar1,
                                  ifelse(Data.ite$choice == 2, Data.ite$clar2,
                                         ifelse(Data.ite$choice == 3, Data.ite$clar3,
                                                ifelse(Data.ite$choice == 4, Data.ite$clar4,
                                                       ifelse(Data.ite$choice == 5, Data.ite$clar5,
                                                              ifelse(Data.ite$choice == 6, Data.ite$clar6, NA))))))
       
       Data.ite$ch.fish <- ifelse(Data.ite$choice == 1, Data.ite$fish1,
                                  ifelse(Data.ite$choice == 2, Data.ite$fish2,
                                         ifelse(Data.ite$choice == 3, Data.ite$fish3,
                                                ifelse(Data.ite$choice == 4, Data.ite$fish4,
                                                       ifelse(Data.ite$choice == 5, Data.ite$fish5,
                                                              ifelse(Data.ite$choice == 6, Data.ite$fish6, NA))))))
       Data.ite$ch.bio  <- ifelse(Data.ite$choice == 1, Data.ite$bio1,
                                  ifelse(Data.ite$choice == 2, Data.ite$bio2,
                                         ifelse(Data.ite$choice == 3, Data.ite$bio3,
                                                ifelse(Data.ite$choice == 4, Data.ite$bio4,
                                                       ifelse(Data.ite$choice == 5, Data.ite$bio5,
                                                              ifelse(Data.ite$choice == 6, Data.ite$bio6, NA))))))
       Data.ite$ch.coast<- ifelse(Data.ite$choice == 1, Data.ite$coast1,
                                  ifelse(Data.ite$choice == 2, Data.ite$coast2,
                                         ifelse(Data.ite$choice == 3, Data.ite$coast3,
                                                ifelse(Data.ite$choice == 4, Data.ite$coast4,
                                                       ifelse(Data.ite$choice == 5, Data.ite$coast5,
                                                              ifelse(Data.ite$choice == 6, Data.ite$coast6, NA))))))
       Data.ite$ch.lit  <- ifelse(Data.ite$choice == 1, Data.ite$lit1,
                                  ifelse(Data.ite$choice == 2, Data.ite$lit2,
                                         ifelse(Data.ite$choice == 3, Data.ite$lit3,
                                                ifelse(Data.ite$choice == 4, Data.ite$lit4,
                                                       ifelse(Data.ite$choice == 5, Data.ite$lit5,
                                                              ifelse(Data.ite$choice == 6, Data.ite$lit6, NA))))))
       Data.ite$ch.cost <- ifelse(Data.ite$choice == 1, Data.ite$cost1,
                                  ifelse(Data.ite$choice == 2, Data.ite$cost2,
                                         ifelse(Data.ite$choice == 3, Data.ite$cost3,
                                                ifelse(Data.ite$choice == 4, Data.ite$cost4,
                                                       ifelse(Data.ite$choice == 5, Data.ite$cost5,
                                                              ifelse(Data.ite$choice == 6, Data.ite$cost6, NA))))))
       
       # Apply the function "min_no_cero" to identify the minimum value of cost different to cero in the alternatives, and the compare if the cost value of the chosen alternative.
       # If the individuals chooses the alternative with the lowest cost value different to cero, "Yes_min_cost_NoCero" takes value 1 and 0 otherwise
       
              # Step 1: Vector with the names of the columns of "cost"
              cost_cols <- paste0("cost", 1:n.alternatives)
              
              # Step 2: Create "Yes_min_cost_NoCero" row by row 
              Data.ite$Yes_min_cost_NoCero <- sapply(1:nrow(Data.ite), function(i) {
                row_vals <- as.numeric(Data.ite[i, cost_cols])
                row_cost <- Data.ite$ch.cost[i]
                min_cost <- min_no_cero(row_vals)
                return(as.integer(row_cost == min_cost))
              })
       
       # Apply the function "max_no_cero" to identify the maximum value of cost different to cero in the alternatives, and the compare if the cost value of the chosen alternative.
       # If the individuals chooses the alternative with the highest cost value different to cero, "Yes_max_cost_NoCero" takes value 1 nad 0 otherwise
              
              # Step 2: Create "Yes_max_cost_NoCero" row by row 
              Data.ite$Yes_max_cost_NoCero <- sapply(1:nrow(Data.ite), function(i) {
                row_vals <- as.numeric(Data.ite[i, cost_cols])
                row_cost <- Data.ite$ch.cost[i]
                max_cost <- max_no_cero(row_vals)
                return(as.integer(row_cost == max_cost))
              })
       
       # Save the results of the iteration
       
       clar.chosen[ite,"Turbid_SQ"]     <- sum(Data.ite$ch.clar== 0)
       clar.chosen[ite,"Rather turbid"] <- sum(Data.ite$ch.clar== 1)
       clar.chosen[ite,"Rather clear"]  <- sum(Data.ite$ch.clar== 2)
       clar.chosen[ite,"Clear"]         <- sum(Data.ite$ch.clar== 3)
       
       fish.chosen[ite,"Overfished_SQ"] <- sum(Data.ite$ch.fish== 0)
       fish.chosen[ite,"Stable"]        <- sum(Data.ite$ch.fish== 1)
       
       
       bio.chosen[ite,"Bad_SQ"]       <- sum(Data.ite$ch.bio== 0)
       bio.chosen[ite,"Rather bad"]   <- sum(Data.ite$ch.bio== 1)
       bio.chosen[ite,"Rather good"]  <- sum(Data.ite$ch.bio== 2)
       bio.chosen[ite,"Good"]         <- sum(Data.ite$ch.bio== 3)
       
       
       coast.chosen[ite,"Strong impact_SQ"]         <- sum(Data.ite$ch.coast== 0)
       coast.chosen[ite,"Mainly low impact"]        <- sum(Data.ite$ch.coast== 1)
       
       lit.chosen[ite,"Very much_SQ"]    <- sum(Data.ite$ch.lit== 0)
       lit.chosen[ite,"Much"]            <- sum(Data.ite$ch.lit== 1)
       lit.chosen[ite,"Little"]          <- sum(Data.ite$ch.lit== 2)
       lit.chosen[ite,"Very Little"]     <- sum(Data.ite$ch.lit== 3)
       
       cost.chosen[ite,"0_SQ" ]    <- sum(Data.ite$ch.cost== 0)
       cost.chosen[ite,"10"   ]    <- sum(Data.ite$ch.cost== 10)
       cost.chosen[ite,"25"   ]    <- sum(Data.ite$ch.cost== 25)
       cost.chosen[ite,"50"   ]    <- sum(Data.ite$ch.cost== 50)
       cost.chosen[ite,"80"   ]    <- sum(Data.ite$ch.cost== 80)
       cost.chosen[ite,"110"  ]    <- sum(Data.ite$ch.cost== 110)
       cost.chosen[ite,"160"  ]    <- sum(Data.ite$ch.cost== 160)
       cost.chosen[ite,"220"  ]    <- sum(Data.ite$ch.cost== 220)
       cost.chosen[ite,"300"  ]    <- sum(Data.ite$ch.cost== 300)
       
       lowest.cost.no0[ite,]    <- sum(Data.ite$Yes_min_cost_NoCero== 1)
       highest.cost.no0[ite,]    <- sum(Data.ite$Yes_max_cost_NoCero== 1)

       
# CLOSE LOOP
}



#####
## Observed values
#####

# List of the prefixes of the variables that we want to analyze
var_prefixes <- c("clar", "fish", "bio", "coast", "lit", "cost")

# Create the new variables
for (prefix in var_prefixes) {
  Data[[prefix]] <- NA  # create Data$clar, Data$fish, Data$bio ...
}

# General loop
for (i in 1:n.alternatives) {
  for (prefix in var_prefixes) {
    var_name <- paste0(prefix, i)
    if (var_name %in% names(Data)) {
      Data[[prefix]][Data$choibio == i] <- Data[[var_name]][Data$choibio == i]
    }
  }
}


# Identify how many times the individuals choose the alternative with the lowest cost value different to cero in the OBSERVED data 
Data$Yes_min_cost_NoCero <- sapply(1:nrow(Data), function(i) {
  row_costs <- as.numeric(Data[i, cost_cols])
  row_cost <- Data$cost[i]
  nonzero_costs <- row_costs[row_costs != 0]
  
  if (length(nonzero_costs) == 0) return(0)  # Avoid the error if all are 0
  
  min_val <- min(nonzero_costs, na.rm = TRUE)
  return(as.integer(row_cost == min_val))
})

# Identify how many times the individuals choose the alternative with the highest cost value different to cero in the OBSERVED data 
Data$Yes_max_cost_NoCero <- sapply(1:nrow(Data), function(i) {
  row_costs <- as.numeric(Data[i, cost_cols])
  row_cost <- Data$cost[i]
  nonzero_costs <- row_costs[row_costs != 0]
  
  if (length(nonzero_costs) == 0) return(0)  # Avoid the error if all are 0
  
  max_val <- max(nonzero_costs, na.rm = TRUE)
  return(as.integer(row_cost == max_val))
})


clar.obs <- matrix(0,1,4)
colnames(clar.obs) <- c("Turbid_SQ", "Rather turbid", "Rather clear", "Clear")
fish.obs <- matrix(0,1,2)
colnames(fish.obs) <- c("Overfished_SQ", "Stable")
bio.obs <- matrix(0,1,4)
colnames(bio.obs) <- c("Bad_SQ", "Rather bad", "Rather good", "Good")
coast.obs <- matrix(0,1,2)
colnames(coast.obs) <- c("Strong impact_SQ", "Mainly low impact")
lit.obs <- matrix(0,1,4)
colnames(lit.obs) <- c("Very much_SQ", "Much", "Little", "Very Little")
cost.obs <- matrix(0,1,9)
colnames(cost.obs) <- c("0_SQ", "10", "25", "50", "80", "110", "160", "220", "300")

lowest.cost.no0.obs  <- matrix(0,1,1)
highest.cost.no0.obs <- matrix(0,1,1)


clar.obs[,"Turbid_SQ"]     <- sum(Data$clar== 0)
clar.obs[,"Rather turbid"] <- sum(Data$clar== 1)
clar.obs[,"Rather clear"]  <- sum(Data$clar== 2)
clar.obs[,"Clear"]         <- sum(Data$clar== 3)

fish.obs[,"Overfished_SQ"] <- sum(Data$fish== 0)
fish.obs[,"Stable"]        <- sum(Data$fish== 1)

bio.obs[,"Bad_SQ"]       <- sum(Data$bio== 0)
bio.obs[,"Rather bad"]   <- sum(Data$bio== 1)
bio.obs[,"Rather good"]  <- sum(Data$bio== 2)
bio.obs[,"Good"]         <- sum(Data$bio== 3)

coast.obs[,"Strong impact_SQ"]         <- sum(Data$coast== 0)
coast.obs[,"Mainly low impact"]        <- sum(Data$coast== 1)

lit.obs[,"Very much_SQ"]    <- sum(Data$lit== 0)
lit.obs[,"Much"]            <- sum(Data$lit== 1)
lit.obs[,"Little"]          <- sum(Data$lit== 2)
lit.obs[,"Very Little"]     <- sum(Data$lit== 3)

cost.obs[,"0_SQ" ]    <- sum(Data$cost== 0)
cost.obs[,"10"   ]    <- sum(Data$cost== 10)
cost.obs[,"25"   ]    <- sum(Data$cost== 25)
cost.obs[,"50"   ]    <- sum(Data$cost== 50)
cost.obs[,"80"   ]    <- sum(Data$cost== 80)
cost.obs[,"110"  ]    <- sum(Data$cost== 110)
cost.obs[,"160"  ]    <- sum(Data$cost== 160)
cost.obs[,"220"  ]    <- sum(Data$cost== 220)
cost.obs[,"300"  ]    <- sum(Data$cost== 300) 

lowest.cost.no0.obs  <- sum(Data$Yes_min_cost_NoCero== 1)
highest.cost.no0.obs <- sum(Data$Yes_max_cost_NoCero== 1)


################
##  COMPARE OBSERVED AND SIMULATED
##  In each matrix Results.clar, Results.fish ... we have the following informations:
##  - In each row, we have information about each alternative of the attribute
##  - By column:
##       - Column "observed": number of individuals that choose this alternative in the original data
##       - Column "simulated median": the median of the simulated distribution of the number of individuals for this alternative
##       - Column "0.5% percentile": the 0.5% percentile of the simulated distribution of the number of individuals for this alternative
##       - Column "99.5% percentile": the 99.5% percentile of the simulated distribution of the number of individuals for this alternative
##       - Column "reject H0": The conclusion whether H0 is rejected or not
##            # H0: the observed quantatity is obtained from the simulated distribution
################

# H0: the observed quntatity is obtained from the simulated distribution

#### CLARITY
columns                     <- c("observed", "simulated median","0.5% percentile", "99.5% percentile", "reject H0")
Results.clar                <- matrix(NA, 4, length(columns))
colnames(Results.clar)      <- columns
row.names(Results.clar)     <- colnames(clar.obs)
for (i in colnames(clar.chosen)) {
  Results.clar[ i,"observed"]          <- clar.obs[,i]
  Results.clar[ i,"simulated median"]  <- median(clar.chosen[,i])
  Results.clar[ i,"0.5% percentile"]   <- quantile(clar.chosen[,i], 0.005)
  Results.clar[ i,"99.5% percentile"]  <- quantile(clar.chosen[,i], 0.995)
  Results.clar[ i,"reject H0"]         <- ifelse(Results.clar[ i,"observed"]>= Results.clar[ i,"0.5% percentile"] & Results.clar[ i,"observed"]<= Results.clar[ i,"99.5% percentile"] ,
                                                 0, 
                                                 1)
}

#### FISH
Results.fish                <- matrix(NA, 2, length(columns))
colnames(Results.fish)      <- columns
row.names(Results.fish)     <- colnames(fish.obs)
for (i in colnames(fish.chosen)) {
  Results.fish[ i,"observed"]          <- fish.obs[,i]
  Results.fish[ i,"simulated median"]  <- median(fish.chosen[,i])
  Results.fish[ i,"0.5% percentile"]   <- quantile(fish.chosen[,i], 0.005)
  Results.fish[ i,"99.5% percentile"]  <- quantile(fish.chosen[,i], 0.995)
  Results.fish[ i,"reject H0"]         <- ifelse(Results.fish[ i,"observed"]>= Results.fish[ i,"0.5% percentile"] & Results.fish[ i,"observed"]<= Results.fish[ i,"99.5% percentile"] ,
                                                 0, 
                                                 1)
}

#### BIO
Results.bio                <- matrix(NA, 4, length(columns))
colnames(Results.bio)      <- columns
row.names(Results.bio)     <- colnames(bio.obs)
for (i in colnames(bio.chosen)) {
  Results.bio[ i,"observed"]          <- bio.obs[,i]
  Results.bio[ i,"simulated median"]  <- median(bio.chosen[,i])
  Results.bio[ i,"0.5% percentile"]   <- quantile(bio.chosen[,i], 0.005)
  Results.bio[ i,"99.5% percentile"]  <- quantile(bio.chosen[,i], 0.995)
  Results.bio[ i,"reject H0"]         <- ifelse(Results.bio[ i,"observed"]>= Results.bio[ i,"0.5% percentile"] & Results.bio[ i,"observed"]<= Results.bio[ i,"99.5% percentile"] ,
                                                0, 
                                                1)
}


#### COAST
Results.coast                <- matrix(NA, 2, length(columns))
colnames(Results.coast)      <- columns
row.names(Results.coast)     <- colnames(coast.obs)
for (i in colnames(coast.chosen)) {
  Results.coast[ i,"observed"]          <- coast.obs[,i]
  Results.coast[ i,"simulated median"]  <- median(coast.chosen[,i])
  Results.coast[ i,"0.5% percentile"]   <- quantile(coast.chosen[,i], 0.005)
  Results.coast[ i,"99.5% percentile"]  <- quantile(coast.chosen[,i], 0.995)
  Results.coast[ i,"reject H0"]         <- ifelse(Results.coast[ i,"observed"]>= Results.coast[ i,"0.5% percentile"] & Results.coast[ i,"observed"]<= Results.coast[ i,"99.5% percentile"] ,
                                                  0, 
                                                  1)
}

#### LIT
Results.lit                <- matrix(NA, 4, length(columns))
colnames(Results.lit)      <- columns
row.names(Results.lit)     <- colnames(lit.obs)
for (i in colnames(lit.chosen)) {
  Results.lit[ i,"observed"]          <- lit.obs[,i]
  Results.lit[ i,"simulated median"]  <- median(lit.chosen[,i])
  Results.lit[ i,"0.5% percentile"]   <- quantile(lit.chosen[,i], 0.005)
  Results.lit[ i,"99.5% percentile"]  <- quantile(lit.chosen[,i], 0.995)
  Results.lit[ i,"reject H0"]         <- ifelse(Results.lit[ i,"observed"]>= Results.lit[ i,"0.5% percentile"] & Results.lit[ i,"observed"]<= Results.lit[ i,"99.5% percentile"] ,
                                                0, 
                                                1)
}

#### COST
Results.cost                <- matrix(NA, 9, length(columns))
colnames(Results.cost)      <- columns
row.names(Results.cost)     <- colnames(cost.obs)
for (i in colnames(cost.chosen)) {
  Results.cost[ i,"observed"]          <- cost.obs[,i]
  Results.cost[ i,"simulated median"]  <- median(cost.chosen[,i])
  Results.cost[ i,"0.5% percentile"]   <- quantile(cost.chosen[,i], 0.005)
  Results.cost[ i,"99.5% percentile"]  <- quantile(cost.chosen[,i], 0.995)
  Results.cost[ i,"reject H0"]         <- ifelse(Results.cost[ i,"observed"]>= Results.cost[ i,"0.5% percentile"] & Results.cost[ i,"observed"]<= Results.cost[ i,"99.5% percentile"] ,
                                                 0, 
                                                 1)
}

max(cost.chosen[,"220"]) 
min(cost.chosen[,"110"]) 



####  MINIMUM COST DIFFERENT TO CERO 
Results.cost.MIN.NOcero <- matrix(NA, 1, length(columns))
colnames(Results.cost.MIN.NOcero)      <- columns
rownames(Results.cost.MIN.NOcero)      <- c("MINIMUM COST DIFFERENT TO CERO")
Results.cost.MIN.NOcero[ ,"observed"]            <- lowest.cost.no0.obs
Results.cost.MIN.NOcero[ ,"simulated median"]    <- median(lowest.cost.no0)
Results.cost.MIN.NOcero[ ,"0.5% percentile"]     <- quantile(lowest.cost.no0, 0.005)
Results.cost.MIN.NOcero[ ,"99.5% percentile"]    <- quantile(lowest.cost.no0, 0.995)
Results.cost.MIN.NOcero[ ,"reject H0"]           <- ifelse(Results.cost.MIN.NOcero[ ,"observed"]>= Results.cost.MIN.NOcero[ ,"0.5% percentile"] & Results.cost.MIN.NOcero[ ,"observed"]<= Results.cost.MIN.NOcero[ ,"99.5% percentile"] ,
                                                           0, 
                                                           1)

#### MAXIMUM COST DIFFERENT TO CERO 
Results.cost.MAX.NOcero <- matrix(NA, 1, length(columns))
colnames(Results.cost.MAX.NOcero)      <- columns
rownames(Results.cost.MAX.NOcero)      <- c("MAXIMUM COST DIFFERENT TO CERO")
Results.cost.MAX.NOcero[ ,"observed"]            <- highest.cost.no0.obs
Results.cost.MAX.NOcero[ ,"simulated median"]    <- median(highest.cost.no0)
Results.cost.MAX.NOcero[ ,"0.5% percentile"]     <- quantile(highest.cost.no0, 0.005)
Results.cost.MAX.NOcero[ ,"99.5% percentile"]    <- quantile(highest.cost.no0, 0.995)
Results.cost.MAX.NOcero[ ,"reject H0"]           <- ifelse(Results.cost.MAX.NOcero[ ,"observed"]>= Results.cost.MAX.NOcero[ ,"0.5% percentile"] & Results.cost.MAX.NOcero[ ,"observed"]<= Results.cost.MAX.NOcero[ ,"99.5% percentile"] ,
                                                           0, 
                                                           1)



#######################
##  SAVE DATA
#######################

Results <- rbind(Results.clar,
                 Results.fish ,
                 Results.bio,
                 Results.coast,
                 Results.lit ,
                 Results.cost ,
                 Results.cost.MIN.NOcero,
                 Results.cost.MAX.NOcero )


write.table(Results, file="./Results_MNL_IC99_6Alt.csv", sep=" ", row.names = TRUE, col.names=TRUE)
