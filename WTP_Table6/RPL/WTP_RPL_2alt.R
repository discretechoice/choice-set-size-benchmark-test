####################################################################
####################################################################
#####
#####            WTP calculations
#####            RPL - 2 alternatives - treatment 1
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


# ################################################################# #
# Setting parameters
# ################################################################# #
       
b_mu_asc1        <-   0.496908
b_mu_clar        <-   0.731339
b_mu_fish        <-   1.917042
b_mu_bio         <-   0.566663
b_mu_coast       <-   0.089702
b_mu_lit         <-   0.862617
b_mu_cost        <-  -4.078784
b_sd_asc1        <-   0.378657
b_sd_clar        <-   0.200838
b_sd_fish        <-   0.544650
b_sd_bio         <-  -0.187499
b_sd_coast       <-  -0.218607
b_sd_lit         <-   0.157570
b_sd_cost        <-  -0.080612
b_sd_asc1_clar   <-  -0.395388
b_sd_asc1_fish   <-  -1.063860
b_sd_asc1_bio    <-  -0.219660
b_sd_asc1_coast  <-  -0.273579
b_sd_asc1_lit    <-  -0.364113
b_sd_asc1_cost   <-  -1.068193
b_sd_clar_fish   <-  -0.585732
b_sd_clar_bio    <-  -0.050855
b_sd_clar_coast  <-  -0.989448
b_sd_clar_lit    <-   0.015681
b_sd_clar_cost   <-   1.000708
b_sd_fish_bio    <-   0.001069
b_sd_fish_coast  <-  -0.067844
b_sd_fish_lit    <-  -0.175618
b_sd_fish_cost   <-   0.266657
b_sd_bio_coast   <-  -0.101254
b_sd_bio_lit     <-   0.382575
b_sd_bio_cost    <-  -0.496174
b_sd_coast_lit   <-  -0.022803
b_sd_coast_cost  <-  -0.727730
b_sd_lit_cost    <-   0.752713 


se_mu_asc1        <- 0.22116
se_mu_clar        <- 0.10361
se_mu_fish        <- 0.26267
se_mu_bio         <- 0.09437
se_mu_coast       <- 0.19610
se_mu_lit         <- 0.11373
se_mu_cost        <- 0.13367
se_sd_asc1        <- 0.51537
se_sd_clar        <- 0.15102
se_sd_fish        <- 0.36786
se_sd_bio         <- 0.14345
se_sd_coast       <- 0.31749
se_sd_lit         <- 0.19287
se_sd_cost        <- 0.02927
se_sd_asc1_clar   <- 0.15074
se_sd_asc1_fish   <- 0.32184
se_sd_asc1_bio    <- 0.12214
se_sd_asc1_coast  <- 0.26064
se_sd_asc1_lit    <- 0.15541
se_sd_asc1_cost   <- 0.08034
se_sd_clar_fish   <- 0.51939
se_sd_clar_bio    <- 0.15510
se_sd_clar_coast  <- 0.25065
se_sd_clar_lit    <- 0.17629
se_sd_clar_cost   <- 0.17357
se_sd_fish_bio    <- 0.16764
se_sd_fish_coast  <- 0.32143
se_sd_fish_lit    <- 0.29335
se_sd_fish_cost   <- 0.05867
se_sd_bio_coast   <- 0.25118
se_sd_bio_lit     <- 0.16124
se_sd_bio_cost    <- 0.05282
se_sd_coast_lit   <- 0.11303
se_sd_coast_cost  <- 0.05750
se_sd_lit_cost    <- 0.09590

n.ite <- 1000

distr_mu_asc1       <- rnorm(n.ite, b_mu_asc1       , se_mu_asc1       )     
distr_mu_clar       <- rnorm(n.ite, b_mu_clar       , se_mu_clar       )       
distr_mu_fish       <- rnorm(n.ite, b_mu_fish       , se_mu_fish       )       
distr_mu_bio        <- rnorm(n.ite, b_mu_bio        , se_mu_bio        )             
distr_mu_coast      <- rnorm(n.ite, b_mu_coast      , se_mu_coast      )      
distr_mu_lit        <- rnorm(n.ite, b_mu_lit        , se_mu_lit        )     
distr_mu_cost       <- rnorm(n.ite, b_mu_cost       , se_mu_cost       )  
distr_sd_asc1       <- rnorm(n.ite, b_sd_asc1       , se_sd_asc1       )  
distr_sd_clar       <- rnorm(n.ite, b_sd_clar       , se_sd_clar       )   
distr_sd_fish       <- rnorm(n.ite, b_sd_fish       , se_sd_fish       )
distr_sd_bio        <- rnorm(n.ite, b_sd_bio        , se_sd_bio        )      
distr_sd_coast      <- rnorm(n.ite, b_sd_coast      , se_sd_coast      )     
distr_sd_lit        <- rnorm(n.ite, b_sd_lit        , se_sd_lit        )    
distr_sd_cost       <- rnorm(n.ite, b_sd_cost       , se_sd_cost       )   
distr_sd_asc1_clar  <- rnorm(n.ite, b_sd_asc1_clar  , se_sd_asc1_clar  )  
distr_sd_asc1_fish  <- rnorm(n.ite, b_sd_asc1_fish  , se_sd_asc1_fish  )
distr_sd_asc1_bio   <- rnorm(n.ite, b_sd_asc1_bio   , se_sd_asc1_bio   )
distr_sd_asc1_coast <- rnorm(n.ite, b_sd_asc1_coast , se_sd_asc1_coast )
distr_sd_asc1_lit   <- rnorm(n.ite, b_sd_asc1_lit   , se_sd_asc1_lit   )
distr_sd_asc1_cost  <- rnorm(n.ite, b_sd_asc1_cost  , se_sd_asc1_cost  )
distr_sd_clar_fish  <- rnorm(n.ite, b_sd_clar_fish  , se_sd_clar_fish  )
distr_sd_clar_bio   <- rnorm(n.ite, b_sd_clar_bio   , se_sd_clar_bio   )
distr_sd_clar_coast <- rnorm(n.ite, b_sd_clar_coast , se_sd_clar_coast )
distr_sd_clar_lit   <- rnorm(n.ite, b_sd_clar_lit   , se_sd_clar_lit   )
distr_sd_clar_cost  <- rnorm(n.ite, b_sd_clar_cost  , se_sd_clar_cost  )
distr_sd_fish_bio   <- rnorm(n.ite, b_sd_fish_bio   , se_sd_fish_bio   )
distr_sd_fish_coast <- rnorm(n.ite, b_sd_fish_coast , se_sd_fish_coast )
distr_sd_fish_lit   <- rnorm(n.ite, b_sd_fish_lit   , se_sd_fish_lit   )
distr_sd_fish_cost  <- rnorm(n.ite, b_sd_fish_cost  , se_sd_fish_cost  )
distr_sd_bio_coast  <- rnorm(n.ite, b_sd_bio_coast  , se_sd_bio_coast  )
distr_sd_bio_lit    <- rnorm(n.ite, b_sd_bio_lit    , se_sd_bio_lit    )
distr_sd_bio_cost   <- rnorm(n.ite, b_sd_bio_cost   , se_sd_bio_cost   )
distr_sd_coast_lit  <- rnorm(n.ite, b_sd_coast_lit  , se_sd_coast_lit  )
distr_sd_coast_cost <- rnorm(n.ite, b_sd_coast_cost , se_sd_coast_cost )
distr_sd_lit_cost   <- rnorm(n.ite, b_sd_lit_cost   , se_sd_lit_cost   )

##
#
#  THE MATRIX TO SAVE THE RESULTS OF THE SIMULATIONS
#
##

simu_distr_median_WTP <-matrix (NA, n.ite, 5)
colnames(simu_distr_median_WTP) <- c("mu_clar","mu_fish","mu_bio","mu_coast","mu_lit")


####
##
#  OPEN LOOP
##
####

for (i in 1:n.ite){
  
  mu_asc1        <- distr_mu_asc1[i]      
  mu_clar        <- distr_mu_clar[i]      
  mu_fish        <- distr_mu_fish[i]      
  mu_bio         <- distr_mu_bio[i]       
  mu_coast       <- distr_mu_coast[i]     
  mu_lit         <- distr_mu_lit[i]       
  mu_cost        <- distr_mu_cost[i]      
  sd_asc1        <- distr_sd_asc1[i]      
  sd_clar        <- distr_sd_clar[i]      
  sd_fish        <- distr_sd_fish[i]      
  sd_bio         <- distr_sd_bio[i]       
  sd_coast       <- distr_sd_coast[i]     
  sd_lit         <- distr_sd_lit[i]       
  sd_cost        <- distr_sd_cost[i]      
  sd_asc1_clar   <- distr_sd_asc1_clar[i] 
  sd_asc1_fish   <- distr_sd_asc1_fish[i] 
  sd_asc1_bio    <- distr_sd_asc1_bio[i]  
  sd_asc1_coast  <- distr_sd_asc1_coast[i]
  sd_asc1_lit    <- distr_sd_asc1_lit[i]  
  sd_asc1_cost   <- distr_sd_asc1_cost[i] 
  sd_clar_fish   <- distr_sd_clar_fish[i] 
  sd_clar_bio    <- distr_sd_clar_bio[i]  
  sd_clar_coast  <- distr_sd_clar_coast[i]
  sd_clar_lit    <- distr_sd_clar_lit[i]  
  sd_clar_cost   <- distr_sd_clar_cost[i] 
  sd_fish_bio    <- distr_sd_fish_bio[i]  
  sd_fish_coast  <- distr_sd_fish_coast[i]
  sd_fish_lit    <- distr_sd_fish_lit[i]  
  sd_fish_cost   <- distr_sd_fish_cost[i] 
  sd_bio_coast   <- distr_sd_bio_coast[i] 
  sd_bio_lit     <- distr_sd_bio_lit[i]   
  sd_bio_cost    <- distr_sd_bio_cost[i]  
  sd_coast_lit   <- distr_sd_coast_lit[i] 
  sd_coast_cost  <- distr_sd_coast_cost[i]
  sd_lit_cost    <- distr_sd_lit_cost[i]  
  
           
           
           choleski.cov <- t(matrix (c(sd_asc1      , 0                            , 0           , 0            ,0            , 0                              , 0                        ,
                                       sd_asc1_clar , sd_clar      , 0             , 0           , 0            ,0            , 0                        ,
                                       sd_asc1_fish , sd_clar_fish , sd_fish       , 0           , 0            ,0            , 0                        ,
                                       sd_asc1_bio  , sd_clar_bio  , sd_fish_bio   , sd_bio      , 0            ,0            , 0                        ,     
                                       sd_asc1_coast, sd_clar_coast, sd_fish_coast , sd_bio_coast, sd_coast     ,0            , 0                        ,
                                       sd_asc1_lit  , sd_clar_lit  , sd_fish_lit   , sd_bio_lit  , sd_coast_lit , sd_lit      , 0                        ,      
                                       sd_asc1_cost , sd_clar_cost , sd_fish_cost  , sd_bio_cost , sd_coast_cost, sd_lit_cost , sd_cost), 7))
           
           var.cov           <- choleski.cov %*% t(choleski.cov)  # variance-covariance matriz of the random parameters
           
           NumIter    <-10000
           
           # ################################################################# #
           #  Simulate parameter values and WTP
           # ################################################################# #
           
           normal.0.1     <- cbind(rnorm(NumIter),
                                   rnorm(NumIter),
                                   rnorm(NumIter),
                                   rnorm(NumIter),
                                   rnorm(NumIter),
                                   rnorm(NumIter),
                                   rnorm(NumIter))
           
           beta.attr      <- cbind(rep(unname(mu_asc1)  ,NumIter),
                                   rep(unname(mu_clar)  ,NumIter),
                                   rep(unname(mu_fish)  ,NumIter),
                                   rep(unname(mu_bio)   ,NumIter),
                                   rep(unname(mu_coast) ,NumIter),
                                   rep(unname(mu_lit)   ,NumIter),
                                   rep(unname(mu_cost)  ,NumIter) )
           
           random.attr    <- normal.0.1 %*% t(choleski.cov) + beta.attr
           colnames(random.attr) <- c("mu_asc1","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
           
           
           RPL.WTP.clar  <-   (- ( random.attr[,"mu_clar"]  / (-exp(random.attr[,"mu_cost"])) ))
           RPL.WTP.fish  <-   (- ( random.attr[,"mu_fish"]  / (-exp(random.attr[,"mu_cost"])) ))
           RPL.WTP.bio   <-   (- ( random.attr[,"mu_bio"]   / (-exp(random.attr[,"mu_cost"])) ))
           RPL.WTP.coast <-   (- ( random.attr[,"mu_coast"] / (-exp(random.attr[,"mu_cost"])) ))
           RPL.WTP.lit   <-   (- ( random.attr[,"mu_lit"]   / (-exp(random.attr[,"mu_cost"])) ))
           
           simu_distr_median_WTP[i, "mu_clar"]  <- median(RPL.WTP.clar )
           simu_distr_median_WTP[i, "mu_fish"]  <- median(RPL.WTP.fish )
           simu_distr_median_WTP[i, "mu_bio"]   <- median(RPL.WTP.bio  )
           simu_distr_median_WTP[i, "mu_coast"] <- median(RPL.WTP.coast)
           simu_distr_median_WTP[i, "mu_lit"]   <- median(RPL.WTP.lit  )
           
          
           
           ####
           ##
           #  CLOSE THE LOOP
           ##
           ####
           
}           
    
       
# ################################################################# #
#  Save results
# ################################################################# #

median_CI_WTP <- rbind(c(median(simu_distr_median_WTP[, "mu_clar"] ), quantile(simu_distr_median_WTP[, "mu_clar"] , 0.025), quantile(simu_distr_median_WTP[, "mu_clar"] , 0.975)),
                       c(median(simu_distr_median_WTP[, "mu_fish"] ), quantile(simu_distr_median_WTP[, "mu_fish"] , 0.025), quantile(simu_distr_median_WTP[, "mu_fish"] , 0.975)),
                       c(median(simu_distr_median_WTP[, "mu_bio"]  ), quantile(simu_distr_median_WTP[, "mu_bio"]  , 0.025), quantile(simu_distr_median_WTP[, "mu_bio"]  , 0.975)),
                       c(median(simu_distr_median_WTP[, "mu_coast"]), quantile(simu_distr_median_WTP[, "mu_coast"], 0.025), quantile(simu_distr_median_WTP[, "mu_coast"], 0.975)),
                       c(median(simu_distr_median_WTP[, "mu_lit"]  ), quantile(simu_distr_median_WTP[, "mu_lit"]  , 0.025), quantile(simu_distr_median_WTP[, "mu_lit"]  , 0.975)))

row.names(median_CI_WTP) <- c("mu_clar","mu_fish","mu_bio","mu_coast","mu_lit")
colnames(median_CI_WTP)  <- c("median", "CI_0.025", "CI_0.975")
median_CI_WTP         


write.table(simu_distr_median_WTP, file="./simu_WTP_2Alt_2026_02_26.csv", sep=" ", row.names = TRUE, col.names=TRUE)
write.table(median_CI_WTP, file="./median_CI_WTP_2Alt_2026_02_26.csv", sep=" ", row.names = TRUE, col.names=TRUE)


