####################################################################
####################################################################
#####
#####            WTP calculations
#####            RPL - 3 alternatives - treatment 2
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
       
b_mu_asc1         <- -1.16029
b_mu_asc2         <- 0.26791
b_mu_clar         <- 0.47020
b_mu_fish         <- 0.79074
b_mu_bio          <- 0.67645
b_mu_coast        <- 0.08804
b_mu_lit          <- 0.65300
b_mu_cost         <- -5.10991
b_sd_asc1         <- 4.29046
b_sd_asc2         <- -0.36019
b_sd_clar         <- 0.34541
b_sd_fish         <- 0.85204
b_sd_bio          <- -0.18404
b_sd_coast        <- -0.31004
b_sd_lit          <- -0.01446
b_sd_cost         <- 0.32212
b_sd_asc1_asc2    <- 0.18983
b_sd_asc1_clar    <- 0.35148
b_sd_asc1_fish    <- 0.37102
b_sd_asc1_bio     <- 0.43828
b_sd_asc1_coast   <- 0.52207
b_sd_asc1_lit     <- 0.38571
b_sd_asc1_cost    <- 0.84895
b_sd_asc2_clar    <- 0.22728
b_sd_asc2_fish    <- 0.24579
b_sd_asc2_bio     <- -0.11611
b_sd_asc2_coast   <- 0.05552
b_sd_asc2_lit     <- -0.23163
b_sd_asc2_cost    <- 0.91968
b_sd_clar_fish    <- 0.82131
b_sd_clar_bio     <- 0.47433
b_sd_clar_coast   <- 0.42486
b_sd_clar_lit     <- 0.48124
b_sd_clar_cost    <- 0.32540
b_sd_fish_bio     <- 0.27159
b_sd_fish_coast   <- 0.63245
b_sd_fish_lit     <- 0.15634
b_sd_fish_cost    <- 0.55838
b_sd_bio_coast    <- 0.32903
b_sd_bio_lit      <- 0.06101
b_sd_bio_cost     <- -1.16134
b_sd_coast_lit    <- -0.16137
b_sd_coast_cost   <- 0.06907
b_sd_lit_cost     <- 0.93875 

se_mu_asc1         <- 0.34606
se_mu_asc2         <- 0.08074
se_mu_clar         <- 0.07193
se_mu_fish         <- 0.13361
se_mu_bio          <- 0.07837
se_mu_coast        <- 0.11141
se_mu_lit          <- 0.07320
se_mu_cost         <- 0.54826
se_sd_asc1         <- 0.54514
se_sd_asc2         <- 0.11873
se_sd_clar         <- 0.13409
se_sd_fish         <- 0.36358
se_sd_bio          <- 0.09321
se_sd_coast        <- 0.40767
se_sd_lit          <- 0.06317
se_sd_cost         <- 0.20113
se_sd_asc1_asc2    <- 0.11379
se_sd_asc1_clar    <- 0.13471
se_sd_asc1_fish    <- 0.34235
se_sd_asc1_bio     <- 0.18294
se_sd_asc1_coast   <- 0.25484
se_sd_asc1_lit     <- 0.20810
se_sd_asc1_cost    <- 0.61318
se_sd_asc2_clar    <- 0.10136
se_sd_asc2_fish    <- 0.25630
se_sd_asc2_bio     <- 0.11947
se_sd_asc2_coast   <- 0.24800
se_sd_asc2_lit     <- 0.13758
se_sd_asc2_cost    <- 0.37063
se_sd_clar_fish    <- 0.34626
se_sd_clar_bio     <- 0.11924
se_sd_clar_coast   <- 0.21173
se_sd_clar_lit     <- 0.09270
se_sd_clar_cost    <- 0.26304
se_sd_fish_bio     <- 0.25180
se_sd_fish_coast   <- 0.24407
se_sd_fish_lit     <- 0.13607
se_sd_fish_cost    <- 0.22176
se_sd_bio_coast    <- 0.15497
se_sd_bio_lit      <- 0.08123
se_sd_bio_cost     <- 0.23577
se_sd_coast_lit    <- 0.12825
se_sd_coast_cost   <- 0.10126
se_sd_lit_cost     <- 0.27594 


n.ite <- 1000

distr_mu_asc1       <- rnorm(n.ite, b_mu_asc1             , se_mu_asc1           )
distr_mu_asc2       <- rnorm(n.ite, b_mu_asc2             , se_mu_asc2           )
distr_mu_clar       <- rnorm(n.ite, b_mu_clar             , se_mu_clar           )
distr_mu_fish       <- rnorm(n.ite, b_mu_fish             , se_mu_fish           )
distr_mu_bio        <- rnorm(n.ite, b_mu_bio              , se_mu_bio            )
distr_mu_coast      <- rnorm(n.ite, b_mu_coast            , se_mu_coast          )
distr_mu_lit        <- rnorm(n.ite, b_mu_lit              , se_mu_lit            )
distr_mu_cost       <- rnorm(n.ite, b_mu_cost             , se_mu_cost           )
distr_sd_asc1       <- rnorm(n.ite, b_sd_asc1             , se_sd_asc1           )
distr_sd_asc2       <- rnorm(n.ite, b_sd_asc2             , se_sd_asc2           )
distr_sd_clar       <- rnorm(n.ite, b_sd_clar             , se_sd_clar           )
distr_sd_fish       <- rnorm(n.ite, b_sd_fish             , se_sd_fish           )
distr_sd_bio        <- rnorm(n.ite, b_sd_bio              , se_sd_bio            )
distr_sd_coast      <- rnorm(n.ite, b_sd_coast            , se_sd_coast          )
distr_sd_lit        <- rnorm(n.ite, b_sd_lit              , se_sd_lit            )
distr_sd_cost       <- rnorm(n.ite, b_sd_cost             , se_sd_cost           )
distr_sd_asc1_asc2  <- rnorm(n.ite, b_sd_asc1_asc2        , se_sd_asc1_asc2      )
distr_sd_asc1_clar  <- rnorm(n.ite, b_sd_asc1_clar        , se_sd_asc1_clar      )
distr_sd_asc1_fish  <- rnorm(n.ite, b_sd_asc1_fish        , se_sd_asc1_fish      )
distr_sd_asc1_bio   <- rnorm(n.ite, b_sd_asc1_bio         , se_sd_asc1_bio       )
distr_sd_asc1_coast <- rnorm(n.ite, b_sd_asc1_coast       , se_sd_asc1_coast     )
distr_sd_asc1_lit   <- rnorm(n.ite, b_sd_asc1_lit         , se_sd_asc1_lit       )
distr_sd_asc1_cost  <- rnorm(n.ite, b_sd_asc1_cost        , se_sd_asc1_cost      )
distr_sd_asc2_clar  <- rnorm(n.ite, b_sd_asc2_clar        , se_sd_asc2_clar      )
distr_sd_asc2_fish  <- rnorm(n.ite, b_sd_asc2_fish        , se_sd_asc2_fish      )
distr_sd_asc2_bio   <- rnorm(n.ite, b_sd_asc2_bio         , se_sd_asc2_bio       )
distr_sd_asc2_coast <- rnorm(n.ite, b_sd_asc2_coast       , se_sd_asc2_coast     )
distr_sd_asc2_lit   <- rnorm(n.ite, b_sd_asc2_lit         , se_sd_asc2_lit       )
distr_sd_asc2_cost  <- rnorm(n.ite, b_sd_asc2_cost        , se_sd_asc2_cost      )
distr_sd_clar_fish  <- rnorm(n.ite, b_sd_clar_fish        , se_sd_clar_fish      )
distr_sd_clar_bio   <- rnorm(n.ite, b_sd_clar_bio         , se_sd_clar_bio       )
distr_sd_clar_coast <- rnorm(n.ite, b_sd_clar_coast       , se_sd_clar_coast     )
distr_sd_clar_lit   <- rnorm(n.ite, b_sd_clar_lit         , se_sd_clar_lit       )
distr_sd_clar_cost  <- rnorm(n.ite, b_sd_clar_cost        , se_sd_clar_cost      )
distr_sd_fish_bio   <- rnorm(n.ite, b_sd_fish_bio         , se_sd_fish_bio       )
distr_sd_fish_coast <- rnorm(n.ite, b_sd_fish_coast       , se_sd_fish_coast     )
distr_sd_fish_lit   <- rnorm(n.ite, b_sd_fish_lit         , se_sd_fish_lit       )
distr_sd_fish_cost  <- rnorm(n.ite, b_sd_fish_cost        , se_sd_fish_cost      )
distr_sd_bio_coast  <- rnorm(n.ite, b_sd_bio_coast        , se_sd_bio_coast      )
distr_sd_bio_lit    <- rnorm(n.ite, b_sd_bio_lit          , se_sd_bio_lit        )
distr_sd_bio_cost   <- rnorm(n.ite, b_sd_bio_cost         , se_sd_bio_cost       )
distr_sd_coast_lit  <- rnorm(n.ite, b_sd_coast_lit        , se_sd_coast_lit      )
distr_sd_coast_cost <- rnorm(n.ite, b_sd_coast_cost       , se_sd_coast_cost     )
distr_sd_lit_cost   <- rnorm(n.ite, b_sd_lit_cost         , se_sd_lit_cost       )

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
  
  mu_asc1      <- distr_mu_asc1[i]
  mu_asc2      <- distr_mu_asc2[i]
  mu_clar      <- distr_mu_clar[i]
  mu_fish      <- distr_mu_fish[i]
  mu_bio       <- distr_mu_bio[i]
  mu_coast     <- distr_mu_coast[i]
  mu_lit       <- distr_mu_lit[i]
  mu_cost      <- distr_mu_cost[i]
  sd_asc1      <- distr_sd_asc1[i]
  sd_asc2      <- distr_sd_asc2[i]
  sd_clar      <- distr_sd_clar[i]
  sd_fish      <- distr_sd_fish[i]
  sd_bio       <- distr_sd_bio[i]
  sd_coast     <- distr_sd_coast[i]
  sd_lit       <- distr_sd_lit[i]
  sd_cost      <- distr_sd_cost[i]
  sd_asc1_asc2 <- distr_sd_asc1_asc2[i]
  sd_asc1_clar <- distr_sd_asc1_clar[i]
  sd_asc1_fish <- distr_sd_asc1_fish[i]
  sd_asc1_bio  <- distr_sd_asc1_bio[i]
  sd_asc1_coast<- distr_sd_asc1_coast[i]
  sd_asc1_lit  <- distr_sd_asc1_lit[i]
  sd_asc1_cost <- distr_sd_asc1_cost[i]
  sd_asc2_clar <- distr_sd_asc2_clar[i]
  sd_asc2_fish <- distr_sd_asc2_fish[i]
  sd_asc2_bio  <- distr_sd_asc2_bio[i]
  sd_asc2_coast<- distr_sd_asc2_coast[i]
  sd_asc2_lit  <- distr_sd_asc2_lit[i]
  sd_asc2_cost <- distr_sd_asc2_cost[i]
  sd_clar_fish <- distr_sd_clar_fish[i]
  sd_clar_bio  <- distr_sd_clar_bio[i]
  sd_clar_coast<- distr_sd_clar_coast[i]
  sd_clar_lit  <- distr_sd_clar_lit[i]
  sd_clar_cost <- distr_sd_clar_cost[i]
  sd_fish_bio  <- distr_sd_fish_bio[i]
  sd_fish_coast<- distr_sd_fish_coast[i]
  sd_fish_lit  <- distr_sd_fish_lit[i]
  sd_fish_cost <- distr_sd_fish_cost[i]
  sd_bio_coast <- distr_sd_bio_coast[i]
  sd_bio_lit   <- distr_sd_bio_lit[i]
  sd_bio_cost  <- distr_sd_bio_cost[i]
  sd_coast_lit <- distr_sd_coast_lit[i]
  sd_coast_cost<- distr_sd_coast_cost[i]
  sd_lit_cost  <- distr_sd_lit_cost[i]



                 choleski.cov <- t(matrix (c(sd_asc1      , 0            , 0           , 0             , 0           , 0            , 0           , 0                        ,
                                             sd_asc1_asc2 , sd_asc2      , 0           , 0             , 0           , 0            , 0           , 0                        ,
                                             sd_asc1_clar , sd_asc2_clar ,sd_clar      , 0             , 0           , 0            , 0           , 0                        ,
                                             sd_asc1_fish , sd_asc2_fish ,sd_clar_fish , sd_fish       , 0           , 0            , 0           , 0                        ,
                                             sd_asc1_bio  , sd_asc2_bio  ,sd_clar_bio  , sd_fish_bio   , sd_bio      , 0            , 0           , 0                        ,     
                                             sd_asc1_coast, sd_asc2_coast,sd_clar_coast, sd_fish_coast , sd_bio_coast, sd_coast     , 0           , 0                        ,
                                             sd_asc1_lit  , sd_asc2_lit  ,sd_clar_lit  , sd_fish_lit   , sd_bio_lit  , sd_coast_lit , sd_lit      , 0                        ,      
                                             sd_asc1_cost , sd_asc2_cost ,sd_clar_cost , sd_fish_cost  , sd_bio_cost , sd_coast_cost, sd_lit_cost , sd_cost), 8))
                 
                 
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
                                         rnorm(NumIter),
                                         rnorm(NumIter))
                 
                 beta.attr      <- cbind(rep(unname(mu_asc1)  ,NumIter),
                                         rep(unname(mu_asc2)  ,NumIter),
                                         rep(unname(mu_clar)  ,NumIter),
                                         rep(unname(mu_fish)  ,NumIter),
                                         rep(unname(mu_bio)   ,NumIter),
                                         rep(unname(mu_coast) ,NumIter),
                                         rep(unname(mu_lit)   ,NumIter),
                                         rep(unname(mu_cost)  ,NumIter) )
                 
                 random.attr    <- normal.0.1 %*% t(choleski.cov) + beta.attr
                 colnames(random.attr) <- c("mu_asc1","mu_asc2","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
                 
                 
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


write.table(simu_distr_median_WTP, file="./simu_WTP_3Alt_2026_02_26.csv", sep=" ", row.names = TRUE, col.names=TRUE)
write.table(median_CI_WTP, file="./median_CI_WTP_3Alt_2026_02_26.csv", sep=" ", row.names = TRUE, col.names=TRUE)


