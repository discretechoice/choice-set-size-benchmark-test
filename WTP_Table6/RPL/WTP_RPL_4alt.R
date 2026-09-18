####################################################################
####################################################################
#####
#####            WTP calculations
#####            RPL - 4 alternatives - treatment 3
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
       
b_mu_asc1         <- -1.85743
b_mu_asc2         <-  0.23968
b_mu_asc3         <-  0.42282
b_mu_clar         <-  0.40128
b_mu_fish         <-  0.73812
b_mu_bio          <-  0.69037
b_mu_coast        <-  0.06226
b_mu_lit          <-  0.73724
b_mu_cost         <- -6.26402
b_sd_asc1         <-  2.97966
b_sd_asc2         <- -0.09889
b_sd_asc3         <-  0.10571
b_sd_clar         <-  0.27987
b_sd_fish         <-  0.43609
b_sd_bio          <- -0.47548
b_sd_coast        <- -0.42736
b_sd_lit          <-  0.32326
b_sd_cost         <-  1.67726
b_sd_asc1_asc2    <-  0.01046
b_sd_asc1_asc3    <- -0.18782
b_sd_asc1_clar    <-  0.03012
b_sd_asc1_fish    <- -0.11365
b_sd_asc1_bio     <- -0.05418
b_sd_asc1_coast   <- -0.02104
b_sd_asc1_lit     <-  0.23331
b_sd_asc1_cost    <-  0.55024
b_sd_asc2_asc3    <-  0.69916
b_sd_asc2_clar    <-  0.02071
b_sd_asc2_fish    <- -0.28270
b_sd_asc2_bio     <- -0.04294
b_sd_asc2_coast   <-  0.08353
b_sd_asc2_lit     <-  0.06396
b_sd_asc2_cost    <-  0.27357
b_sd_asc3_clar    <-  0.22140
b_sd_asc3_fish    <- -0.28932
b_sd_asc3_bio     <- -0.06325
b_sd_asc3_coast   <- -0.32810
b_sd_asc3_lit     <- -0.03179
b_sd_asc3_cost    <-  2.04165
b_sd_clar_fish    <-  0.44113
b_sd_clar_bio     <-  0.28507
b_sd_clar_coast   <-  0.34184
b_sd_clar_lit     <-  0.44470
b_sd_clar_cost    <-  0.59018
b_sd_fish_bio     <-  0.23145
b_sd_fish_coast   <- -0.23289
b_sd_fish_lit     <-  0.14777
b_sd_fish_cost    <- -0.87733
b_sd_bio_coast    <-  0.11486
b_sd_bio_lit      <- -0.16374
b_sd_bio_cost     <- -1.81260
b_sd_coast_lit    <-  0.12530
b_sd_coast_cost   <- -1.00581
b_sd_lit_cost     <-  0.04662


se_mu_asc1         <- 2.71476
se_mu_asc2         <- 0.11256
se_mu_asc3         <- 0.11281
se_mu_clar         <- 0.11038
se_mu_fish         <- 0.19164
se_mu_bio          <- 0.14541
se_mu_coast        <- 0.11814
se_mu_lit          <- 0.07838
se_mu_cost         <- 0.68990
se_sd_asc1         <- 2.87538
se_sd_asc2         <- 0.28418
se_sd_asc3         <- 0.70196
se_sd_clar         <- 0.18235
se_sd_fish         <- 0.60096
se_sd_bio          <- 0.16593
se_sd_coast        <- 1.03482
se_sd_lit          <- 0.14156
se_sd_cost         <- 0.19229
se_sd_asc1_asc2    <- 0.53867
se_sd_asc1_asc3    <- 0.26801
se_sd_asc1_clar    <- 0.22136
se_sd_asc1_fish    <- 0.26652
se_sd_asc1_bio     <- 0.64207
se_sd_asc1_coast   <- 0.61969
se_sd_asc1_lit     <- 0.69300
se_sd_asc1_cost    <- 0.38434
se_sd_asc2_asc3    <- 0.28432
se_sd_asc2_clar    <- 0.23320
se_sd_asc2_fish    <- 0.49201
se_sd_asc2_bio     <- 0.13932
se_sd_asc2_coast   <- 0.55469
se_sd_asc2_lit     <- 0.19168
se_sd_asc2_cost    <- 0.19810
se_sd_asc3_clar    <- 0.11214
se_sd_asc3_fish    <- 0.62088
se_sd_asc3_bio     <- 0.12366
se_sd_asc3_coast   <- 0.16368
se_sd_asc3_lit     <- 0.16768
se_sd_asc3_cost    <- 0.12659
se_sd_clar_fish    <- 0.20657
se_sd_clar_bio     <- 0.24891
se_sd_clar_coast   <- 0.14793
se_sd_clar_lit     <- 0.11634
se_sd_clar_cost    <- 0.37150
se_sd_fish_bio     <- 0.59037
se_sd_fish_coast   <- 1.27949
se_sd_fish_lit     <- 0.68976
se_sd_fish_cost    <- 0.09946
se_sd_bio_coast    <- 0.26002
se_sd_bio_lit      <- 0.40267
se_sd_bio_cost     <- 0.40666
se_sd_coast_lit    <- 0.32573
se_sd_coast_cost   <- 0.11162
se_sd_lit_cost     <- 0.31449


n.ite <- 1000

distr_mu_asc1       <- rnorm(n.ite, b_mu_asc1           , se_mu_asc1         )
distr_mu_asc2       <- rnorm(n.ite, b_mu_asc2           , se_mu_asc2         )
distr_mu_asc3       <- rnorm(n.ite, b_mu_asc3           , se_mu_asc3         )
distr_mu_clar       <- rnorm(n.ite, b_mu_clar           , se_mu_clar         )
distr_mu_fish       <- rnorm(n.ite, b_mu_fish           , se_mu_fish         )
distr_mu_bio        <- rnorm(n.ite, b_mu_bio            , se_mu_bio          )
distr_mu_coast      <- rnorm(n.ite, b_mu_coast          , se_mu_coast        )
distr_mu_lit        <- rnorm(n.ite, b_mu_lit            , se_mu_lit          )
distr_mu_cost       <- rnorm(n.ite, b_mu_cost           , se_mu_cost         )
distr_sd_asc1       <- rnorm(n.ite, b_sd_asc1           , se_sd_asc1         )
distr_sd_asc2       <- rnorm(n.ite, b_sd_asc2           , se_sd_asc2         )
distr_sd_asc3       <- rnorm(n.ite, b_sd_asc3           , se_sd_asc3         )
distr_sd_clar       <- rnorm(n.ite, b_sd_clar           , se_sd_clar         )
distr_sd_fish       <- rnorm(n.ite, b_sd_fish           , se_sd_fish         )
distr_sd_bio        <- rnorm(n.ite, b_sd_bio            , se_sd_bio          )
distr_sd_coast      <- rnorm(n.ite, b_sd_coast          , se_sd_coast        )
distr_sd_lit        <- rnorm(n.ite, b_sd_lit            , se_sd_lit          )
distr_sd_cost       <- rnorm(n.ite, b_sd_cost           , se_sd_cost         )
distr_sd_asc1_asc2  <- rnorm(n.ite, b_sd_asc1_asc2      , se_sd_asc1_asc2    )
distr_sd_asc1_asc3  <- rnorm(n.ite, b_sd_asc1_asc3      , se_sd_asc1_asc3    )
distr_sd_asc1_clar  <- rnorm(n.ite, b_sd_asc1_clar      , se_sd_asc1_clar    )
distr_sd_asc1_fish  <- rnorm(n.ite, b_sd_asc1_fish      , se_sd_asc1_fish    )
distr_sd_asc1_bio   <- rnorm(n.ite, b_sd_asc1_bio       , se_sd_asc1_bio     )
distr_sd_asc1_coast <- rnorm(n.ite, b_sd_asc1_coast     , se_sd_asc1_coast   )
distr_sd_asc1_lit   <- rnorm(n.ite, b_sd_asc1_lit       , se_sd_asc1_lit     )
distr_sd_asc1_cost  <- rnorm(n.ite, b_sd_asc1_cost      , se_sd_asc1_cost    )
distr_sd_asc2_asc3  <- rnorm(n.ite, b_sd_asc2_asc3      , se_sd_asc2_asc3    )
distr_sd_asc2_clar  <- rnorm(n.ite, b_sd_asc2_clar      , se_sd_asc2_clar    )
distr_sd_asc2_fish  <- rnorm(n.ite, b_sd_asc2_fish      , se_sd_asc2_fish    )
distr_sd_asc2_bio   <- rnorm(n.ite, b_sd_asc2_bio       , se_sd_asc2_bio     )
distr_sd_asc2_coast <- rnorm(n.ite, b_sd_asc2_coast     , se_sd_asc2_coast   )
distr_sd_asc2_lit   <- rnorm(n.ite, b_sd_asc2_lit       , se_sd_asc2_lit     )
distr_sd_asc2_cost  <- rnorm(n.ite, b_sd_asc2_cost      , se_sd_asc2_cost    )
distr_sd_asc3_clar  <- rnorm(n.ite, b_sd_asc3_clar      , se_sd_asc3_clar    )
distr_sd_asc3_fish  <- rnorm(n.ite, b_sd_asc3_fish      , se_sd_asc3_fish    )
distr_sd_asc3_bio   <- rnorm(n.ite, b_sd_asc3_bio       , se_sd_asc3_bio     )
distr_sd_asc3_coast <- rnorm(n.ite, b_sd_asc3_coast     , se_sd_asc3_coast   )
distr_sd_asc3_lit   <- rnorm(n.ite, b_sd_asc3_lit       , se_sd_asc3_lit     )
distr_sd_asc3_cost  <- rnorm(n.ite, b_sd_asc3_cost      , se_sd_asc3_cost    )
distr_sd_clar_fish  <- rnorm(n.ite, b_sd_clar_fish      , se_sd_clar_fish    )
distr_sd_clar_bio   <- rnorm(n.ite, b_sd_clar_bio       , se_sd_clar_bio     )
distr_sd_clar_coast <- rnorm(n.ite, b_sd_clar_coast     , se_sd_clar_coast   )
distr_sd_clar_lit   <- rnorm(n.ite, b_sd_clar_lit       , se_sd_clar_lit     )
distr_sd_clar_cost  <- rnorm(n.ite, b_sd_clar_cost      , se_sd_clar_cost    )
distr_sd_fish_bio   <- rnorm(n.ite, b_sd_fish_bio       , se_sd_fish_bio     )
distr_sd_fish_coast <- rnorm(n.ite, b_sd_fish_coast     , se_sd_fish_coast   )
distr_sd_fish_lit   <- rnorm(n.ite, b_sd_fish_lit       , se_sd_fish_lit     )
distr_sd_fish_cost  <- rnorm(n.ite, b_sd_fish_cost      , se_sd_fish_cost    )
distr_sd_bio_coast  <- rnorm(n.ite, b_sd_bio_coast      , se_sd_bio_coast    )
distr_sd_bio_lit    <- rnorm(n.ite, b_sd_bio_lit        , se_sd_bio_lit      )
distr_sd_bio_cost   <- rnorm(n.ite, b_sd_bio_cost       , se_sd_bio_cost     )
distr_sd_coast_lit  <- rnorm(n.ite, b_sd_coast_lit      , se_sd_coast_lit    )
distr_sd_coast_cost <- rnorm(n.ite, b_sd_coast_cost     , se_sd_coast_cost   )
distr_sd_lit_cost   <- rnorm(n.ite, b_sd_lit_cost       , se_sd_lit_cost     )


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
  
    mu_asc1       <-  distr_mu_asc1[i]
    mu_asc2       <-  distr_mu_asc2[i]
    mu_asc3       <-  distr_mu_asc3[i]
    mu_clar       <-  distr_mu_clar[i]
    mu_fish       <-  distr_mu_fish[i]
    mu_bio        <-  distr_mu_bio[i]
    mu_coast      <-  distr_mu_coast[i]
    mu_lit        <-  distr_mu_lit[i]
    mu_cost       <-  distr_mu_cost[i]
    sd_asc1       <-  distr_sd_asc1[i]
    sd_asc2       <-  distr_sd_asc2[i]
    sd_asc3       <-  distr_sd_asc3[i]
    sd_clar       <-  distr_sd_clar[i]
    sd_fish       <-  distr_sd_fish[i]
    sd_bio        <-  distr_sd_bio[i]
    sd_coast      <-  distr_sd_coast[i]
    sd_lit        <-  distr_sd_lit[i]
    sd_cost       <-  distr_sd_cost[i]
    sd_asc1_asc2  <-  distr_sd_asc1_asc2[i]
    sd_asc1_asc3  <-  distr_sd_asc1_asc3[i]
    sd_asc1_clar  <-  distr_sd_asc1_clar[i]
    sd_asc1_fish  <-  distr_sd_asc1_fish[i]
    sd_asc1_bio   <-  distr_sd_asc1_bio[i]
    sd_asc1_coast <-  distr_sd_asc1_coast[i]
    sd_asc1_lit   <-  distr_sd_asc1_lit[i]
    sd_asc1_cost  <-  distr_sd_asc1_cost[i]
    sd_asc2_asc3  <-  distr_sd_asc2_asc3[i]
    sd_asc2_clar  <-  distr_sd_asc2_clar[i]
    sd_asc2_fish  <-  distr_sd_asc2_fish[i]
    sd_asc2_bio   <-  distr_sd_asc2_bio[i]
    sd_asc2_coast <-  distr_sd_asc2_coast[i]
    sd_asc2_lit   <-  distr_sd_asc2_lit[i]
    sd_asc2_cost  <-  distr_sd_asc2_cost[i]
    sd_asc3_clar  <-  distr_sd_asc3_clar[i]
    sd_asc3_fish  <-  distr_sd_asc3_fish[i]
    sd_asc3_bio   <-  distr_sd_asc3_bio[i]
    sd_asc3_coast <-  distr_sd_asc3_coast[i]
    sd_asc3_lit   <-  distr_sd_asc3_lit[i]
    sd_asc3_cost  <-  distr_sd_asc3_cost[i]
    sd_clar_fish  <-  distr_sd_clar_fish[i]
    sd_clar_bio   <-  distr_sd_clar_bio[i]
    sd_clar_coast <-  distr_sd_clar_coast[i]
    sd_clar_lit   <-  distr_sd_clar_lit[i]
    sd_clar_cost  <-  distr_sd_clar_cost[i]
    sd_fish_bio   <-  distr_sd_fish_bio[i]
    sd_fish_coast <-  distr_sd_fish_coast[i]
    sd_fish_lit   <-  distr_sd_fish_lit[i]
    sd_fish_cost  <-  distr_sd_fish_cost[i]
    sd_bio_coast  <-  distr_sd_bio_coast[i]
    sd_bio_lit    <-  distr_sd_bio_lit[i]
    sd_bio_cost   <-  distr_sd_bio_cost[i]
    sd_coast_lit  <-  distr_sd_coast_lit[i]
    sd_coast_cost <-  distr_sd_coast_cost[i]
    sd_lit_cost   <-  distr_sd_lit_cost [i]
  


                     choleski.cov <- t(matrix (c(sd_asc1      , 0            , 0           , 0            , 0             , 0           , 0            , 0           , 0                        ,
                                                 sd_asc1_asc2 , sd_asc2      , 0           , 0            , 0             , 0           , 0            , 0           , 0                        ,
                                                 sd_asc1_asc3 , sd_asc2_asc3 ,sd_asc3      , 0            , 0             , 0           , 0            , 0           , 0                        ,
                                                 sd_asc1_clar , sd_asc2_clar ,sd_asc3_clar , sd_clar      , 0             , 0           , 0            , 0           , 0                        ,
                                                 sd_asc1_fish , sd_asc2_fish ,sd_asc3_fish , sd_clar_fish , sd_fish       , 0           , 0            , 0           , 0                        ,
                                                 sd_asc1_bio  , sd_asc2_bio  ,sd_asc3_bio  , sd_clar_bio  , sd_fish_bio   , sd_bio      , 0            , 0           , 0                        ,     
                                                 sd_asc1_coast, sd_asc2_coast,sd_asc3_coast, sd_clar_coast, sd_fish_coast , sd_bio_coast, sd_coast     , 0           , 0                        ,
                                                 sd_asc1_lit  , sd_asc2_lit  ,sd_asc3_lit  , sd_clar_lit  , sd_fish_lit   , sd_bio_lit  , sd_coast_lit , sd_lit      , 0                        ,      
                                                 sd_asc1_cost , sd_asc2_cost ,sd_asc3_cost , sd_clar_cost , sd_fish_cost  , sd_bio_cost , sd_coast_cost, sd_lit_cost , sd_cost), 9))
                     
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
                                             rnorm(NumIter),
                                             rnorm(NumIter))
                     
                     beta.attr      <- cbind(rep(unname(mu_asc1)  ,NumIter),
                                             rep(unname(mu_asc2)  ,NumIter),
                                             rep(unname(mu_asc3)  ,NumIter),
                                             rep(unname(mu_clar)  ,NumIter),
                                             rep(unname(mu_fish)  ,NumIter),
                                             rep(unname(mu_bio)   ,NumIter),
                                             rep(unname(mu_coast) ,NumIter),
                                             rep(unname(mu_lit)   ,NumIter),
                                             rep(unname(mu_cost)  ,NumIter) )
                     
                     random.attr    <- normal.0.1 %*% t(choleski.cov) + beta.attr
                     colnames(random.attr) <- c("mu_asc1","mu_asc2","mu_asc3","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
                     
                     
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


write.table(simu_distr_median_WTP, file="./simu_WTP_4Alt_2026_02_26.csv", sep=" ", row.names = TRUE, col.names=TRUE)
write.table(median_CI_WTP, file="./median_CI_WTP_4Alt_2026_02_26.csv", sep=" ", row.names = TRUE, col.names=TRUE)


