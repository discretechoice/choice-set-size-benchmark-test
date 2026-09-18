####################################################################
####################################################################
#####
#####            WTP calculations
#####            RPL - 5 alternatives - treatment 4
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
       
b_mu_asc1        <- -1.781953
b_mu_asc2        <-  0.105854
b_mu_asc3        <-  0.508808
b_mu_asc4        <-  0.487587
b_mu_clar        <-  0.303056
b_mu_fish        <-  0.622605
b_mu_bio         <-  0.634028
b_mu_coast       <-  0.128749
b_mu_lit         <-  0.748648
b_mu_cost        <- -6.627173
b_sd_asc1        <-  3.604989
b_sd_asc2        <- -0.902238
b_sd_asc3        <-  0.191416
b_sd_asc4        <-  0.391316
b_sd_clar        <-  0.034524
b_sd_fish        <- -0.174344
b_sd_bio         <-  0.158558
b_sd_coast       <-  0.113069
b_sd_lit         <-  0.147092
b_sd_cost        <-  1.037462
b_sd_asc1_asc2   <-  0.812311
b_sd_asc1_asc3   <-  0.700856
b_sd_asc1_asc4   <-  0.495216
b_sd_asc1_clar   <- -0.052094
b_sd_asc1_fish   <- -0.369140
b_sd_asc1_bio    <- -0.065183
b_sd_asc1_coast  <- -0.282597
b_sd_asc1_lit    <- -0.108562
b_sd_asc1_cost   <-  0.510345
b_sd_asc2_asc3   <- -0.086949
b_sd_asc2_asc4   <-  0.283025
b_sd_asc2_clar   <-  0.149203
b_sd_asc2_fish   <-  0.192008
b_sd_asc2_bio    <-  0.085110
b_sd_asc2_coast  <-  0.139002
b_sd_asc2_lit    <-  0.068396
b_sd_asc2_cost   <-  2.052089
b_sd_asc3_asc4   <- -0.234600
b_sd_asc3_clar   <- -0.095703
b_sd_asc3_fish   <- -0.278330
b_sd_asc3_bio    <- -0.219635
b_sd_asc3_coast  <- -0.136678
b_sd_asc3_lit    <- -0.062935
b_sd_asc3_cost   <-  1.986044
b_sd_asc4_clar   <- -0.357460
b_sd_asc4_fish   <- -0.290408
b_sd_asc4_bio    <- -0.081599
b_sd_asc4_coast  <- -0.009892
b_sd_asc4_lit    <- -0.045461
b_sd_asc4_cost   <-  0.293099
b_sd_clar_fish   <- -0.824525
b_sd_clar_bio    <- -0.103563
b_sd_clar_coast  <- -0.095011
b_sd_clar_lit    <- -0.153070
b_sd_clar_cost   <- -1.753533
b_sd_fish_bio    <-  0.495983
b_sd_fish_coast  <- -0.058646
b_sd_fish_lit    <-  0.521579
b_sd_fish_cost   <- -0.344893
b_sd_bio_coast   <-  0.338255
b_sd_bio_lit     <- -0.023455
b_sd_bio_cost    <- -0.374675
b_sd_coast_lit   <-  0.479954
b_sd_coast_cost  <- -0.823746
b_sd_lit_cost    <- -3.179869

se_mu_asc1        <- 0.47575
se_mu_asc2        <- 0.13949
se_mu_asc3        <- 0.13007
se_mu_asc4        <- 0.11594
se_mu_clar        <- 0.04664
se_mu_fish        <- 0.11465
se_mu_bio         <- 0.05800
se_mu_coast       <- 0.08786
se_mu_lit         <- 0.06287
se_mu_cost        <- 0.18301
se_sd_asc1        <- 0.59465
se_sd_asc2        <- 0.23975
se_sd_asc3        <- 0.32640
se_sd_asc4        <- 0.12712
se_sd_clar        <- 0.04599
se_sd_fish        <- 0.12215
se_sd_bio         <- 0.05939
se_sd_coast       <- 0.10861
se_sd_lit         <- 0.08627
se_sd_cost        <- 0.06131
se_sd_asc1_asc2   <- 0.27311
se_sd_asc1_asc3   <- 0.30299
se_sd_asc1_asc4   <- 0.21838
se_sd_asc1_clar   <- 0.08981
se_sd_asc1_fish   <- 0.24326
se_sd_asc1_bio    <- 0.13645
se_sd_asc1_coast  <- 0.17101
se_sd_asc1_lit    <- 0.09594
se_sd_asc1_cost   <- 0.13286
se_sd_asc2_asc3   <- 0.27678
se_sd_asc2_asc4   <- 0.22173
se_sd_asc2_clar   <- 0.07829
se_sd_asc2_fish   <- 0.20638
se_sd_asc2_bio    <- 0.07954
se_sd_asc2_coast  <- 0.14697
se_sd_asc2_lit    <- 0.05929
se_sd_asc2_cost   <- 0.14031
se_sd_asc3_asc4   <- 0.21101
se_sd_asc3_clar   <- 0.05228
se_sd_asc3_fish   <- 0.12343
se_sd_asc3_bio    <- 0.05528
se_sd_asc3_coast  <- 0.15263
se_sd_asc3_lit    <- 0.05949
se_sd_asc3_cost   <- 0.16176
se_sd_asc4_clar   <- 0.05579
se_sd_asc4_fish   <- 0.13432
se_sd_asc4_bio    <- 0.06774
se_sd_asc4_coast  <- 0.11333
se_sd_asc4_lit    <- 0.05229
se_sd_asc4_cost   <- 0.06934
se_sd_clar_fish   <- 0.15435
se_sd_clar_bio    <- 0.05718
se_sd_clar_coast  <- 0.11442
se_sd_clar_lit    <- 0.05140
se_sd_clar_cost   <- 0.08571
se_sd_fish_bio    <- 0.05834
se_sd_fish_coast  <- 0.09866
se_sd_fish_lit    <- 0.05870
se_sd_fish_cost   <- 0.07164
se_sd_bio_coast   <- 0.12492
se_sd_bio_lit     <- 0.05300
se_sd_bio_cost    <- 0.06367
se_sd_coast_lit   <- 0.05936
se_sd_coast_cost  <- 0.09565
se_sd_lit_cost    <- 0.15128


n.ite <- 1000


distr_mu_asc1        <-rnorm(n.ite, b_mu_asc1                 , se_mu_asc1           )
distr_mu_asc2        <-rnorm(n.ite, b_mu_asc2                 , se_mu_asc2           )
distr_mu_asc3        <-rnorm(n.ite, b_mu_asc3                 , se_mu_asc3           )
distr_mu_asc4        <-rnorm(n.ite, b_mu_asc4                 , se_mu_asc4           )
distr_mu_clar        <-rnorm(n.ite, b_mu_clar                 , se_mu_clar           )
distr_mu_fish        <-rnorm(n.ite, b_mu_fish                 , se_mu_fish           )
distr_mu_bio         <-rnorm(n.ite, b_mu_bio                  , se_mu_bio            )
distr_mu_coast       <-rnorm(n.ite, b_mu_coast                , se_mu_coast          )
distr_mu_lit         <-rnorm(n.ite, b_mu_lit                  , se_mu_lit            )
distr_mu_cost        <-rnorm(n.ite, b_mu_cost                 , se_mu_cost           )
distr_sd_asc1        <-rnorm(n.ite, b_sd_asc1                 , se_sd_asc1           )
distr_sd_asc2        <-rnorm(n.ite, b_sd_asc2                 , se_sd_asc2           )
distr_sd_asc3        <-rnorm(n.ite, b_sd_asc3                 , se_sd_asc3           )
distr_sd_asc4        <-rnorm(n.ite, b_sd_asc4                 , se_sd_asc4           )
distr_sd_clar        <-rnorm(n.ite, b_sd_clar                 , se_sd_clar           )
distr_sd_fish        <-rnorm(n.ite, b_sd_fish                 , se_sd_fish           )
distr_sd_bio         <-rnorm(n.ite, b_sd_bio                  , se_sd_bio            )
distr_sd_coast       <-rnorm(n.ite, b_sd_coast                , se_sd_coast          )
distr_sd_lit         <-rnorm(n.ite, b_sd_lit                  , se_sd_lit            )
distr_sd_cost        <-rnorm(n.ite, b_sd_cost                 , se_sd_cost           )
distr_sd_asc1_asc2   <-rnorm(n.ite, b_sd_asc1_asc2            , se_sd_asc1_asc2      )
distr_sd_asc1_asc3   <-rnorm(n.ite, b_sd_asc1_asc3            , se_sd_asc1_asc3      )
distr_sd_asc1_asc4   <-rnorm(n.ite, b_sd_asc1_asc4            , se_sd_asc1_asc4      )
distr_sd_asc1_clar   <-rnorm(n.ite, b_sd_asc1_clar            , se_sd_asc1_clar      )
distr_sd_asc1_fish   <-rnorm(n.ite, b_sd_asc1_fish            , se_sd_asc1_fish      )
distr_sd_asc1_bio    <-rnorm(n.ite, b_sd_asc1_bio             , se_sd_asc1_bio       )
distr_sd_asc1_coast  <-rnorm(n.ite, b_sd_asc1_coast           , se_sd_asc1_coast     )
distr_sd_asc1_lit    <-rnorm(n.ite, b_sd_asc1_lit             , se_sd_asc1_lit       )
distr_sd_asc1_cost   <-rnorm(n.ite, b_sd_asc1_cost            , se_sd_asc1_cost      )
distr_sd_asc2_asc3   <-rnorm(n.ite, b_sd_asc2_asc3            , se_sd_asc2_asc3      )
distr_sd_asc2_asc4   <-rnorm(n.ite, b_sd_asc2_asc4            , se_sd_asc2_asc4      )
distr_sd_asc2_clar   <-rnorm(n.ite, b_sd_asc2_clar            , se_sd_asc2_clar      )
distr_sd_asc2_fish   <-rnorm(n.ite, b_sd_asc2_fish            , se_sd_asc2_fish      )
distr_sd_asc2_bio    <-rnorm(n.ite, b_sd_asc2_bio             , se_sd_asc2_bio       )
distr_sd_asc2_coast  <-rnorm(n.ite, b_sd_asc2_coast           , se_sd_asc2_coast     )
distr_sd_asc2_lit    <-rnorm(n.ite, b_sd_asc2_lit             , se_sd_asc2_lit       )
distr_sd_asc2_cost   <-rnorm(n.ite, b_sd_asc2_cost            , se_sd_asc2_cost      )
distr_sd_asc3_asc4   <-rnorm(n.ite, b_sd_asc3_asc4            , se_sd_asc3_asc4      )
distr_sd_asc3_clar   <-rnorm(n.ite, b_sd_asc3_clar            , se_sd_asc3_clar      )
distr_sd_asc3_fish   <-rnorm(n.ite, b_sd_asc3_fish            , se_sd_asc3_fish      )
distr_sd_asc3_bio    <-rnorm(n.ite, b_sd_asc3_bio             , se_sd_asc3_bio       )
distr_sd_asc3_coast  <-rnorm(n.ite, b_sd_asc3_coast           , se_sd_asc3_coast     )
distr_sd_asc3_lit    <-rnorm(n.ite, b_sd_asc3_lit             , se_sd_asc3_lit       )
distr_sd_asc3_cost   <-rnorm(n.ite, b_sd_asc3_cost            , se_sd_asc3_cost      )
distr_sd_asc4_clar   <-rnorm(n.ite, b_sd_asc4_clar            , se_sd_asc4_clar      )
distr_sd_asc4_fish   <-rnorm(n.ite, b_sd_asc4_fish            , se_sd_asc4_fish      )
distr_sd_asc4_bio    <-rnorm(n.ite, b_sd_asc4_bio             , se_sd_asc4_bio       )
distr_sd_asc4_coast  <-rnorm(n.ite, b_sd_asc4_coast           , se_sd_asc4_coast     )
distr_sd_asc4_lit    <-rnorm(n.ite, b_sd_asc4_lit             , se_sd_asc4_lit       )
distr_sd_asc4_cost   <-rnorm(n.ite, b_sd_asc4_cost            , se_sd_asc4_cost      )
distr_sd_clar_fish   <-rnorm(n.ite, b_sd_clar_fish            , se_sd_clar_fish      )
distr_sd_clar_bio    <-rnorm(n.ite, b_sd_clar_bio             , se_sd_clar_bio       )
distr_sd_clar_coast  <-rnorm(n.ite, b_sd_clar_coast           , se_sd_clar_coast     )
distr_sd_clar_lit    <-rnorm(n.ite, b_sd_clar_lit             , se_sd_clar_lit       )
distr_sd_clar_cost   <-rnorm(n.ite, b_sd_clar_cost            , se_sd_clar_cost      )
distr_sd_fish_bio    <-rnorm(n.ite, b_sd_fish_bio             , se_sd_fish_bio       )
distr_sd_fish_coast  <-rnorm(n.ite, b_sd_fish_coast           , se_sd_fish_coast     )
distr_sd_fish_lit    <-rnorm(n.ite, b_sd_fish_lit             , se_sd_fish_lit       )
distr_sd_fish_cost   <-rnorm(n.ite, b_sd_fish_cost            , se_sd_fish_cost      )
distr_sd_bio_coast   <-rnorm(n.ite, b_sd_bio_coast            , se_sd_bio_coast      )
distr_sd_bio_lit     <-rnorm(n.ite, b_sd_bio_lit              , se_sd_bio_lit        )
distr_sd_bio_cost    <-rnorm(n.ite, b_sd_bio_cost             , se_sd_bio_cost       )
distr_sd_coast_lit   <-rnorm(n.ite, b_sd_coast_lit            , se_sd_coast_lit      )
distr_sd_coast_cost  <-rnorm(n.ite, b_sd_coast_cost           , se_sd_coast_cost     )
distr_sd_lit_cost    <-rnorm(n.ite, b_sd_lit_cost             , se_sd_lit_cost       )


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
  mu_asc3      <- distr_mu_asc3[i]
  mu_asc4      <- distr_mu_asc4[i]
  mu_clar      <- distr_mu_clar[i]
  mu_fish      <- distr_mu_fish[i]
  mu_bio       <- distr_mu_bio[i]
  mu_coast     <- distr_mu_coast[i]
  mu_lit       <- distr_mu_lit[i]
  mu_cost      <- distr_mu_cost[i]
  sd_asc1      <- distr_sd_asc1[i]
  sd_asc2      <- distr_sd_asc2[i]
  sd_asc3      <- distr_sd_asc3[i]
  sd_asc4      <- distr_sd_asc4[i]
  sd_clar      <- distr_sd_clar[i]
  sd_fish      <- distr_sd_fish[i]
  sd_bio       <- distr_sd_bio[i]
  sd_coast     <- distr_sd_coast[i]
  sd_lit       <- distr_sd_lit[i]
  sd_cost      <- distr_sd_cost[i]
  sd_asc1_asc2 <- distr_sd_asc1_asc2[i]
  sd_asc1_asc3 <- distr_sd_asc1_asc3[i]
  sd_asc1_asc4 <- distr_sd_asc1_asc4[i]
  sd_asc1_clar <- distr_sd_asc1_clar[i]
  sd_asc1_fish <- distr_sd_asc1_fish[i]
  sd_asc1_bio  <- distr_sd_asc1_bio[i]
  sd_asc1_coast<- distr_sd_asc1_coast[i]
  sd_asc1_lit  <- distr_sd_asc1_lit[i]
  sd_asc1_cost <- distr_sd_asc1_cost[i]
  sd_asc2_asc3 <- distr_sd_asc2_asc3[i]
  sd_asc2_asc4 <- distr_sd_asc2_asc4[i]
  sd_asc2_clar <- distr_sd_asc2_clar[i]
  sd_asc2_fish <- distr_sd_asc2_fish[i]
  sd_asc2_bio  <- distr_sd_asc2_bio[i]
  sd_asc2_coast<- distr_sd_asc2_coast[i]
  sd_asc2_lit  <- distr_sd_asc2_lit[i]
  sd_asc2_cost <- distr_sd_asc2_cost[i]
  sd_asc3_asc4 <- distr_sd_asc3_asc4[i]
  sd_asc3_clar <- distr_sd_asc3_clar[i]
  sd_asc3_fish <- distr_sd_asc3_fish[i]
  sd_asc3_bio  <- distr_sd_asc3_bio[i]
  sd_asc3_coast<- distr_sd_asc3_coast[i]
  sd_asc3_lit  <- distr_sd_asc3_lit[i]
  sd_asc3_cost <- distr_sd_asc3_cost[i]
  sd_asc4_clar <- distr_sd_asc4_clar[i]
  sd_asc4_fish <- distr_sd_asc4_fish[i]
  sd_asc4_bio  <- distr_sd_asc4_bio[i]
  sd_asc4_coast<- distr_sd_asc4_coast[i]
  sd_asc4_lit  <- distr_sd_asc4_lit[i]
  sd_asc4_cost <- distr_sd_asc4_cost[i]
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


                  choleski.cov <- t(matrix (c(sd_asc1      , 0            , 0           , 0            , 0            , 0             , 0           , 0            , 0           , 0                        ,
                                              sd_asc1_asc2 , sd_asc2      , 0           , 0            , 0            , 0             , 0           , 0            , 0           , 0                        ,
                                              sd_asc1_asc3 , sd_asc2_asc3 ,sd_asc3      , 0            , 0            , 0             , 0           , 0            , 0           , 0                        ,
                                              sd_asc1_asc4 , sd_asc2_asc4 ,sd_asc3_asc4 , sd_asc4      , 0            , 0             , 0           , 0            , 0           , 0                        ,
                                              sd_asc1_clar , sd_asc2_clar ,sd_asc3_clar , sd_asc4_clar , sd_clar      , 0             , 0           , 0            , 0           , 0                        ,
                                              sd_asc1_fish , sd_asc2_fish ,sd_asc3_fish , sd_asc4_fish , sd_clar_fish , sd_fish       , 0           , 0            , 0           , 0                        ,
                                              sd_asc1_bio  , sd_asc2_bio  ,sd_asc3_bio  , sd_asc4_bio  , sd_clar_bio  , sd_fish_bio   , sd_bio      , 0            , 0           , 0                        ,     
                                              sd_asc1_coast, sd_asc2_coast,sd_asc3_coast, sd_asc4_coast, sd_clar_coast, sd_fish_coast , sd_bio_coast, sd_coast     , 0           , 0                        ,
                                              sd_asc1_lit  , sd_asc2_lit  ,sd_asc3_lit  , sd_asc4_lit  , sd_clar_lit  , sd_fish_lit   , sd_bio_lit  , sd_coast_lit , sd_lit      , 0                        ,      
                                              sd_asc1_cost , sd_asc2_cost ,sd_asc3_cost , sd_asc4_cost , sd_clar_cost , sd_fish_cost  , sd_bio_cost , sd_coast_cost, sd_lit_cost , sd_cost), 10))
                  
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
                                          rnorm(NumIter),
                                          rnorm(NumIter))
                  
                  beta.attr      <- cbind(rep(unname(mu_asc1)  ,NumIter),
                                          rep(unname(mu_asc2)  ,NumIter),
                                          rep(unname(mu_asc3)  ,NumIter),
                                          rep(unname(mu_asc4)  ,NumIter),
                                          rep(unname(mu_clar)  ,NumIter),
                                          rep(unname(mu_fish)  ,NumIter),
                                          rep(unname(mu_bio)   ,NumIter),
                                          rep(unname(mu_coast) ,NumIter),
                                          rep(unname(mu_lit)   ,NumIter),
                                          rep(unname(mu_cost)  ,NumIter) )
                  
                  random.attr    <- normal.0.1 %*% t(choleski.cov) + beta.attr
                  colnames(random.attr) <- c("mu_asc1","mu_asc2","mu_asc3","mu_asc4","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
                  
                  
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


write.table(simu_distr_median_WTP, file="./simu_WTP_5Alt_2026_02_26.csv", sep=" ", row.names = TRUE, col.names=TRUE)
write.table(median_CI_WTP, file="./median_CI_WTP_5Alt_2026_02_26.csv", sep=" ", row.names = TRUE, col.names=TRUE)


