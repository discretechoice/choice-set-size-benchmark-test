####################################################################
####################################################################
#####
#####            WTP calculations
#####            RPL - 6 alternatives - treatment 5
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
       
b_mu_asc1        <- -1.074161
b_mu_asc2        <-  0.110077
b_mu_asc3        <-  0.369245
b_mu_asc4        <-  0.427446
b_mu_asc5        <-  0.110221
b_mu_clar        <-  0.249632
b_mu_fish        <-  0.781123
b_mu_bio         <-  0.696729
b_mu_coast       <- -0.001183
b_mu_lit         <-  0.732562
b_mu_cost        <- -8.379574
b_sd_asc1        <-  2.862239
b_sd_asc2        <-  0.753862
b_sd_asc3        <- -0.664382
b_sd_asc4        <- -0.212165
b_sd_asc5        <-  0.138718
b_sd_clar        <- -0.015196
b_sd_fish        <- -0.239521
b_sd_bio         <-  0.297157
b_sd_coast       <-  0.460372
b_sd_lit         <-  0.041186
b_sd_cost        <-  0.182238
b_sd_asc1_asc2   <- -0.368619
b_sd_asc1_asc3   <-  0.072039
b_sd_asc1_asc4   <- -0.136148
b_sd_asc1_asc5   <- -0.413014
b_sd_asc1_clar   <-  0.053072
b_sd_asc1_fish   <- -0.031333
b_sd_asc1_bio    <-  0.182922
b_sd_asc1_coast  <-  0.090530
b_sd_asc1_lit    <-  0.091754
b_sd_asc1_cost   <-  0.710149
b_sd_asc2_asc3   <-  0.475918
b_sd_asc2_asc4   <-  0.113223
b_sd_asc2_asc5   <- -0.114828
b_sd_asc2_clar   <-  0.079106
b_sd_asc2_fish   <-  0.050329
b_sd_asc2_bio    <- -0.194028
b_sd_asc2_coast  <- -0.214853
b_sd_asc2_lit    <- -0.303283
b_sd_asc2_cost   <-  2.067593
b_sd_asc3_asc4   <- -0.889640
b_sd_asc3_asc5   <- -0.429530
b_sd_asc3_clar   <-  0.241911
b_sd_asc3_fish   <-  0.103507
b_sd_asc3_bio    <-  0.259337
b_sd_asc3_coast  <-  0.158417
b_sd_asc3_lit    <-  0.396461
b_sd_asc3_cost   <-  0.152293
b_sd_asc4_asc5   <- -0.094790
b_sd_asc4_clar   <- -0.292624
b_sd_asc4_fish   <-  0.055934
b_sd_asc4_bio    <-  0.187041
b_sd_asc4_coast  <-  0.106054
b_sd_asc4_lit    <-  0.028717
b_sd_asc4_cost   <- -0.579723
b_sd_asc5_clar   <- -0.187594
b_sd_asc5_fish   <- -0.439566
b_sd_asc5_bio    <-  0.054799
b_sd_asc5_coast  <- -0.232683
b_sd_asc5_lit    <-  0.249701
b_sd_asc5_cost   <-  3.382075
b_sd_clar_fish   <-  0.821826
b_sd_clar_bio    <-  0.186235
b_sd_clar_coast  <- -0.404485
b_sd_clar_lit    <-  0.261125
b_sd_clar_cost   <- -2.505765
b_sd_fish_bio    <- -0.326458
b_sd_fish_coast  <- -0.176690
b_sd_fish_lit    <- -0.028832
b_sd_fish_cost   <- -0.594930
b_sd_bio_coast   <-  0.259238
b_sd_bio_lit     <-  0.006753
b_sd_bio_cost    <- -3.195859
b_sd_coast_lit   <-  0.127707
b_sd_coast_cost  <-  0.175160
b_sd_lit_cost    <-  2.583424


se_mu_asc1        <- 0.38489
se_mu_asc2        <- 0.12746
se_mu_asc3        <- 0.12528
se_mu_asc4        <- 0.12768
se_mu_asc5        <- 0.11594
se_mu_clar        <- 0.04084
se_mu_fish        <- 0.09995
se_mu_bio         <- 0.05964
se_mu_coast       <- 0.08574
se_mu_lit         <- 0.05897
se_mu_cost        <- 0.36185
se_sd_asc1        <- 0.41680
se_sd_asc2        <- 0.24276
se_sd_asc3        <- 0.24295
se_sd_asc4        <- 0.17007
se_sd_asc5        <- 0.12937
se_sd_clar        <- 0.07495
se_sd_fish        <- 0.11087
se_sd_bio         <- 0.08843
se_sd_coast       <- 0.12764
se_sd_lit         <- 0.09602
se_sd_cost        <- 0.07271
se_sd_asc1_asc2   <- 0.20239
se_sd_asc1_asc3   <- 0.24146
se_sd_asc1_asc4   <- 0.19432
se_sd_asc1_asc5   <- 0.14730
se_sd_asc1_clar   <- 0.08183
se_sd_asc1_fish   <- 0.19900
se_sd_asc1_bio    <- 0.06861
se_sd_asc1_coast  <- 0.15394
se_sd_asc1_lit    <- 0.06784
se_sd_asc1_cost   <- 0.14214
se_sd_asc2_asc3   <- 0.26898
se_sd_asc2_asc4   <- 0.19095
se_sd_asc2_asc5   <- 0.14703
se_sd_asc2_clar   <- 0.05433
se_sd_asc2_fish   <- 0.12055
se_sd_asc2_bio    <- 0.05406
se_sd_asc2_coast  <- 0.14601
se_sd_asc2_lit    <- 0.05456
se_sd_asc2_cost   <- 0.19849
se_sd_asc3_asc4   <- 0.14337
se_sd_asc3_asc5   <- 0.10663
se_sd_asc3_clar   <- 0.05083
se_sd_asc3_fish   <- 0.10021
se_sd_asc3_bio    <- 0.04189
se_sd_asc3_coast  <- 0.10423
se_sd_asc3_lit    <- 0.04886
se_sd_asc3_cost   <- 0.07809
se_sd_asc4_asc5   <- 0.17468
se_sd_asc4_clar   <- 0.05277
se_sd_asc4_fish   <- 0.16072
se_sd_asc4_bio    <- 0.05516
se_sd_asc4_coast  <- 0.13816
se_sd_asc4_lit    <- 0.10412
se_sd_asc4_cost   <- 0.06933
se_sd_asc5_clar   <- 0.05540
se_sd_asc5_fish   <- 0.21397
se_sd_asc5_bio    <- 0.11698
se_sd_asc5_coast  <- 0.12782
se_sd_asc5_lit    <- 0.07790
se_sd_asc5_cost   <- 0.19544
se_sd_clar_fish   <- 0.13284
se_sd_clar_bio    <- 0.06966
se_sd_clar_coast  <- 0.17464
se_sd_clar_lit    <- 0.07488
se_sd_clar_cost   <- 0.19817
se_sd_fish_bio    <- 0.04470
se_sd_fish_coast  <- 0.11549
se_sd_fish_lit    <- 0.06119
se_sd_fish_cost   <- 0.05820
se_sd_bio_coast   <- 0.15368
se_sd_bio_lit     <- 0.15371
se_sd_bio_cost    <- 0.32178
se_sd_coast_lit   <- 0.09464
se_sd_coast_cost  <- 0.11428
se_sd_lit_cost    <- 0.21921

n.ite <- 1000

distr_mu_asc1        <-rnorm(n.ite, b_mu_asc1         , se_mu_asc1        )
distr_mu_asc2        <-rnorm(n.ite, b_mu_asc2         , se_mu_asc2        )
distr_mu_asc3        <-rnorm(n.ite, b_mu_asc3         , se_mu_asc3        )
distr_mu_asc4        <-rnorm(n.ite, b_mu_asc4         , se_mu_asc4        )
distr_mu_asc5        <-rnorm(n.ite, b_mu_asc5         , se_mu_asc5        )
distr_mu_clar        <-rnorm(n.ite, b_mu_clar         , se_mu_clar        )
distr_mu_fish        <-rnorm(n.ite, b_mu_fish         , se_mu_fish        )
distr_mu_bio         <-rnorm(n.ite, b_mu_bio          , se_mu_bio         )
distr_mu_coast       <-rnorm(n.ite, b_mu_coast        , se_mu_coast       )
distr_mu_lit         <-rnorm(n.ite, b_mu_lit          , se_mu_lit         )
distr_mu_cost        <-rnorm(n.ite, b_mu_cost         , se_mu_cost        )
distr_sd_asc1        <-rnorm(n.ite, b_sd_asc1         , se_sd_asc1        )
distr_sd_asc2        <-rnorm(n.ite, b_sd_asc2         , se_sd_asc2        )
distr_sd_asc3        <-rnorm(n.ite, b_sd_asc3         , se_sd_asc3        )
distr_sd_asc4        <-rnorm(n.ite, b_sd_asc4         , se_sd_asc4        )
distr_sd_asc5        <-rnorm(n.ite, b_sd_asc5         , se_sd_asc5        )
distr_sd_clar        <-rnorm(n.ite, b_sd_clar         , se_sd_clar        )
distr_sd_fish        <-rnorm(n.ite, b_sd_fish         , se_sd_fish        )
distr_sd_bio         <-rnorm(n.ite, b_sd_bio          , se_sd_bio         )
distr_sd_coast       <-rnorm(n.ite, b_sd_coast        , se_sd_coast       )
distr_sd_lit         <-rnorm(n.ite, b_sd_lit          , se_sd_lit         )
distr_sd_cost        <-rnorm(n.ite, b_sd_cost         , se_sd_cost        )
distr_sd_asc1_asc2   <-rnorm(n.ite, b_sd_asc1_asc2    , se_sd_asc1_asc2   )
distr_sd_asc1_asc3   <-rnorm(n.ite, b_sd_asc1_asc3    , se_sd_asc1_asc3   )
distr_sd_asc1_asc4   <-rnorm(n.ite, b_sd_asc1_asc4    , se_sd_asc1_asc4   )
distr_sd_asc1_asc5   <-rnorm(n.ite, b_sd_asc1_asc5    , se_sd_asc1_asc5   )
distr_sd_asc1_clar   <-rnorm(n.ite, b_sd_asc1_clar    , se_sd_asc1_clar   )
distr_sd_asc1_fish   <-rnorm(n.ite, b_sd_asc1_fish    , se_sd_asc1_fish   )
distr_sd_asc1_bio    <-rnorm(n.ite, b_sd_asc1_bio     , se_sd_asc1_bio    )
distr_sd_asc1_coast  <-rnorm(n.ite, b_sd_asc1_coast   , se_sd_asc1_coast  )
distr_sd_asc1_lit    <-rnorm(n.ite, b_sd_asc1_lit     , se_sd_asc1_lit    )
distr_sd_asc1_cost   <-rnorm(n.ite, b_sd_asc1_cost    , se_sd_asc1_cost   )
distr_sd_asc2_asc3   <-rnorm(n.ite, b_sd_asc2_asc3    , se_sd_asc2_asc3   )
distr_sd_asc2_asc4   <-rnorm(n.ite, b_sd_asc2_asc4    , se_sd_asc2_asc4   )
distr_sd_asc2_asc5   <-rnorm(n.ite, b_sd_asc2_asc5    , se_sd_asc2_asc5   )
distr_sd_asc2_clar   <-rnorm(n.ite, b_sd_asc2_clar    , se_sd_asc2_clar   )
distr_sd_asc2_fish   <-rnorm(n.ite, b_sd_asc2_fish    , se_sd_asc2_fish   )
distr_sd_asc2_bio    <-rnorm(n.ite, b_sd_asc2_bio     , se_sd_asc2_bio    )
distr_sd_asc2_coast  <-rnorm(n.ite, b_sd_asc2_coast   , se_sd_asc2_coast  )
distr_sd_asc2_lit    <-rnorm(n.ite, b_sd_asc2_lit     , se_sd_asc2_lit    )
distr_sd_asc2_cost   <-rnorm(n.ite, b_sd_asc2_cost    , se_sd_asc2_cost   )
distr_sd_asc3_asc4   <-rnorm(n.ite, b_sd_asc3_asc4    , se_sd_asc3_asc4   )
distr_sd_asc3_asc5   <-rnorm(n.ite, b_sd_asc3_asc5    , se_sd_asc3_asc5   )
distr_sd_asc3_clar   <-rnorm(n.ite, b_sd_asc3_clar    , se_sd_asc3_clar   )
distr_sd_asc3_fish   <-rnorm(n.ite, b_sd_asc3_fish    , se_sd_asc3_fish   )
distr_sd_asc3_bio    <-rnorm(n.ite, b_sd_asc3_bio     , se_sd_asc3_bio    )
distr_sd_asc3_coast  <-rnorm(n.ite, b_sd_asc3_coast   , se_sd_asc3_coast  )
distr_sd_asc3_lit    <-rnorm(n.ite, b_sd_asc3_lit     , se_sd_asc3_lit    )
distr_sd_asc3_cost   <-rnorm(n.ite, b_sd_asc3_cost    , se_sd_asc3_cost   )
distr_sd_asc4_asc5   <-rnorm(n.ite, b_sd_asc4_asc5    , se_sd_asc4_asc5   )
distr_sd_asc4_clar   <-rnorm(n.ite, b_sd_asc4_clar    , se_sd_asc4_clar   )
distr_sd_asc4_fish   <-rnorm(n.ite, b_sd_asc4_fish    , se_sd_asc4_fish   )
distr_sd_asc4_bio    <-rnorm(n.ite, b_sd_asc4_bio     , se_sd_asc4_bio    )
distr_sd_asc4_coast  <-rnorm(n.ite, b_sd_asc4_coast   , se_sd_asc4_coast  )
distr_sd_asc4_lit    <-rnorm(n.ite, b_sd_asc4_lit     , se_sd_asc4_lit    )
distr_sd_asc4_cost   <-rnorm(n.ite, b_sd_asc4_cost    , se_sd_asc4_cost   )
distr_sd_asc5_clar   <-rnorm(n.ite, b_sd_asc5_clar    , se_sd_asc5_clar   )
distr_sd_asc5_fish   <-rnorm(n.ite, b_sd_asc5_fish    , se_sd_asc5_fish   )
distr_sd_asc5_bio    <-rnorm(n.ite, b_sd_asc5_bio     , se_sd_asc5_bio    )
distr_sd_asc5_coast  <-rnorm(n.ite, b_sd_asc5_coast   , se_sd_asc5_coast  )
distr_sd_asc5_lit    <-rnorm(n.ite, b_sd_asc5_lit     , se_sd_asc5_lit    )
distr_sd_asc5_cost   <-rnorm(n.ite, b_sd_asc5_cost    , se_sd_asc5_cost   )
distr_sd_clar_fish   <-rnorm(n.ite, b_sd_clar_fish    , se_sd_clar_fish   )
distr_sd_clar_bio    <-rnorm(n.ite, b_sd_clar_bio     , se_sd_clar_bio    )
distr_sd_clar_coast  <-rnorm(n.ite, b_sd_clar_coast   , se_sd_clar_coast  )
distr_sd_clar_lit    <-rnorm(n.ite, b_sd_clar_lit     , se_sd_clar_lit    )
distr_sd_clar_cost   <-rnorm(n.ite, b_sd_clar_cost    , se_sd_clar_cost   )
distr_sd_fish_bio    <-rnorm(n.ite, b_sd_fish_bio     , se_sd_fish_bio    )
distr_sd_fish_coast  <-rnorm(n.ite, b_sd_fish_coast   , se_sd_fish_coast  )
distr_sd_fish_lit    <-rnorm(n.ite, b_sd_fish_lit     , se_sd_fish_lit    )
distr_sd_fish_cost   <-rnorm(n.ite, b_sd_fish_cost    , se_sd_fish_cost   )
distr_sd_bio_coast   <-rnorm(n.ite, b_sd_bio_coast    , se_sd_bio_coast   )
distr_sd_bio_lit     <-rnorm(n.ite, b_sd_bio_lit      , se_sd_bio_lit     )
distr_sd_bio_cost    <-rnorm(n.ite, b_sd_bio_cost     , se_sd_bio_cost    )
distr_sd_coast_lit   <-rnorm(n.ite, b_sd_coast_lit    , se_sd_coast_lit   )
distr_sd_coast_cost  <-rnorm(n.ite, b_sd_coast_cost   , se_sd_coast_cost  )
distr_sd_lit_cost    <-rnorm(n.ite, b_sd_lit_cost     , se_sd_lit_cost    )



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
  
    mu_asc1        <-distr_mu_asc1[i]
    mu_asc2        <-distr_mu_asc2[i]
    mu_asc3        <-distr_mu_asc3[i]
    mu_asc4        <-distr_mu_asc4[i]
    mu_asc5        <-distr_mu_asc5[i]
    mu_clar        <-distr_mu_clar[i]
    mu_fish        <-distr_mu_fish[i]
    mu_bio         <-distr_mu_bio[i]
    mu_coast       <-distr_mu_coast[i]
    mu_lit         <-distr_mu_lit[i]
    mu_cost        <-distr_mu_cost[i]
    sd_asc1        <-distr_sd_asc1[i]
    sd_asc2        <-distr_sd_asc2[i]
    sd_asc3        <-distr_sd_asc3[i]
    sd_asc4        <-distr_sd_asc4[i]
    sd_asc5        <-distr_sd_asc5[i]
    sd_clar        <-distr_sd_clar[i]
    sd_fish        <-distr_sd_fish[i]
    sd_bio         <-distr_sd_bio[i]
    sd_coast       <-distr_sd_coast[i]
    sd_lit         <-distr_sd_lit[i]
    sd_cost        <-distr_sd_cost[i]
    sd_asc1_asc2   <-distr_sd_asc1_asc2[i]
    sd_asc1_asc3   <-distr_sd_asc1_asc3[i]
    sd_asc1_asc4   <-distr_sd_asc1_asc4[i]
    sd_asc1_asc5   <-distr_sd_asc1_asc5[i]
    sd_asc1_clar   <-distr_sd_asc1_clar[i]
    sd_asc1_fish   <-distr_sd_asc1_fish[i]
    sd_asc1_bio    <-distr_sd_asc1_bio[i]
    sd_asc1_coast  <-distr_sd_asc1_coast[i]
    sd_asc1_lit    <-distr_sd_asc1_lit[i]
    sd_asc1_cost   <-distr_sd_asc1_cost[i]
    sd_asc2_asc3   <-distr_sd_asc2_asc3[i]
    sd_asc2_asc4   <-distr_sd_asc2_asc4[i]
    sd_asc2_asc5   <-distr_sd_asc2_asc5[i]
    sd_asc2_clar   <-distr_sd_asc2_clar[i]
    sd_asc2_fish   <-distr_sd_asc2_fish[i]
    sd_asc2_bio    <-distr_sd_asc2_bio[i]
    sd_asc2_coast  <-distr_sd_asc2_coast[i]
    sd_asc2_lit    <-distr_sd_asc2_lit[i]
    sd_asc2_cost   <-distr_sd_asc2_cost[i]
    sd_asc3_asc4   <-distr_sd_asc3_asc4[i]
    sd_asc3_asc5   <-distr_sd_asc3_asc5[i]
    sd_asc3_clar   <-distr_sd_asc3_clar[i]
    sd_asc3_fish   <-distr_sd_asc3_fish[i]
    sd_asc3_bio    <-distr_sd_asc3_bio[i]
    sd_asc3_coast  <-distr_sd_asc3_coast[i]
    sd_asc3_lit    <-distr_sd_asc3_lit[i]
    sd_asc3_cost   <-distr_sd_asc3_cost[i]
    sd_asc4_asc5   <-distr_sd_asc4_asc5[i]
    sd_asc4_clar   <-distr_sd_asc4_clar[i]
    sd_asc4_fish   <-distr_sd_asc4_fish[i]
    sd_asc4_bio    <-distr_sd_asc4_bio[i]
    sd_asc4_coast  <-distr_sd_asc4_coast[i]
    sd_asc4_lit    <-distr_sd_asc4_lit[i]
    sd_asc4_cost   <-distr_sd_asc4_cost[i]
    sd_asc5_clar   <-distr_sd_asc5_clar[i]
    sd_asc5_fish   <-distr_sd_asc5_fish[i]
    sd_asc5_bio    <-distr_sd_asc5_bio[i]
    sd_asc5_coast  <-distr_sd_asc5_coast[i]
    sd_asc5_lit    <-distr_sd_asc5_lit[i]
    sd_asc5_cost   <-distr_sd_asc5_cost[i]
    sd_clar_fish   <-distr_sd_clar_fish[i]
    sd_clar_bio    <-distr_sd_clar_bio[i]
    sd_clar_coast  <-distr_sd_clar_coast[i]
    sd_clar_lit    <-distr_sd_clar_lit[i]
    sd_clar_cost   <-distr_sd_clar_cost[i]
    sd_fish_bio    <-distr_sd_fish_bio[i]
    sd_fish_coast  <-distr_sd_fish_coast[i]
    sd_fish_lit    <-distr_sd_fish_lit[i]
    sd_fish_cost   <-distr_sd_fish_cost[i]
    sd_bio_coast   <-distr_sd_bio_coast[i]
    sd_bio_lit     <-distr_sd_bio_lit[i]
    sd_bio_cost    <-distr_sd_bio_cost[i]
    sd_coast_lit   <-distr_sd_coast_lit[i]
    sd_coast_cost  <-distr_sd_coast_cost[i]
    sd_lit_cost    <-distr_sd_lit_cost[i]


                      choleski.cov <- t(matrix (c(sd_asc1      , 0            , 0           , 0            , 0            , 0            , 0            , 0            , 0            , 0           , 0                        ,
                                                  sd_asc1_asc2 , sd_asc2      , 0           , 0            , 0            , 0            , 0            , 0            , 0            , 0           , 0                        ,
                                                  sd_asc1_asc3 , sd_asc2_asc3 ,sd_asc3      , 0            , 0            , 0            , 0            , 0            , 0            , 0           , 0                        ,
                                                  sd_asc1_asc4 , sd_asc2_asc4 ,sd_asc3_asc4 , sd_asc4      , 0            , 0            , 0            , 0            , 0            , 0           , 0                        ,
                                                  sd_asc1_asc5 , sd_asc2_asc5 ,sd_asc3_asc5 , sd_asc4_asc5 , sd_asc5      , 0            , 0            , 0            , 0            , 0           , 0                        ,
                                                  sd_asc1_clar , sd_asc2_clar ,sd_asc3_clar , sd_asc4_clar , sd_asc5_clar , sd_clar      , 0            , 0            , 0            , 0           , 0                        ,
                                                  sd_asc1_fish , sd_asc2_fish ,sd_asc3_fish , sd_asc4_fish , sd_asc5_fish , sd_clar_fish , sd_fish       , 0           , 0            , 0           , 0                        ,
                                                  sd_asc1_bio  , sd_asc2_bio  ,sd_asc3_bio  , sd_asc4_bio  , sd_asc5_bio  , sd_clar_bio  , sd_fish_bio   , sd_bio      , 0            , 0           , 0                        ,     
                                                  sd_asc1_coast, sd_asc2_coast,sd_asc3_coast, sd_asc4_coast, sd_asc5_coast, sd_clar_coast, sd_fish_coast , sd_bio_coast, sd_coast     , 0           , 0                        ,
                                                  sd_asc1_lit  , sd_asc2_lit  ,sd_asc3_lit  , sd_asc4_lit  , sd_asc5_lit  , sd_clar_lit  , sd_fish_lit   , sd_bio_lit  , sd_coast_lit , sd_lit      , 0                        ,      
                                                  sd_asc1_cost , sd_asc2_cost ,sd_asc3_cost , sd_asc4_cost , sd_asc5_cost , sd_clar_cost , sd_fish_cost  , sd_bio_cost , sd_coast_cost, sd_lit_cost , sd_cost), 11))
                      
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
                                              rnorm(NumIter),
                                              rnorm(NumIter))
                      
                      beta.attr      <- cbind(rep(unname(mu_asc1)  ,NumIter),
                                              rep(unname(mu_asc2)  ,NumIter),
                                              rep(unname(mu_asc3)  ,NumIter),
                                              rep(unname(mu_asc4)  ,NumIter),
                                              rep(unname(mu_asc5)  ,NumIter),
                                              rep(unname(mu_clar)  ,NumIter),
                                              rep(unname(mu_fish)  ,NumIter),
                                              rep(unname(mu_bio)   ,NumIter),
                                              rep(unname(mu_coast) ,NumIter),
                                              rep(unname(mu_lit)   ,NumIter),
                                              rep(unname(mu_cost)  ,NumIter) )
                      
                      random.attr    <- normal.0.1 %*% t(choleski.cov) + beta.attr
                      colnames(random.attr) <- c("mu_asc1","mu_asc2","mu_asc3","mu_asc4","mu_asc5","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")
                      
                      
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


write.table(simu_distr_median_WTP, file="./simu_WTP_6Alt_2026_02_26.csv", sep=" ", row.names = TRUE, col.names=TRUE)
write.table(median_CI_WTP, file="./median_CI_WTP_6Alt_2026_02_26.csv", sep=" ", row.names = TRUE, col.names=TRUE)



