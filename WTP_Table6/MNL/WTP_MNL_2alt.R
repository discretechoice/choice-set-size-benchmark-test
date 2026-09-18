####################################################################
####################################################################
#####
#####            WTP calculations
#####            MNL - 2 alternatives - treatment 1
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
       
b_asc1    <-  0.776581   
b_clar    <-  0.297951   
b_fish    <-  0.498575   
b_bio     <-  0.267840   
b_coast   <-  0.187876   
b_lit     <-  0.425977   
b_cost    <- -0.006938   

se_asc1   <-    0.13302   
se_clar   <-    0.03466 
se_fish   <-    0.09686 
se_bio    <-    0.03603 
se_coast  <-    0.06402 
se_lit    <-    0.04133 
se_cost   <- 6.3462e-04 


NumIter    <-10000

# ################################################################# #
#  Simulate parameter values and WTP
# ################################################################# #

random.attr <- cbind(rnorm(NumIter, b_asc1 , se_asc1 ),
                     rnorm(NumIter, b_clar , se_clar ),
                     rnorm(NumIter, b_fish , se_fish ),
                     rnorm(NumIter, b_bio  , se_bio  ),
                     rnorm(NumIter, b_coast, se_coast),
                     rnorm(NumIter, b_lit  , se_lit  ),
                     rnorm(NumIter, b_cost , se_cost ))

colnames(random.attr) <- c("mu_asc1","mu_clar","mu_fish","mu_bio","mu_coast","mu_lit","mu_cost")

MNL.WTP.clar  <-   (- ( random.attr[,"mu_clar"]  / random.attr[,"mu_cost"]))
MNL.WTP.fish  <-   (- ( random.attr[,"mu_fish"]  / random.attr[,"mu_cost"]))
MNL.WTP.bio   <-   (- ( random.attr[,"mu_bio"]   / random.attr[,"mu_cost"]))
MNL.WTP.coast <-   (- ( random.attr[,"mu_coast"] / random.attr[,"mu_cost"]))
MNL.WTP.lit   <-   (- ( random.attr[,"mu_lit"]   / random.attr[,"mu_cost"]))


# ################################################################# #
#  Save results
# ################################################################# #

simu.WTP.Results <- cbind(MNL.WTP.clar ,
                          MNL.WTP.fish ,
                          MNL.WTP.bio  ,
                          MNL.WTP.coast,
                          MNL.WTP.lit    )


median_CI_WTP <- rbind(c(median(MNL.WTP.clar ), quantile(MNL.WTP.clar , 0.025), quantile(MNL.WTP.clar , 0.975)),
                       c(median(MNL.WTP.fish ), quantile(MNL.WTP.fish , 0.025), quantile(MNL.WTP.fish , 0.975)),
                       c(median(MNL.WTP.bio  ), quantile(MNL.WTP.bio  , 0.025), quantile(MNL.WTP.bio  , 0.975)),
                       c(median(MNL.WTP.coast), quantile(MNL.WTP.coast, 0.025), quantile(MNL.WTP.coast, 0.975)),
                       c(median(MNL.WTP.lit  ), quantile(MNL.WTP.lit  , 0.025), quantile(MNL.WTP.lit  , 0.975)))

row.names(median_CI_WTP) <- c("mu_clar","mu_fish","mu_bio","mu_coast","mu_lit")
colnames(median_CI_WTP)  <- c("median", "CI_0.025", "CI_0.975")



write.table(simu.WTP.Results, file="./simu_WTP_2Alt.csv", sep=" ", row.names = TRUE, col.names=TRUE)
write.table(median_CI_WTP, file="./median_CI_WTP_2Alt.csv", sep=" ", row.names = TRUE, col.names=TRUE)


