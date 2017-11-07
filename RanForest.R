library(caret)
library(corrplot)
library(plyr)
library(rpart)
library(randomForest)
set.seed(123)
#Train Random Forest
rf_t <-randomForest(slop~.,data=train,keep.forest=FALSE, importance=TRUE,ntree=10000)
print(rf)
# Call:
#   randomForest(formula = ty ~ ., data = train, keep.forest = FALSE,      importance = TRUE, ntree = 1000) 
# Type of random forest: regression
# Number of trees: 1000
# No. of variables tried at each split: 117
# 
# Mean of squared residuals: 1.56867e-08
# % Var explained: 82.12

rf <-randomForest(slop~.,data=df, importance=TRUE,ntree=1000)
print(rf)
# Call:
#   randomForest(formula = slop ~ ., data = df, keep.forest = FALSE,      importance = TRUE, ntree = 1000) 
# Type of random forest: regression
# Number of trees: 1000
# No. of variables tried at each split: 117
# 
# Mean of squared residuals: 7.599835e-08
# % Var explained: 9.45
#Evaluate variable importance
mse_rf <- mean((ty - rf$predicted  )^2)
mse_rf
imp<-rf$importance    #worked
importance(rf, type =1 )
imp = importance(rf, type =1)
imp <- data.frame(predictors=rownames(imp),imp)
rf$importanceSD
rf %>% summary
rf$mse
# Order the predictor levels by importance 
# I guess %IncMSE of j'th is (mse(j)-mse0)/mse0 * 100%   so the higher number more importent

imp.sort <- arrange(imp,desc(imp$X.IncMSE))
imp.sort
imp.sort$predictors <- factor(imp.sort$predictors,levels=imp.sort$predictors)
imp.sort$predictors
# Select the top 20 predictors
imp.20<- imp.sort[1:20,]
print(imp.20)

# predictors     X.IncMSE IncNodePurity
# 1             frlt_sfbcmin_er3_pcs_ 1.837079e-09  3.518822e-07
# 2                         vx4_t_mv_ 1.427244e-09  2.777758e-07
# 3            ron_odt_33_pn_rate_mv_ 1.142765e-09  2.365935e-07
# 4       sfbc_b32wlsalp_dc_post_pcs_ 1.128754e-09  2.140324e-07
# 5               vpgms_sgdprog_t_mv_ 1.094796e-09  2.374922e-07
# 6  sfbc_t32wlsalp_far_dc_fresh_pcs_ 1.001499e-09  3.296777e-07
# 7              crd_blopen1slc1_b_p_ 9.233554e-10  1.997785e-07
# 8                        vref_t_mv_ 8.053229e-10  2.170027e-07
# 9                wlleak_post_15_na_ 7.419665e-10  6.922053e-08
# 10                   sfbc_drtime_s_ 7.219311e-10  1.195924e-07
# 11       sfbc_t32wlsaerx_fresh_pcs_ 4.726002e-10  9.210109e-08
# 12                    wlrc_240_pcs_ 4.580388e-10  1.271446e-07
# 13                  vth_sgs_l3s_mv_ 3.622792e-10  7.500148e-08
# 14               wlleak_post_62_na_ 3.580425e-10  4.363612e-08
# 15         wlleak_each_value_12_na_ 3.474335e-10  6.168720e-08
# 16                  vth_sgd_u3s_mv_ 3.441663e-10  8.244018e-08
# 17         wlleak_each_value_18_na_ 3.316492e-10  5.179436e-08
# 18                 halfvccq33_t_mv_ 3.294085e-10  8.233673e-08
# 19       sfbc_b32wlsaup_drpost_pcs_ 3.212950e-10  2.306869e-07
# 20               wlleak_post_19_na_ 3.147903e-10  7.117619e-08

# Plot Important Variables
varImpPlot(rf, type=1, main = "Random Forest var importance")

# Subset data with 20 independent and 1 dependent variables
dat4 = cbind(classe = dat3$classe, dat3[,c(imp.20$predictors)])
dat4

imp.22<- imp.sort[1:22,]
print(imp.22)

#retreave the particular names of the variable from DS and fit the linear model
imp.22[,1]
f_i <- c("frlt_sfbcmin_er3_pcs_" ,           "vx4_t_mv_"  ,                      "ron_odt_33_pn_rate_mv_"  ,         "sfbc_b32wlsalp_dc_post_pcs_",     
         "vpgms_sgdprog_t_mv_",              "sfbc_t32wlsalp_far_dc_fresh_pcs_",  "crd_blopen1slc1_b_p_",             "vref_t_mv_"       ,               
         "wlleak_post_15_na_",               "sfbc_drtime_s_",                   "sfbc_t32wlsaerx_fresh_pcs_",        "wlrc_240_pcs_"  ,                 
         "vth_sgs_l3s_mv_",                  "wlleak_post_62_na_",               "wlleak_each_value_12_na_",         "vth_sgd_u3s_mv_",                 
         "wlleak_each_value_18_na_",         "halfvccq33_t_mv_",                 "sfbc_b32wlsaup_drpost_pcs_",       "wlleak_post_19_na_",              
         "iccs_moni_1_ua_",                  "vpass2_t_mv_" ) 

f_i
f_i %>% str
# subset of all data with selected  21 veriables
sub_rf <-dfj%>% select(f_i)
sub_rf %>% dim
sub_rf %>% names
sub_rf
# add slop 
sub_rf$slop <-dfj$slop

# now ready to fit linear model with this subset
lmod_rf <- lm(slop~., sub_rf)
lmod_rf

summary (lmod_rf)
summary_rf <-summary(lmod_rf)
summary_rf