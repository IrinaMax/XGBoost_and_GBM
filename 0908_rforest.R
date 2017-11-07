library(data.table)
library(dplyr)
library(lars)
library(doMC)
devtools::install_github('mlampros/FeatureSelection', force = TRUE)
library(FeatureSelection)


df_m<- read.csv("f_model_htpd.csv", header = TRUE, sep = ",", na.strings = c("NULL", "NaN", "NA"), stringsAsFactors = TRUE)
df_m %>% names
library(data.table)
library(dplyr)
df <- df_m[,-c(1)]
df %>% dim
#[1] 151 353     must be


# ## model 1-----------------------  CV with Lambda min (LASSO) alpha by default 1--------------------------------

set.seed(1)
x <- model.matrix(slop~. , -353, data=df)
x[, 350:353]
df[, 350:353]
y <- as.matrix(df[, 353]) # Only slop
y %>% dim
cv = cv.glmnet(x, y)
cv
cv %>% names
cv$lambda.min
#[1] 3.232086e-05
model = glmnet(x, y, type.gaussian="covariance",  lambda=cv$lambda.min)
model
# Call:  glmnet(x = x, y = y, lambda = cv$lambda.min, type.gaussian = "covariance") 
# 
# Df   %Dev    Lambda
# [1,] 21 0.3334 3.232e-05
model %>% names

# Prediction with Lasso CV pred1------------------------------------------------------------------
pred1 <- predict(model, type="coefficients")
pred1
plot(pred1,y)
plot(pred1, y,  xlab = "prediction", ylab = "SLOPE",col = "dark red", main = "expectation on model pred1, lambda min= 3.232e-05") 
abline(lm(y ~ pred1, data=df))
# plot prediction as a slop for the 151 dies
abline(lm(y ~ pred1 + 0),  col="red")
# summarize accuracy
mse_lasso <- mean((y - pred1)^2)
print(mse_lasso)
[1] 5.594663e-08

# I want to make the result table with coloms
temp <- pred1%>% summary
temp$i
#  [1]   1  10  14  18  23  26  38  75  79  97 171 186 192 195 199 201 224 237 298 307 321 352
pr <- as.matrix(pred1)
pr
res <- data.frame(which( !pr ==0, arr.ind = T))
res$col<- NULL
res
res$sl_coef <- pr[which( !pr ==0, arr.ind = T)]
res %>% summary
res
write.csv(res, "result_of_CV_prediction1.csv")


# LINEAR MODEL WITH CV LASSO 1-------------------------------------------------------------------------
# try to make subset of variables picked by Lasso for linear model
# vector of columns you DON'T want
res_n <- read.csv("result_of_CV_prediction1.csv" )[,1]
res_n
f <- c("div2res_s_pcs_" ,                  
       "vref_t_mv_",                        "vdda_s_dac_",                      
       "sde_pmp_um_s_dac_",                "vx4_t_mv_",                        
       "vcg_slcr_s_dac_",                   "halfvccq18_t_mv_",                 
       "vpgms_sgdprog_t_mv_",               "vth_wldd01_l3s_mv_",               
       "vcelsrct_p_",                       "bit_mhopen_io3_pcs_",              
       "bbk_mhopen1_001_vblc015_ev_c_pcs_", "crd_blopen1slc1_b_p_",             
       "layer1_vpgms_s_dac_",               "layer4_vpgms_s_dac_",              
       "wlleak_post_ds0_na_",               "wlleak_post_12_na_",               
       "wlrc_240_pcs_",                     "frlt_sfbcmin_er3_pcs_",            
       "sfbc_drtime_s_",                    "sfbc_t32wlsalp_far_dc_fresh_pcs_") 

f %>% str
# subset of all data with selected  21 veriables
ss <-dfj%>% select(f)
ss %>% dim
ss %>% names
ss
# add slop 
dfj$slop
ss$slop <-dfj$slop
write.csv(ss, "Lasso_var_subset.csv")

# prediction 2 with type "responce"
pred2 <- predict(model,x, s=,type="response")
# plot prediction as a slop for the 151 dies
plot(pred2,y)
plot(pred2, y,  xlab = "prediction", ylab = "SLOP",col = "dark red", main = "expectation on model pred2, lambda min= 5.889913e-05") 
abline(lm(y ~ pred2, data=df))
             # plot prediction as a slop for the 151 dies
     abline(lm(y ~ pred2 + 0),  col="blue")
     
# CV 5 fold with type performs MSE ("Mean-Squared Error")
glmnet1<-cv.glmnet(x=x,y=y,type.measure='mse',nfolds=5,alpha=.5)
glmnet1 %>% names   # give lambda even more higher 
glmnet1$lambda.min
# [1] 7.786118e-05
pred3 <- predict(glmnet1, type="coefficient")

pred3 <- predict(glmnet1,x, s=7.786118e-05,type="response")
plot(pred3, y,  xlab = "prediction", ylab = "SLOP",col = "dark red", main = "expectation on model pred3 with mse, lambda min= 7.786118e-05")      
# plot prediction as a slop for the 151 dies
abline(lm(y ~ pred3, data=df))
# plot prediction as a slop for the 151 dies
abline(lm(y ~ pred3 + 0),  col="green")

pred4 <- predict(glmnet1,x, s=glmnet1$lambda.min, type="response")
plot(pred4, y,  xlab = "prediction", ylab = "SLOP",col = "dark red", main = "expectation on model pred 4 with mse, glmnet1$lambda.min,type responce") 
abline(lm(y ~ pred4, data=df))
# plot prediction as a slop for the 151 dies
abline(lm(y ~ pred4 + 0),  col="yellow")


#   ----------   try Elastic Net
# library(glmnet)
# # load data
# 
# # fit model
# el_net <- glmnet(x, y, family="gaussian", alpha=0.5, lambda=0.001)
# 
# # summarize the fit
# summary(el_net)
# # make predictions
# pred5 <- predict(el_net, x, type= "coefficient")
# plot(pred5, y,  xlab = "prediction", ylab = "SLOP",col = "dark green", main = "expectation on model pred  with mse, Elestic Net") 
# abline(lm(y ~ pred5, data=df))
# # plot prediction as a slop for the 151 dies
# abline(lm(y ~ pred5 + 0),  col="yellow")
# # summarize accuracy
# mse <- mean((y - pred5)^2)
# print(mse)
# #[1] 8.393186e-08

#   ------------------------------------Random Forest --------------
library(caret)
library(corrplot)
library(plyr)
library(rpart)
library(randomForest)
set.seed(123)
#Train Random Forest
rf <-randomForest(ty~.,data=train,keep.forest=FALSE, importance=TRUE,ntree=10000)
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
# Call:
#   lm(formula = slop ~ ., data = sub_rf)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -6.774e-04 -1.413e-04 -1.328e-05  1.596e-04  7.010e-04 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                       2.429e-02  1.619e-02   1.500  0.13604   
# frlt_sfbcmin_er3_pcs_            -4.133e-07  1.843e-07  -2.243  0.02662 * 
#   vx4_t_mv_                         1.100e-06  4.072e-07   2.702  0.00784 **
#   ron_odt_33_pn_rate_mv_           -3.747e-04  4.585e-04  -0.817  0.41524   
# sfbc_b32wlsalp_dc_post_pcs_      -2.671e-08  5.239e-08  -0.510  0.61106   
# vpgms_sgdprog_t_mv_              -1.414e-06  7.526e-07  -1.879  0.06255 . 
# sfbc_t32wlsalp_far_dc_fresh_pcs_ -5.362e-08  4.468e-08  -1.200  0.23234   
# crd_blopen1slc1_b_p_             -3.708e-05  1.363e-05  -2.721  0.00741 **
#   vref_t_mv_                        2.447e-06  7.423e-06   0.330  0.74223   
# wlleak_post_15_na_               -4.634e-07  1.875e-06  -0.247  0.80518   
# sfbc_drtime_s_                   -1.080e-06  4.289e-07  -2.517  0.01307 * 
#   sfbc_t32wlsaerx_fresh_pcs_        6.080e-08  1.768e-07   0.344  0.73149   
# wlrc_240_pcs_                     1.258e-09  8.407e-10   1.496  0.13704   
# vth_sgs_l3s_mv_                   2.711e-07  2.174e-07   1.247  0.21475   
# wlleak_post_62_na_               -2.934e-06  1.833e-06  -1.601  0.11190   
# wlleak_each_value_12_na_          1.424e-06  1.860e-06   0.766  0.44515   
# vth_sgd_u3s_mv_                   3.751e-08  1.995e-07   0.188  0.85113   
# wlleak_each_value_18_na_          1.878e-06  1.907e-06   0.985  0.32648   
# halfvccq33_t_mv_                 -1.023e-05  7.127e-06  -1.435  0.15376   
# sfbc_b32wlsaup_drpost_pcs_        1.063e-07  3.460e-08   3.072  0.00260 **
#   wlleak_post_19_na_                3.750e-06  1.995e-06   1.879  0.06248 . 
# iccs_moni_1_ua_                   6.134e-07  1.351e-06   0.454  0.65043   
# vpass2_t_mv_                     -1.918e-07  3.178e-07  -0.604  0.54724   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.0002433 on 128 degrees of freedom
# Multiple R-squared:  0.4022,	Adjusted R-squared:  0.2994 
# F-statistic: 3.914 on 22 and 128 DF,  p-value: 5.131e-07
# plot for random forest subset
plot(lmod_rf, main="Random Forest") 
print(summary_rf, useSource = T)
summary_rf %>% str %>% as.matrix
# Copy of the console was safed in Excel as "Com_result."

#   ----------------------------------END of RANDOM FOREST --------------------------


#  -----------try XGBoost and ranger--------------
devtools::install_version("xgboost", version = "0.4-4", repos = "http://cran.us.r-project.org")
install.packages("ranger")
library(xgboost)
library(ranger)

dfj <- df
 X_n = df[, -353]
 y_n = df[, 353]

  ## 75% of the sample size  or may be 80%  ???
 set.seed(123)
 smp_size <- floor(0.90 * nrow(dfj))
 
 train_ind <- sample(seq_len(nrow(dfj)), size = smp_size, replace = FALSE  )
 
 train <- dfj[train_ind, ]
 test <- dfj[-train_ind, ]
 
 #TRAIN
 train %>% dim
 tx <- train[,-c(353)]
 #[1] 135 353
 names(train)
 tx <- model.matrix(slop~. , -1, data=train )
 ty <- as.matrix(train[, 353]) # Only slop
 ty %>% dim
 
 # TEST
 test %>% dim
 #[1]  16 353
 names(test)
 y_st <- test[,353]
 y_st %>% summary  # we need to to campare result
 test <- model.matrix(slop~. , -353, data=test )
 test <- test[, -c(353)]
 test
 
 #X_train <- cbind(X_train, MIR_DER[, -1])
 p = df[, 'slop']
 ########
 params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 5, parallel = TRUE)
 
 
 params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.001, "subsample" = 0.75, "max_depth" = 5,
                                      
                                      "colsample_bytree" = 0.75, "nthread" = 6),
                        
                        nrounds = 1000, print.every.n = 250, maximize = FALSE)
 
 
 params_ranger = list(dependent.variable.name = 'y', probability = FALSE, num.trees = 1000, verbose = TRUE, mtry = 5, 
                      
                      min.node.size = 10, num.threads = 6, classification = FALSE, importance = 'permutation')
 
 
 params_features = list(keep_number_feat = NULL, union = TRUE)
 
 
 feat <- wrapper_feat_select(X = df, y = ty, params_glmnet = params_glmnet, params_xgboost = params_xgboost, 
                            
                            params_ranger = params_ranger, xgb_sort = 'Gain', CV_folds = 5, stratified_regr = FALSE, 
                            
                            scale_coefs_glmnet = FALSE, cores_glmnet = 5, params_features = params_features, verbose = TRUE)
##  run all
str(feat)
params_barplot = list(keep_features = 30, horiz = TRUE, cex.names = 1.0)

barplot_feat_select(feat, params_barplot, xgb_sort = 'Cover')
  params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 3, parallel = TRUE)
 res_n = feature_selection(X_n, y_n, method = 'glmnet-lasso', params_glmnet = params_glmnet, CV_folds = 5, cores_glmnet = 5)

# binary classification

# data(iris)
# y = iris[, 5]
# y = as.character(y)
 y[y == 'slop'] = 'slop'
# X = iris[, -5]

 params_ranger = list(write.forest = TRUE, probability = TRUE, num.threads = 6, num.trees = 50, verbose = FALSE, classification = TRUE, mtry = 2, min.node.size = 5, importance = 'impurity')

res_s = feature_selection(tx, ty, method = 'ranger', params_ranger = params_ranger, CV_folds = 5)


# multiclass classification


# data(iris)
# y = iris[, 5]
y[y == 'slop'] = 'slop'
tx <- train[,-c(353)]
 multiclass_xgboost = ifelse(y == 'slop', 0, ifelse(y == 'slop', 1, 2))
# X = iris[, -5]

params_xgboost = list( params = list("objective" = "multi:softprob", "bst:eta" = 0.35, "subsample" = 0.65, "num_class" = 3, "max_depth" = 6, "colsample_bytree" = 0.65, "nthread" = 2),
                       nrounds = 50, print.every.n = 50, verbose = 0, maximize = FALSE)

res_xgboost = feature_selection(xt, multiclass_xgboost, method = 'xgboost', params_xgboost = params_xgboost, CV_folds = 5)



