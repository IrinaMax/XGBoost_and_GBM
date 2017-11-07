# This code is my work after: 1.work with DQ to find slop, 2. merging DS and DQ with slop on th HIVE, 
# 3.Tuning parametrs and cleaning data after merging
# file htpd_dsslop_clean.csv
library(data.table)
library(dplyr)
library(glmnet)
# wor with "htpd_dsslop_clean.csv"
df_m<- read.csv("htpd_dsslop_clean.csv", header = TRUE, sep = ",", na.strings = c("NULL", "NaN", "NA"), stringsAsFactors = TRUE)
#  0830 update with more elemenatiing columns "avro_file_name$"
#
df <- df_m
is.data.table(df)
df <- data.table(df)
df %>% colnames
df %>% dim
drop.cols <- grep("avro_file_name$", colnames(df))
drop.cols
df[, (drop.cols) := NULL]
dfj <- na.omit(df)
#mydata[!complete.cases(mydata),]
row.has.na <- apply(df, 1, function(x){any(is.na(x))})
row.has.na    ##  row 35 was been removed

dfj <- dfj[,-c(1)]
dfj %>% dim
#[1] 151 353
write.csv(dfj, "f_model_htpd.csv")

# model with CV
set.seed(123)
x <- model.matrix(slop~. , -353, data=dfj )
x[, 350:353]
dfj[, 350:353]
y <- as.matrix(dfj[, 353]) # Only slop
y %>% dim
cv = cv.glmnet(x, y)
cv
plot(cv)
cv %>% names
cv$lambda.min
#[1] 3.385984e-05
model = glmnet(x, y, type.gaussian="covariance",  lambda=cv$lambda.min)
model_s = glmnet(x, y, type.gaussian="covariance",  lambda=cv$lambda.min, standardize = TRUE, standardize.response = TRUE )
model_s
plot(model, xvar = "lambda", label = TRUE)
plot(model_s, xvar = "dev", label = TRUE)

summary(model)
# Call:  glmnet(x = x, y = y, lambda = cv$lambda.min, type.gaussian = "covariance") 
# 
# Df   %Dev    Lambda
# [1,] 21 0.3177 3.386e-05
model %>% names
model$lambda
# [1] 3.385984e-05
pred1 <- predict(model_s, type="coefficients")
pred1
pred<-predict(model_s, newx = x[1:15,], s = "lambda.min")
pred
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
##-----------------------------------------------------------------------------------------------------------------------------------
ln <- lm(sl_coef~. , res)
ln %>% summary
# Call:
#   lm(formula = sl_coef ~ ., data = res)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -0.0069605 -0.0000032  0.0002146  0.0008053  0.0009477 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) -9.851e-04  5.890e-04  -1.673    0.110
# row          4.231e-06  3.170e-06   1.335    0.197
# 
# Residual standard error: 0.001663 on 20 degrees of freedom
# Multiple R-squared:  0.0818,	Adjusted R-squared:  0.03589 
# F-statistic: 1.782 on 1 and 20 DF,  p-value: 0.1969

pred1 <- predict(model,x, s=3.385984e-05,type="response")
plot(pred1, y,  xlab = "prediction", ylab = "SLOP",col = "dark red", main = "expectation on model 1, lambda min= 3.232e-05", abline(pred1, col = "blue"))             # plot prediction as a slop for the 151 dies
abline(pred1, col = "blue")
# CV 5 fold with type performs MSE ("Mean-Squared Error")
glmnet1<-cv.glmnet(x=x,y=y,type.measure='mse',nfolds=5,alpha=.5)
glmnet1 %>% names   # give lambda even more higher 
glmnet1$lambda.min
# [1] 7.786118e-05
pred2 <- predict(glmnet1, type="coefficient")

pred2 <- predict(glmnet1,x, s=3.232086e-05,type="response")
plot(pred2, y,  xlab = "prediction", ylab = "SLOP",col = "dark red", main = "expectation on model 2 with mse, lambda min= 3.232086e-05")      
# plot prediction as a slop for the 151 dies

pred2 <- predict(glmnet1,x, s=glmnet1$lambda.min, type="response")
plot(pred2, y,  xlab = "prediction", ylab = "SLOP",col = "dark red", main = "expectation on model 2 with mse, lambda min= 6.464172e-05") 


# # Fitting the CV model  ( Alpha = 1)  type.misure 'mse'

# CV 10 fold
glmnet2<-cv.glmnet(x=x,y=y,  type.measure='mse',nfolds=10,alpha=1)
glmnet2 %>% names 
#[1] "lambda"     "cvm"        "cvsd"       "cvup"       "cvlo"       "nzero"      "name"       "glmnet.fit" "lambda.min" "lambda.1se"
glmnet2$lambda.min  
#[1] 3.232086e-05
glmnet2$lambda.1se
#[1] 6.494014e-05

# extract coefficient with min lambda and names of dlmnet2 result
c<-coef(glmnet2, s='lambda.min',exact=TRUE) 
c
coef_lambda <- coef(glmnet2, s='lambda.min',exact=TRUE) [which(coef(glmnet2, s = "lambda.min") != 0)]
colnames <- colnames(dfj)[which(coef(glmnet2, s = "lambda.min") != 0)]
colnames
##  Updated frame of coeff with names of the variable
tmp_coeffs <- coef(glmnet2, s = "lambda.min")
glmnet2_name_coeff<- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
glmnet2_name_coeff
write.csv(glmnet2_name_coeff, "glmnet2_coeff.csv")

# Fitting the model (Ridge: Alpha = 0)
set.seed(999)
str(y)
cv.ridge <- cv.glmnet(x=x,y=y,nfolds=10, alpha=0, parallel=TRUE, standardize=TRUE, type.measure='mse')
cv.ridge
plot(cv.ridge)
cv.ridge$lambda.min
# [1] 0.006056339
cv.ridge$lambda.1se
#[1] 0.08194516
coeff_ridge <- coef(cv.ridge, s=cv.ridge$lambda.min,exact=TRUE) [which(coef(cv.ridge, s = "lambda.min") != 0)]
colnames_ridge <- colnames(dfj)[which(coef(cv.ridge, s = "lambda.min") != 0)]
colnames_ridge
##  Updated frame of coeff with names of the variable
ridge_coeffs <- coef(cv.ridge, s = "lambda.min")
ridge_coeffs
ridge_name_coeff<- data.frame(name = ridge_coeffs@Dimnames[[1]][ridge_coeffs@i + 1], coefficient = ridge_coeffs@x)
# I will sort all data by coefficient
ridge_name_coeff_sort <- ridge_name_coeff[order(ridge_name_coeff$coefficient), ]
ridge_name_coeff_sort
write.csv(ridge_name_coeff, "ridge_name_coeff.csv")
lin_mod_ridge <- lm(ridge_name_coeff_sort$coefficient~., ridge_name_coeff_sort)
lin_mod_ridge %>% summary
plot(lin_mod_ridge)


##----------------LASSO 2
cv.lasso <- cv.glmnet(x, y, nfold=10, alpha=1, parallel=TRUE, standardize=TRUE, type.measure='mse')
cv.lasso
# Results
plot(cv.lasso)

cv.lasso$lambda.min
#[1] 3.232086e-05
cv.lasso$lambda.1se
#[1] 5.391445e-05

coeff_lasso <- coef(cv.lasso, s=cv.lasso$lambda.min,exact=TRUE) [which(coef(cv.lasso, s = "lambda.min") != 0)]

coeff_lasso
#extract coefficient with min lambda and names of LASSO result
# c<-coef(glmnet2, s='lambda.min',exact=TRUE) 
# c
# coef_l <- coef(glmnet2, s='lambda.min',exact=TRUE) [which(coef(glmnet2, s = "lambda.min") != 0)]
colnames <- colnames(dfj)[which(coef(cv.lasso, s = "lambda.min") != 0)]
colnames
##  Updated frame of coeff with names of the variable
l_coeffs <- coef(cv.lasso, s = "lambda.min")
l_name_coeff<- data.frame(name = l_coeffs@Dimnames[[1]][l_coeffs@i + 1], coefficient = l_coeffs@x)
l_name_coeff

write.csv(l_name_coeff, "lasso_name_coeff.csv")
as.data.frame(l_name_coeff)
histogram(l_name_coeff)

# try to make subset of variables picked by Lasso
# vector of columns you DON'T want
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
lmod_ss <- lm(slop~., ss)
lmod_ss
lmod_summary <-as.data.frame(lmod_ss %>% summary)
lmod_summary
# Call:
#   lm(formula = slop ~ ., data = ss)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -6.678e-04 -1.327e-04 -1.389e-05  1.403e-04  4.586e-04 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       -1.860e-02  1.188e-02  -1.565 0.120052    
# div2res_s_pcs_                     3.793e-05  2.896e-05   1.310 0.192636    
# vref_t_mv_                        -2.037e-06  6.830e-06  -0.298 0.766039    
# vdda_s_dac_                       -5.544e-05  3.890e-05  -1.425 0.156601    
# sde_pmp_um_s_dac_                  4.648e-05  2.967e-05   1.566 0.119686    
# vx4_t_mv_                          1.137e-06  3.701e-07   3.071 0.002605 ** 
#   vcg_slcr_s_dac_                    1.573e-04  9.159e-05   1.717 0.088316 .  
# halfvccq18_t_mv_                   4.749e-06  2.455e-06   1.934 0.055245 .  
# vpgms_sgdprog_t_mv_               -1.152e-07  6.954e-07  -0.166 0.868727    
# vth_wldd01_l3s_mv_                 1.069e-06  6.823e-07   1.566 0.119760    
# vcelsrct_p_                        3.320e-06  2.973e-06   1.117 0.266081    
# bit_mhopen_io3_pcs_                1.399e-04  4.437e-05   3.153 0.002012 ** 
#   bbk_mhopen1_001_vblc015_ev_c_pcs_  5.271e-06  2.761e-06   1.909 0.058499 .  
# crd_blopen1slc1_b_p_              -5.794e-05  1.306e-05  -4.437 1.94e-05 ***
#   layer1_vpgms_s_dac_               -1.104e-05  2.302e-05  -0.480 0.632351    
# layer4_vpgms_s_dac_               -1.134e-05  9.859e-06  -1.150 0.252385    
# wlleak_post_ds0_na_                2.355e-06  1.482e-06   1.589 0.114408    
# wlleak_post_12_na_                 1.389e-06  1.463e-06   0.949 0.344393    
# wlrc_240_pcs_                      4.889e-10  8.331e-10   0.587 0.558361    
# frlt_sfbcmin_er3_pcs_             -4.095e-07  1.144e-07  -3.581 0.000484 ***
#   sfbc_drtime_s_                    -2.128e-07  3.749e-07  -0.568 0.571319    
# sfbc_t32wlsalp_far_dc_fresh_pcs_  -1.511e-08  2.956e-08  -0.511 0.610088    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.0002227 on 129 degrees of freedom
# Multiple R-squared:  0.4951,	Adjusted R-squared:  0.413 
# F-statistic: 6.025 on 21 and 129 DF,  p-value: 3.008e-11
plot(lmod_ss)

#   END----------------------------------------------


# my work with bbk subset 
library(dplyr)
install.packages("corrplot")
library(corrplot)
dfj %>% names
bbk <-  grep("bbk_", names(dfj), value=TRUE)
bbk 
bbk <- dfj%>% select(bbk)
bbk%>% dim
bbk %>% names
cor_bbk <- cor(bbk)
cor_bbk <- cor.mtest(bbk)

cor_bbk
corrplot(cor_bbk, method="pie")
corrplot(cor_bbk, type="upper")
corrplot(cor_bbk, type="lower")
corrplot(cor_bbk, type="upper", order="hclust", col=c("black", "white"),
         bg="lightblue",sig.level = 0.01)
corrplot(cor_bbk, method="pie",type="upper", order="hclust", 
         sig.level = 0.01)

# work with wlleak variables
dfj %>% names
wll <-  grep("wlleak", names(dfj), value=TRUE)
wll 
wll <- dfj%>% select(wll)
wll%>% dim   # 122
wll %>% names
cor_w <- cor(wll)
cor_w<- cor.mtest(wll)

cor_w <-cor_wll*zoom
corrplot(cor_w, method="pie")
corrplot(cor_w, type="upper")
corrplot(cor_w, type="lower")
corrplot(cor_w, type="upper", order="hclust", col=c("black", "white"),
         bg="lightblue",sig.level = 0.01)
corrplot(cor_w, method="pie",type="upper", order="hclust", 
         sig.level = 0.01)


#Factor analisys and PCA for Lasso subset
fac <- factanal(ss[, -22], 2, scores="regression",rotation = "promax")
fac
# Call:
#   factanal(x = ss[, -22], factors = 2, scores = "regression", rotation = "promax")
# 
# Uniquenesses:
#   div2res_s_pcs_                        vref_t_mv_                       vdda_s_dac_                 sde_pmp_um_s_dac_ 
# 0.988                             0.986                             0.999                             0.740 
# vx4_t_mv_                   vcg_slcr_s_dac_                  halfvccq18_t_mv_               vpgms_sgdprog_t_mv_ 
# 0.998                             0.994                             0.995                             0.964 
# vth_wldd01_l3s_mv_                       vcelsrct_p_               bit_mhopen_io3_pcs_ bbk_mhopen1_001_vblc015_ev_c_pcs_ 
# 0.546                             0.980                             0.990                             0.979 
# crd_blopen1slc1_b_p_               layer1_vpgms_s_dac_               layer4_vpgms_s_dac_               wlleak_post_ds0_na_ 
# 0.739                             0.996                             0.958                             0.556 
# wlleak_post_12_na_                     wlrc_240_pcs_             frlt_sfbcmin_er3_pcs_                    sfbc_drtime_s_ 
# 0.005                             0.999                             0.827                             0.940 
# sfbc_t32wlsalp_far_dc_fresh_pcs_ 
# 0.730 
# 
# Loadings:
#   Factor1 Factor2
# div2res_s_pcs_                                   
# vref_t_mv_                        -0.119         
# vdda_s_dac_                                      
# sde_pmp_um_s_dac_                  0.503         
# vx4_t_mv_                                        
# vcg_slcr_s_dac_                                  
# halfvccq18_t_mv_                                 
# vpgms_sgdprog_t_mv_                       -0.178 
# vth_wldd01_l3s_mv_                 0.664         
# vcelsrct_p_                        0.126         
# bit_mhopen_io3_pcs_                              
# bbk_mhopen1_001_vblc015_ev_c_pcs_  0.140         
# crd_blopen1slc1_b_p_               0.514         
# layer1_vpgms_s_dac_                              
# layer4_vpgms_s_dac_                0.161   0.109 
# wlleak_post_ds0_na_                0.134   0.638 
# wlleak_post_12_na_                         0.999 
# wlrc_240_pcs_                                    
# frlt_sfbcmin_er3_pcs_              0.413  -0.115 
# sfbc_drtime_s_                     0.223  -0.129 
# sfbc_t32wlsalp_far_dc_fresh_pcs_   0.522         
# 
# Factor1 Factor2
# SS loadings      1.574   1.512
# Proportion Var   0.075   0.072
# Cumulative Var   0.075   0.147
# 
# Factor Correlations:
#   Factor1 Factor2
# Factor1   1.000  -0.111
# Factor2  -0.111   1.000
# 
# Test of the hypothesis that 2 factors are sufficient.
# The chi square statistic is 331.95 on 169 degrees of freedom.
# The p-value is 1.08e-12 

fac$correlation
tscore <- fac$scores
tscore
par(mtflow = c(2,1))
plot(tscore[,1], type='l')
plot(tscore[,2], type='l')

# pca  principal component analysis
pca <- princomp(slop~.,ss) 
pca %>% names
pca %>% summary
screeplot(pca)
pca$loadings
biplot(pca, expand=20, xlim=c(-0.0002, 0.0002), ylim=c(-0.0001, 0.0001))
#The other method prcomp
pc_fit <- prcomp( ss[,-22], scale=T)
pc_fit
biplot(pc_fit)
biplot(pc_fit, expand=4, xlim=c(-0.2, 0.2), ylim=c(-0.1, 0.1))
biplot(pc_fit)
# Loadings:
#   Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7 Comp.8 Comp.9 Comp.10 Comp.11 Comp.12 Comp.13 Comp.14 Comp.15 Comp.16
# div2res_s_pcs_                                                                                                                            0.121         
# vref_t_mv_                                                                                                                0.985           0.107         
# vdda_s_dac_                                                                                                                                       0.120 
# sde_pmp_um_s_dac_                                                                                                                                       
# vx4_t_mv_                                              -0.956 -0.289                                                                                    
# vcg_slcr_s_dac_                                                                                                                                         
# halfvccq18_t_mv_                                                                          -0.169  0.960   0.191                                         
# vpgms_sgdprog_t_mv_                                                  -0.324 -0.909  0.252                                                               
# vth_wldd01_l3s_mv_                                                    0.903 -0.374 -0.175                                                               
# vcelsrct_p_                                                                                0.161  0.128          -0.972                                 
# bit_mhopen_io3_pcs_                                                                                                                                     
# bbk_mhopen1_001_vblc015_ev_c_pcs_                                                                -0.198   0.974                                         
# crd_blopen1slc1_b_p_                                                                                                     -0.120   0.162   0.967         
# layer1_vpgms_s_dac_                                                                                                               0.157          -0.976 
# layer4_vpgms_s_dac_                                                                                                               0.972  -0.155   0.165 
# wlleak_post_ds0_na_                                                   0.197         0.677 -0.675                 -0.176                                 
# wlleak_post_12_na_                                                    0.175  0.153  0.660  0.697  0.116                                                 
# wlrc_240_pcs_                     -1.000                                                                                                                
# frlt_sfbcmin_er3_pcs_                           -0.996                                                                                                  
# sfbc_drtime_s_                                         -0.288  0.954                                                                                    
# sfbc_t32wlsalp_far_dc_fresh_pcs_          0.998                                                                                                         
# slop                                                                                                                                                    
# Comp.17 Comp.18 Comp.19 Comp.20 Comp.21 Comp.22
# div2res_s_pcs_                     0.726   0.657           0.130                 
# vref_t_mv_                                                                       
# vdda_s_dac_                                       -0.931   0.336                 
# sde_pmp_um_s_dac_                  0.638  -0.747           0.163                 
# vx4_t_mv_                                                                        
# vcg_slcr_s_dac_                                                    0.998         
# halfvccq18_t_mv_                                                                 
# vpgms_sgdprog_t_mv_                                                              
# vth_wldd01_l3s_mv_                                                               
# vcelsrct_p_                                                                      
# bit_mhopen_io3_pcs_                0.196          -0.336  -0.916                 
# bbk_mhopen1_001_vblc015_ev_c_pcs_                                                
# crd_blopen1slc1_b_p_              -0.124                                         
# layer1_vpgms_s_dac_                               -0.115                         
# layer4_vpgms_s_dac_                                                              
# wlleak_post_ds0_na_                                                              
# wlleak_post_12_na_                                                               
# wlrc_240_pcs_                                                                    
# frlt_sfbcmin_er3_pcs_                                                            
# sfbc_drtime_s_                                                                   
# sfbc_t32wlsalp_far_dc_fresh_pcs_                                                 
# slop                                                                       1.000 
# 
# Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7 Comp.8 Comp.9 Comp.10 Comp.11 Comp.12 Comp.13 Comp.14 Comp.15 Comp.16 Comp.17 Comp.18
# SS loadings     1.000  1.000  1.000  1.000  1.000  1.000  1.000  1.000  1.000   1.000   1.000   1.000   1.000   1.000   1.000   1.000   1.000   1.000
# Proportion Var  0.045  0.045  0.045  0.045  0.045  0.045  0.045  0.045  0.045   0.045   0.045   0.045   0.045   0.045   0.045   0.045   0.045   0.045
# Cumulative Var  0.045  0.091  0.136  0.182  0.227  0.273  0.318  0.364  0.409   0.455   0.500   0.545   0.591   0.636   0.682   0.727   0.773   0.818
# Comp.19 Comp.20 Comp.21 Comp.22
# SS loadings      1.000   1.000   1.000   1.000
# Proportion Var   0.045   0.045   0.045   0.045
# Cumulative Var   0.864   0.909   0.955   1.000


# Can be use cv as,matrix also, but result do not have significant difference 
# x <- as.matrix(dfj[, -353] )
# x[, 350:353]
# dfj[, 350:353]
# y <- as.matrix(dfj[, 353]) # Only slop
# y %>% dim
# cv = cv.glmnet(x, y)
# cv
# load required libraries
library(caret)
library(corrplot)
library(plyr)
library(rpart)
library(randomForest)
set.seed(123)
#Train Random Forest
rf <-randomForest(slop~.,data=train,keep.forest=FALSE, importance=TRUE,ntree=10000)
print(rf)
# Call:
#   randomForest(formula = ty ~ ., data = train, keep.forest = FALSE,      importance = TRUE, ntree = 1000) 
# Type of random forest: regression
# Number of trees: 1000
# No. of variables tried at each split: 117
# 
# Mean of squared residuals: 1.56867e-08
# % Var explained: 82.12

rf <-randomForest(df$slop~.,data=df, importance=TRUE,ntree=1000)
print(rf)
# Call:
#   randomForest(formula = df$slop ~ ., data = df, importance = TRUE,      ntree = 1000) 
# Type of random forest: regression
# Number of trees: 1000
# No. of variables tried at each split: 117
# 
# Mean of squared residuals: 7.743458e-08
# % Var explained: 7.74
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




####   another LASSO :)---------


# Ridge Regression

library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
predict(ridge.mod,s=50,type="coefficients")[1:20,]
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# The Lasso

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]


# Chapter 6 Lab 3: PCR and PLS Regression

# Principal Components Regression

library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP")
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)

# Partial Least Squares
set.seed(123)
# smp_size <- floor(0.90 * nrow(dfj))
# 
# train_ind <- sample(seq_len(nrow(dfj)), size = smp_size, replace = FALSE  )
# 
# train <- data.table(dfj[train_ind, ])
# test <- dfj[-train_ind, ]
# train[,353]
# train %>% str

bound <- floor((nrow(df)/4)*3)         #define % of training and test set

df <- df[sample(nrow(df)), ]           #sample rows 
df.train <- df[1:bound, ]              #get training set
df.test <- df[(bound+1):nrow(df), ]
df.train %>% str
set.seed(1)
pls.fit=plsr(slop ~., data=dfj, subset=df.train, scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)
pls.fit=plsr(Salary~., data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)

##   0919
set.seed(123)
# x <- model.matrix(slop~. , -353, data=dfj )
# x[, 350:353]
# dfj[, 350:353]
# y <- as.matrix(dfj[, 353]) # Only slop
# y %>% dim
# cv = cv.glmnet(x, y)
x <- as.matrix(dfj[, 1:352])
y <- as.matrix(dfj[,353])
fit <- glmnet(x,y)
plot(fit)
coef(fit, s=0.1)
