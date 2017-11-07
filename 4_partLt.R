# October14, cleaning new retreaved data from haddop
library(glmnet)
library(dplyr)
library(plyr)
library(corrplot)
library(caret)
library(devtools)
library(data.table)
l2 <- read.csv("K_LTPD_DS_full.csv", header = TRUE, sep = ",", na.strings = c("NULL", "NaN", "NA"), stringsAsFactors = TRUE)
l2 %>% dim
df <- l2
# make copy of original table and work with copy in case if something going wrong...

df[,1773:1780] %>% head
names(df) <- substring(names(df) , 3)

#names(df) <- substring(names(df[,1:348]), 3) remove  first 2 elements from names
nm<- names(df)
nm
str(df)
tail(df)# I am going to save cycling fbc and tem columns for future
df.c.f.t <- df[,c(1774:1780)]
df.c.f.t %>% head
# identefy and remove fisrt 38colomn and last 6 and remove them
n1 <- names(df[,1:38])
n1
n2 <- names(df[, 1774:1780])  
n2
df <- df[, -c(1:38, 1774:1780) ]



df <- data.table(df)
drop.cols <- grep("avro_file_name$", colnames(df))
drop.cols
df <-df[, (drop.cols) := NULL]

drop.cols <- grep("original_file_name", colnames(df))
drop.cols
df <-df[, (drop.cols) := NULL]
# sub("c.*", "", df[,1])
# names(df) = gsub(pattern = "b.*", replacement = "", x = names(df))

nmr<- names(df)
nmr
write.csv(nmr, "ltpd_fbc_names_fullset.csv")
str(df)
tail(df)
df %>% names %>%  head
df %>% dim
df <- data.frame(df)
df %>% names 

cat('Number of Columns in the original table: ' , ncol(df),'\n')


# Columns with more than 20% missing values
na <- (colSums(is.na(df) | df=='', na.rm = TRUE)/nrow(df) > 0.2)
na1 <- na[na==TRUE]
na1.names <- names(na1)
df <- df[ , !names(df) %in% na1.names]
df2 <-df
cat('Number of columns with more than 20% missing values: ',length(na1),'\n')   
#   shrinked to 1257
cat('Name of these columns: ', na1.names,'\n', sep = ',' )
#cat(na1.names,'\n', sep = ',')
#print('Omitting more than 20% missing result')
print(ncol(df))


# cleaning all columns close to 0
nzv1 <- nearZeroVar(df, saveMetrics= TRUE)
num_a<-row.names(nzv1)[nzv1$nzv == TRUE]
df<-df[, !names(df) %in% num_a]
print('Omitting nzv result')
print(ncol(df))
# 813
cat('Number of columns with near zero variance: ,',length(num_a),'\n')
cat('Name of These columns: ,')
cat(num_a,'\n', sep = ',')
df3 <- df
# names_b_cor <- names(df3)
# names_b_cor
# write.csv(names_b_cor, "names_b_cor.csv")

# find correlation
df.cor <- data.matrix(df)

df.cor <- cor(df.cor, use = "pairwise.complete.obs")
CorPath <- capture.output(cat(substr(outputPath,1,nchar(outputPath)-4),'_CorMatrix-LTPD.csv',sep = ""))
write.csv(df.cor, CorPath, row.names=FALSE, na="")
write.csv(df.cor, "L_CorMatrix_Ltpd.csv", row.names=FALSE, na="")

#The absolute values of pair-wise correlations are considered. If two variables have a high correlation, the function looks at the mean absolute correlation 
#of each variable and removes the variable with the largest mean absolute correlation.
hccor = findCorrelation(df.cor, cutoff = 0.75, names = TRUE)
hccor
df <- df[ , !names(df) %in% hccor]
print('Omitting dependent result')

cat('Number of elemenated columns:     ',length(hc),'\n')
cat('Name of these columns:     ')
cat(hccor,'\n', sep = ',')
cat('Number of   columns:    ',length(df),'\n')
print(dim(df))
##  df$FBC n  id df$u._c6 rename it
#df <- rename(df, c("u._c6" = "FBC"))
# will make copy of the data after cleaning

df5 <- df
col.name <- colnames(df)
col.name
# now I will add back cycling, FBC 
df %>% dim
df.c.f.t %>% names
#df$tem <- df.c.f.t$u.tem
df$ level <- df.c.f.t$level
df$cycling <- df.c.f.t$cycling
df$fbc <- df.c.f.t$fbc
df %>% dim
# [1] 7293   347

# library(data.table)
# df <- data.table(df)
# drop.cols <- grep("avro_file_name$", colnames(df))
# drop.cols
# # 197
# df[, (drop.cols) := NULL]
# df %>% dim
# drop.cols <- grep("original_file_name", colnames(df))
# drop.cols
#drop.cols <- grep("vf_sk_mv_", colnames(df))
df <- na.omit(df)
df6_fbcLTPD <- df

##  save as csv file for next step
write.csv(df, outputPath, row.names=FALSE, na="")
write.csv(df6_fbcLTPD, "L_LTPD_formodel.csv", row.names=FALSE, na="")


#-----------------LASSO--------------------------------------------------------
df <- df6_fbcLTPD
df[,340:345] %>% head
# model with CV and lambda min
set.seed(777)
df$fbc %>% summary

df <- na.omit(df)

df %>% dim
df %>% names

x <- model.matrix(fbc~. , -345, data=df )
x %>% dim
x[, 340:346] %>% head
df[, 340:345] %>% head
y <- as.matrix(df[, 345]) # Only fbc
y %>% head
cv = cv.glmnet(x, y)
cv
cv %>% names
cv$lambda.min
#[1]0.0004500762
model = glmnet(x, y, type.gaussian="covariance",  lambda=cv$lambda.min,standardize = TRUE, standardize.response = TRUE)
model
# Call:  glmnet(x = x, y = y, lambda = cv$lambda.min, standardize = TRUE,      type.gaussian = "covariance", standardize.response = TRUE) 
# 
# Df   %Dev  Lambda
# [1,] 169 0.5646 0.002743
summary(model)

plot(cv, main="LTPD LASSO coefficients capture based on Lambda.min  ")
cv %>% names
cv$lambda.min
#[1]  0.0004500762

# need to standardize all predictors 
# install.packages("ggfortify")
library(ggfortify)
model_s = glmnet(x, y, type.gaussian="covariance",  lambda= cv$lambda.min, standardize = TRUE, standardize.response = TRUE )
model_s
#p_model_s <-glmnet::glmnet(x, y, type.gaussian="covariance",  lambda=cv$lambda.min, standardize = TRUE, standardize.response = TRUE )
#autoplot(model_s, pch=19)
#plot(model_s, xvar = "lambda", label = TRUE)
#plot(model_s, xvar = "dev", label = TRUE)

summary(model_s)

# extracting names of the LASSO MODEL------------------------------------------------
model_s %>% names
model_s$lambda
# [1] 0.0004500762
# pred1 <- predict(model_s, type="coefficients")
# pred1
# pred_300<-predict(model_s, newx = x[1:100,], s = "lambda.min")
# pred_300

pred2 <- predict(model_s,x, s="lambda.1se",type="response")
#plot(pred2,y)
plot(pred2, y,  xlab = "prediction", ylab = "fbc",col = "lightblue", main = "Rate of actual LTPD FBC vs Predicted on model Lasso, 
     lambda min= 0.0004500762") 
abline(lm(y ~ pred2, data=df))
# plot prediction as a slop for the 151 dies
abline(lm(y ~ pred2 + 0),  col="blue")

# I want to make the result table with coloms
temp <- pred1%>% summary
temp$i

#  [1]   1   5  22  45  64  69  78  80  81 163 166 168 174 189 196 205 215 220 289 290 303 304 312 314 331 334 335 339 344
#[30] 347 348 349 351 352 353



pr <- as.matrix(pred1)
pr
res <- data.frame(which( !pr ==0, arr.ind = T))
res$col<- NULL
res
res$sl_coef <- pr[which( !pr ==0, arr.ind = T)]
res %>% summary
res 

# sorting coeff
res_sort <- res[order(- res$sl_coef),]  
res_sort
write.csv(res_sort, "L_LTPD_lasso1.csv")
######----------------LASSO 2-------------------------------------
set.seed(5)

x <- model.matrix(fbc~. , -345, data=df )
x %>% dim
y <- as.matrix(df[, 345]) # Only fbc

cv.lasso <- cv.glmnet(x, y, nfold=10, alpha=1, parallel=TRUE, standardize=TRUE, standardize.response = TRUE, type.measure='mae')
cv.lasso
# Results
plot(cv.lasso, main = "LTPD LASSO coefficients capture with type.measure='mae', Lambda min ")
plot(cv.lasso$glmnet.fit, xvar = "lambda", label = TRUE, main ="LTPD LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568")
plot(cv.lasso$glmnet.fit, xvar = "dev", label = TRUE, main="LTPD LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568 ")
cv.lasso$lambda.min
#[1] 0.01215539
cv.lasso$lambda.1se
#[1] 0.3799428

coeff_lasso <- coef(cv.lasso, s=cv.lasso$lambda.min,exact=TRUE) [which(coef(cv.lasso, s = "lambda.1se") != 0)]

coeff_lasso
#extract coefficient with min lambda and names of LASSO result
# c<-coef(glmnet2, s='lambda.min',exact=TRUE) 
# c
# coef_l <- coef(glmnet2, s='lambda.min',exact=TRUE) [which(coef(glmnet2, s = "lambda.min") != 0)]
colnames <- colnames(df)[which(coef(cv.lasso, s = "lambda.1se") != 0)]
colnames
##  Updated frame of coeff with names of the variable
l_coeffs <- coef(cv.lasso, s = "lambda.1se")
l_name_coeff<- data.frame(name = l_coeffs@Dimnames[[1]][l_coeffs@i + 1], coefficient = l_coeffs@x)
l_name_coeff <- l_name_coeff[order(- l_name_coeff$coefficient),]  # sorting coeff
l_name_coeff 

# result in D_result of htpd" in Exel"



####--------------------------    GLM based on the LASSO pded1-----------------------------
# try to make subset of variables picked by Lasso pred1 
# vector of columns you DON'T want
f <- as.vector(l_name_coeff[,1])
f <-f[-1]  # do need intercept
f %>% str
# subset of all data with selected  21 veriables
df %>% dim
# need to convert level to the 3 col with binary 
df_m <- df
df_l <- model.matrix(~df$level-1, data=df$level)
df_l 
df_m <- cbind(df_m, df_l)
df_m %>% names

df_m %>% names
#df_m$levelMP <- rename(df_m$`df$levelMP`)
colnames(df_m)[346] <- "levelLP" 
colnames(df_m)[347] <- "levelMP"
colnames(df_m)[348] <- "levelUP"
df_m$level <- NULL
df_m %>% names
ss <-df_m%>% select(f)
ss %>% dim
ss %>% names
ss
# add responce 
df$fbc
ss$fbc <-df$fbc
write.csv(ss, "L_LTPDlasso_subset.csv")
lmod_ss <- lm(fbc~., ss)
lmod_ss
summary(lmod_ss)
lmod_summary <-lmod_ss %>% summary
capture.output(lmod_summary, file = "L_LTPD_lassoGLM.txt")
# Call:
#   lm(formula = fbc ~ ., data = ss)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -5.655 -2.919 -1.652  0.528 38.132 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                       4.838e+01  1.886e+02   0.257   0.7977  
# vf_sk_mv_                        -5.896e-02  1.087e-01  -0.542   0.5879  
# sde_pmp_um_s_dac_                 1.999e-01  5.092e-01   0.393   0.6949  
# vcg_dv3_t_mv_                    -3.403e-02  5.448e-02  -0.625   0.5327  
# ron_no33_s_dac_                  -2.290e-02  3.283e-01  -0.070   0.9444  
# ron_odt_18_pn_rate_mv_            3.493e+01  1.200e+02   0.291   0.7712  
# vth_sgs_med_mv_                  -3.023e-03  4.151e-03  -0.728   0.4670  
# fbc_sdllk4_pcs_                  -2.003e-04  5.361e-04  -0.374   0.7089  
# crd_blopen1slc1_b_p_             -4.269e-01  2.343e-01  -1.822   0.0695 .
# layer4_vpgmu_s_dac_               1.685e-01  3.977e-01   0.424   0.6721  
# terasea2slc_us_                   4.811e-03  1.384e-02   0.348   0.7283  
# vth_08pwl60_lt_mv_               -8.688e-04  5.848e-03  -0.149   0.8820  
# vth_08pwl31_mh1_lt_mv_            2.328e-03  9.469e-03   0.246   0.8060  
# wlds0leak_mid_2_pcs_             -8.901e-05  2.099e-04  -0.424   0.6718  
# sfbc_b32wlsaup_drpost_pcs_        6.931e-04  5.930e-04   1.169   0.2435  
# crd_scrnslcbot_p_                 4.363e-01  4.364e-01   1.000   0.3183  
# vpgmslooptrial1__                -4.142e-01  6.677e-01  -0.620   0.5356  
# sfbc_t32wlsalp_far_dc_fresh_pcs_  2.504e-04  5.647e-04   0.443   0.6578  
# sfbc_t32wlsaerx_far_fresh_pcs_   -9.611e-04  2.552e-03  -0.377   0.7068  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.742 on 283 degrees of freedom
# Multiple R-squared:  0.03123,	Adjusted R-squared:  -0.03039 
# F-statistic: 0.5068 on 18 and 283 DF,  p-value: 0.9542

# Percentage of vriance explaned  Contribution Percent (%) (Ssi/SSt)
af.rf <- anova(lmod_ss)
afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
#  trivially, scale them by 100 divided by their sum.
print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
write.csv(result, file =  "L_contribution_LTPD_Lasso.txt")

# anova with stndardized Lasso var
# Linear model with standardizing predictors
ss %>% dim 
ss %>% names
# standartization  # normalize

ss %>% names
ss1 <- lapply(ss, scale)

# ss$level <-as.numeric(ss$level)
# ss$level <- ss$level
ss1 %>% names
st_lmod_rLasso <- lm(fbc~., ss1)
st_lmod_rLasso %>% summary


af.rf <- anova(st_lmod_rLasso)
afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
#  trivially, scale them by 100 divided by their sum.
print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
res_sort <- result[order(- result$PctExp),]  # sorting coeff
res_sort1 <- res_sort[-2,]
write.csv(res_sort1, file =  "L_contribution_LTPD_Lasso_stan_anova12.csv")

##----------       GBM    -----------------------------X--------------------------------------
df %>% dim
df <- df6_fbcLTPD
#nH <- intersect(h1_coef_slop, df6_HTPD_clean)
library(data.table)
require(gbm)
library(gbm)
df %>% dim
#separating training and test data
set.seed(123)
smp_size <- floor(0.90 * nrow(df))

train_ind <- sample(seq_len(nrow(df)), size = smp_size, replace = FALSE  )

train <- df[train_ind, ]
test <- df[-train_ind, ]
#
st.time <- Sys.time()
df.boost=gbm(fbc ~ . ,data = df[train_ind, ],distribution = "gaussian",n.trees = 3000,
             shrinkage = 0.01, interaction.depth = 4)
df.boost
end.time <- Sys.time()
time.taken <- end.time- st.time
time.taken
# time was about 30 sec 
# gbm(formula = fbc ~ ., distribution = "gaussian", data = train, 
#     n.trees = 200, interaction.depth = 4, shrinkage = 0.01)
# A gradient boosted model with gaussian loss function.
# 200 iterations were performed.
# There were 352 predictors of which 12 had non-zero influence.

s<-summary(df.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance
s 
s<- data.table(s)
gbm.imp <-  s[1:30,]
gbm.imp
write.csv(gbm.imp, "L_LTPD_GBMachine_subset.csv")
#
test %>% names
pred.boost <-gbm(formula = fbc ~ ., distribution = "gaussian", data = test, n.trees = 1000, interaction.depth = 4, shrinkage = 0.01)
#A gradient boosted model with gaussian loss function.
#1000 iterations were performed, during less then 1 min
summary(pred.boost)
p_boost <- data.table(summary(pred.boost))
p_boost %>% head(30)
#
# check performance using an out-of-bag estimator
# OOB underestimates the optimal number of iterations
best.iter <- gbm.perf(df.boost,method="OOB")
print(best.iter)
test %>% names
f.predict <- predict(df.boost,test[,-345],best.iter)
f.predict 
df %>% dim
f1.predict <- predict(df.boost, train[,-345], n.trees = 500, type = "response")
f1.predict
gbm.preddf <- data.frame(train[,345], f1.predict)

head(data.frame("PredictedProbability" = f1.predict,"Actual" = train$fbc))

plot( test$fbc,  f.predict,
      
      xlab = "FBC", ylab = "prediction",col = "blue", 
      main = "Rate of actual LTPD FBC rate againt of predicted GBM") 

plot(train$fbc, f1.predict,  xlab = "FBC", ylab = "Prediction",col = "blue", main = "Rate of actual LTPD FBC growth against to predicted, 
     based on Gradient Boosting Machine")            
# plot prediction as a slop for the 151 dies
#abline(lm( train$fbc ~ f1.predict, data=df))
# plot  perfect prediction line red
#abline(lm(train$fbc ~ f1.predict + 0),  col="blue")


#  Contribution ----------------------------------

# vector of columns we want
gbm.imp <- data.frame(gbm.imp)
gbm.imp
v <- as.vector(gbm.imp[,"var"])
v
#f <-f[-1]  # do need intercept
v %>% str
# subset of all data with selected  21 veriables
# df %>% dim
# # need to convert level to the 3 col with binary 
# df_m <- df
# df_l <- model.matrix(~df$level-1, data=df$level)
# df_l 
# df_m <- cbind(df_m, df_l)
# df_m %>% names
# 
# df_m %>% names
# #df_m$levelMP <- rename(df_m$`df$levelMP`)
# colnames(df_m)[346] <- "levelLP" 
# colnames(df_m)[347] <- "levelMP"
# colnames(df_m)[348] <- "levelUP"
# df_m <- df_m[, -"level"]
# df_m %>% names
sg <-df%>% select(v)
sg %>% dim
sg %>% names
sg
# add responce 
df$fbc
sg$fbc <-df$fbc
write.csv(ss, "L_LTPD_GBM_subset.csv")
lmod_GBM_Ltpd <- lm(fbc~., sg)
lmod_GBM_Ltpd
summary(lmod_GBM_Ltpd)
lmod_GBM_L <-lmod_GBM_Ltpd%>% summary
capture.output(lmod_GBM_L, file = "L_LTPD_GMB_linearModel.txt")

# Linear model with standardizing predictors
sg %>% dim

sg %>% names
# anova with stndardized  GBM var
# standartization  # normalize
sg %>% str
sg$level <-as.numeric(sg$level)
sg$level
df$level <- as.numeric(df$level)
sg1 <- lapply(sg, scale)

# ss$level <-as.numeric(ss$level)
# ss$level <- ss$level
#sg1 <- data.table(sg1)
st_lmod_GBM <- lm(fbc~., sg1)
st_lmod_GBM %>% summary
st_lmod_GBM
af.rf1 <- anova(st_lmod_GBM)
af.rf1
afss.rf1 <- af.rf1$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
#  trivially, scale them by 100 divided by their sum.
print(cbind(af.rf1,    PctExp=afss.rf1/sum(afss.rf1)*100))
result<-cbind(af.rf1,  PctExp=afss.rf1/sum(afss.rf1)*100)
res_sort <- result[order(- result$PctExp),]  # sorting coeff
res_sort1 <- res_sort[-1,]
res_sort1 <- res_sort1[1:30,]
write.csv(res_sort1,  file = "L_contribution_LTPD_GBM_stan_anova11.csv")

#-----------------------------------------------------------------------------end GBM--------------------------


#-----------------------------------------------------------------------------end GBM--------------------------


########     NO RUN#######


# ------------------------------------Random Forest -----------------------------------------------
library(caret)
library(dplyr)
library(corrplot)
library(plyr)
library(rpart)
library(randomForest)
set.seed(5)

## 75% of the sample size  or may be 80%  ???

hdf<- read.csv("L_LTPD_formodel.csv")
hdf %>% dim
df <- na.omit(hdf)
df %>% dim
set.seed(123)
smp_size <- floor(0.90 * nrow(df))

train_ind <- sample(seq_len(nrow(df)), size = smp_size, replace = FALSE  )

train <- df[train_ind, ]
test <- df[-train_ind, ]

#TRAIN
train %>% dim
names(train)
tx <- model.matrix(fbc~. , -347, data=train )
ty <- as.matrix(train[, 347]) # Only fbc
ty %>% dim

# TEST
test %>% dim
names(test)
y_test <- test[,347]
y_test %>% summary  # we need to to campare result
#test <- model.matrix(fbc~. , -351, data=test )
xtest <- test[, -346]
#Train Random Forest
df %>% dim
start.time <- Sys.time()

rf <-randomForest(df$fbc~.,data=df, teskeep.forest=FALSE, importance=TRUE,ntree=200)
rf
# Call:
#   randomForest(formula = df$fbc ~ ., data = df, teskeep.forest = FALSE,      importance = TRUE, ntree = 200) 
# Type of random forest: regression
# Number of trees: 200
# No. of variables tried at each split: 115
# 
# Mean of squared residuals: 5.251101
# % Var explained: 88.01

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
#Time difference of 10.60247 mins

plot(rf , main =" L_Random forest 200 trees for LTPDe")

#    will see importence var
impRF<-rf$importance    #worked
importance(rf, type =1 )
impRF = importance(rf, type =1)
impRF <- data.frame(predictors=rownames(impRF),impRF)

rf$importanceSD
rf %>% summary
rf$mse
# Order the predictor levels by importance 
# I guess %IncMSE of j'th is (mse(j)-mse0)/mse0 * 100%   so the higher number more importent

imp.sortRF <- arrange(impRF,desc(impRF$X.IncMSE))
imp.sortRF
imp.sortRF$predictors <- factor(imp.sortRF$predictors,levels=imp.sortRF$predictors)
imp.sortRF$predictors
imp.sortRF
# Select the top 20 predictors
imp.30<- imp.sortRF[1:30,]
print(imp.30)
write.csv(imp.30, "L_LTPD_RandomForest_200t_30var.csv")

# Now we can compare the Out of Bag Sample Errors and Error on Test set
# The above Random Forest model chose Randomly 4 variables to be considered at each split. We could now try all possible 13 predictors which can be found at each split.

oob.err=double(13)
test.err=double(13)


# let try to do tain and predict on test------------------------- NO RUN-------------------------------------------
#ptm <- proc.time()
start.time <- Sys.time()
set.seed(123)
rftrain <-randomForest(train$fbc ~., data = train, teskeep.forest=FALSE, importance=TRUE,ntree=150)
rftrain
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken 
#Time difference of 3.938105 mins
# Call:
#  randomForest(formula = train$fbc ~ ., data = train, teskeep.forest = FALSE,      importance = TRUE, ntree = 150) 
# Type of random forest: regression
# Number of trees: 150
# No. of variables tried at each split: 115
# 
# Mean of squared residuals: 5.917783
# % Var explained: 86.63
varImpPlot(rftrain)
pred_rf <- predict(rftrain, test)
pred_rf
plot(pred_rf, alpha=.2)
plot(rftrain)

matRF_L <- data.table(test$fbc, pred_rf)
matRF_L %>% plot
#------------------- PLOTING----------------------------------
library(ggplot2)
library(plot3D)
library(randomForestSRC)
library(ggRandomForests)
ggplot(df6_L)+
  geom_boxplot(aes(y=fbc,x=cycling,fill=level),notch=T,notchwidth=0.8)+
  facet_grid(~level,margins=T)+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  ylab(label='fbc')+
  xlab('cycling')+
  ggtitle('Plot of the FBC by cycling on t=-15 ( LTPD) ')

p1=ggplot(df6_L)+
  geom_point(aes(y=fbc,x=cycling,color=as.factor(level)))+
  # scale_x_log10()+scale_y_log10()+
  facet_grid(~level)+
  ylab('FBC')+
  xlab('Cycling')+
  ggtitle('Plot of the FBC by cycling with t=15 ( LTPD) ')
p1
varImpPlot(rftrain, main = "Random Forest on LTPD")
# all plots saved as "P...HTPD"

## plot Predicted VS Actual
df
pred<-predict(object=rftrain,newdata=test[,-347])
actual<-test$fbc
result.plot<-data.frame(actual=actual,predicted=pred)
paste('Function Call: ', rftrain$call)
result.plot
# [1] "Function Call:  randomForest"  "Function Call:  train$fbc ~ ." "Function Call:  train"        
# [4] "Function Call:  FALSE"         "Function Call:  TRUE"          "Function Call:  150" 

paste('Mean Squared error: ',mean(rftrain$mse))
#[1] "Mean Squared error:  0.266526574525833"
paste('Root Mean Squared error: ',mean(sqrt(rftrain$mse)))
#[1] "Root Mean Squared error:  0.51527253953601"
gg <-ggplot(result)+
  geom_point(aes(x=actual,y=predicted,color=predicted-actual),alpha=0.3)+
  ggtitle('Predicted vs Actual on test data LTPD')
result.plot
gg
# add abline

# plot  perfect prediction line red
reg <- lm(actual~predicted, data=result.plot )
reg
# Call:
#   lm(formula = actual ~ predicted, data = result.plot)
# 
# Coefficients:
#   (Intercept)    predicted  
# -0.4143       1.0508  
gg1 <-gg + geom_abline(intercept = -0.4143, slop = 1.0508, color = "light blue", linetype = "dashed", size = 1)
## perfect prediction
reg <- lm(actual~predicted+ 0, data=result.plot )
reg


#abline(lm(actual ~ predicted +0, data=result.plot, col="green"))
gg1 + geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 0.5)



gg_e <- gg_error(rftrain)
plot(gg_e)
plot(gg_rfsrc(rftrain), alpha=.2 )  

## plot Predicted VS Actual

pred<-predict(object=rftrain,newdata=test[,-347])
actual<-test$fbc
result<-data.frame(actual=actual,predicted=pred)
paste('Function Call: ', rftrain$call)

# Plot the VIMP rankings of independent variables.
#rfsrc_tr <- rfsrc(fbc ~ ., data = train)
plot(gg_vimp(rftrain), 20)

ggplot(rftrain, aes(x=fbc, y=cycling, color=chas))+
  geom_point(alpha=.4)+
  geom_rug(data=rftrain %>% filter(is.na(cycling)))+
  labs(y="", x=st.labs["mfbc"]) +
  scale_color_brewer(palette="Set2")+
  facet_wrap(~variable, scales="free_y", ncol=3)



####    XGBoost---------------------------------------
library(data.table)
require(gbm)
library(gbm)

#separating training and test data
set.seed(123)
smp_size <- floor(0.90 * nrow(df))

train_ind <- sample(seq_len(nrow(df)), size = smp_size, replace = FALSE  )

train <- df[train_ind, ]
test <- df[-train_ind, ]
#

df.boost=gbm(fbc ~ . ,data = df[train_ind, ],distribution = "gaussian",n.trees = 200,
             shrinkage = 0.01, interaction.depth = 4)
df.boost
# time was about 1 min
# gbm(formula = fbc ~ ., distribution = "gaussian", data = train, 
#     n.trees = 200, interaction.depth = 4, shrinkage = 0.01)
# A gradient boosted model with gaussian loss function.
# 200 iterations were performed.
# There were 352 predictors of which 12 had non-zero influence.

s<-summary(df.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance
s 
s<- data.table(s)
s[1:20,]
# var     rel.inf
# tem                                                             tem 68.56837693
# level                                                         level 14.95446290
# sfbc_b32wlsaup_drpost_pcs_               sfbc_b32wlsaup_drpost_pcs_  6.98134780
# vth_sgs_med_mv_                                     vth_sgs_med_mv_  4.45158309
# cycling                                                     cycling  3.54464492
# sfbc_t32wlsalp_far_dc_fresh_pcs_   sfbc_t32wlsalp_far_dc_fresh_pcs_  0.85982695
# crd_m1bll_224_c_pcs_                           crd_m1bll_224_c_pcs_  0.23196186
# vth_12pwl3_lt_mv_                                 vth_12pwl3_lt_mv_  0.11601911
# sfbc_b32wlsalp_dc_post_pcs_             sfbc_b32wlsalp_dc_post_pcs_  0.10441571
# fbc_sdllk4_pcs_                                     fbc_sdllk4_pcs_  0.08722952
# vth_wlds0last_l3s_mv_                         vth_wlds0last_l3s_mv_  0.07596677
# sfbc_slcerslp_5kpost_pcs_                 sfbc_slcerslp_5kpost_pcs_  0.02416446
# vf_sk_mv_                                                 vf_sk_mv_  0.00000000
# iccsb_300a_ua_                                       iccsb_300a_ua_  0.00000000
# crd_ttl_pl0_p_                                       crd_ttl_pl0_p_  0.00000000
# bbk_ttl_pl1_p_                                       bbk_ttl_pl1_p_  0.00000000
# div2res_s_pcs_                                       div2res_s_pcs_  0.00000000
# fifthratio__                                           fifthratio__  0.00000000
# iref_t_ua_                                               iref_t_ua_  0.00000000
# iref_s_dac_                                             iref_s_dac_  0.00000000

pred.boost <-gbm(formula = fbc ~ ., distribution = "gaussian", data = df[-train_ind], n.trees = 200, interaction.depth = 4, shrinkage = 0.01)
#A gradient boosted model with gaussian loss function.
#10000 iterations were performed.
#There were 13 predictors of which 13 had non-zero influence.

summary(pred.boost)
p_boost <- data.table(summary(pred.boost))
p_boost %>% head(30)
#
# check performance using an out-of-bag estimator
# OOB underestimates the optimal number of iterations
best.iter <- gbm.perf(df.boost,method="OOB")
print(best.iter)
test[,345] %>% dim
df.boost %>% dim
f.predict <- predict(df.boost,test[,-345],best.iter)
f.predict
test %>% names
f1.predict <- predict(df.boost, train[,-345], n.trees = 200, type = "response")
f1.predict
gbm.preddf <- data.frame(train[,345], f1.predict)

head(data.frame("PredictedProbability" = f1.predict,"Actual" = train$fbc))

plot( f1.predict, train$fbc, 
      
      xlab = "prediction", ylab = "fbc",col = "blue", 
      main = "Rate of actual HTPD FBC rate againt of predicted GBM") 
plot(f1.predict, train$fbc,   xlab = "prediction", ylab = "fbc",col = "light blue", main = "Rate of actual RTPD FBC growth against to predicted, 
     based on Gradient Boosting Machine")            
# plot prediction as a slop for the 151 dies
abline(lm( train$fbc ~ f1.predict, data=df))
# plot  perfect prediction line red
#abline(lm(train$fbc ~ f1.predict + 0),  col="blue")

#  Contribution ----------------------------------

# vector of columns we want
gbm.imp <- data.frame(gbm.imp)
gbm.imp
v <- as.vector(gbm.imp[,"var"])
v
#f <-f[-1]  # do need intercept
v %>% str
# subset of all data with selected  21 veriables
# df %>% dim
# # need to convert level to the 3 col with binary 
# df_m <- df
# df_l <- model.matrix(~df$level-1, data=df$level)
# df_l 
# df_m <- cbind(df_m, df_l)
# df_m %>% names
# 
# df_m %>% names
# #df_m$levelMP <- rename(df_m$`df$levelMP`)
# colnames(df_m)[346] <- "levelLP" 
# colnames(df_m)[347] <- "levelMP"
# colnames(df_m)[348] <- "levelUP"
# df_m <- df_m[, -"level"]
# df_m %>% names
sg <-df%>% select(v)
sg %>% dim
sg %>% names
sg
# add responce 
df$fbc
sg$fbc <-df$fbc
write.csv(ss, "L_LTPD_GBM_subset.csv")
lmod_GBM_Ltpd <- lm(fbc~., sg)
lmod_GBM_Ltpd
summary(lmod_GBM_Ltpd)
#lmod_GBM_Rtpr <-lmod_Lasso_Ltpd %>% summary
capture.output(lmod_GBM_Ltpd, file = "L_LTPD_GMB_linearModel.txt")

# Linear model with standardizing predictors
sg %>% dim
sg %>% names
# standartization  # normalize

sg %>% names
ssg <- lapply(sg, scale)

# ss$level <-as.numeric(ss$level)
# ss$level <- ss$level
ssg %>% names
st_lmod_rGBM <- lm(fbc~., ssg)
st_lmod_rGBM %>% summary



# Percentage of vriance explaned  Contribution Percent (%) (Ssi/SSt)
af.rf <- anova(lmod_GBM_Ltpd)
afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
#  trivially, scale them by 100 divided by their sum.
print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
write.csv(result, file = "R_RTPD_GBMcontribution_anova.csv")
# anova with stndardized Lasso var
af.rf <- anova(st_lmod_rGBM)
afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
#  trivially, scale them by 100 divided by their sum.
print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
res_sort <- result[order(- result$PctExp),]  # sorting coeff
res_sort1 <- res_sort[-2,]
res_sort1 <- res_sort1[1:30,]
res_sort1
write.csv(res_sort1, file = "R_contribution_RTPD_GBM_stan_anova.csv")

summary(pred.boost)
p_boost <- data.table(summary(pred.boost))
library(ggplot2)
p <-ggplot(p_boost,aes(p_boost[1:20,2], p_boost[1:20,1]))


p+geom_bar(stat="identity")
pred.boost$oobag.improve
plot(p_boost,
     i.var = 20,
     n.trees = df.boost$n.trees,
     continuous.resolution = 1,
     return.grid = FALSE,
     type = "link")


#Plot of Response variable with lstat variable
summary(p_boost,n.trees=best.iter)
# plot(df.boost,
#      i.var = 1:20,
#      n.trees = df.boost$n.trees,
#      continuous.resolution = 100,
#      return.grid = TRUE,
#      type = "link"
# )
abline(model1, col="lightblue")
#Inverse relation with lstat variable

plot(pred.doost,i="rm") 
#as the average number of rooms increases the the price increases

#Prediction on Test Set
#We will compute the Test Error as a function of number of Trees.

n.trees = seq(from=100 ,to=1000, by=100) #no of trees-a vector of 100 values 

#Generating a Prediction matrix for each Tree
predmatrix<-predict(df.boost,test,n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix
predmatrix <- as.vector(round(predmatrix))
test_new <- cbind(test,predmatrix)
test_new[,351:354]
test_new <- data.frame(test_new)
library(ggplot2)

a <- ggplot(test_new, aes(x= test_new$fbc, test_new$cycling))
a + geom_bar(stat = "identity")
b <- ggplot(test_new, aes(x= test_new$predmatrix, test_new$cycling))
b + geom_bar(stat = "identity")

plot(test_new$fbc, test_new$predmatrix, xlabel= "actual fbc", ylabel= "Predicted fbc")
#Calculating The Mean squared Test Error
test.error<-with(test,apply( (predmatrix-test$fbc)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

#Plotting the test error vs number of trees



plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

#adding the RandomForests Minimum Error line trained on same data and similar parameters
abline(h = min(test.err),col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)


dim(predmatrix)


head(test.error)
a <- ggplot(test)

