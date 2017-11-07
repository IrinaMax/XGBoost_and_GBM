# October14, cleaning new retreaved data from haddop
library(glmnet)
library(dplyr)
library(plyr)
library(corrplot)
library(caret)
library(devtools)
library(data.table)
h2 <- read.csv("K_HTPD_DS_full.csv", header = TRUE, sep = ",", na.strings = c("NULL", "NaN", "NA"), stringsAsFactors = TRUE)
h2 %>% dim
h2[,1772:1780] %>% head
# check out the invalid data
#outht <- data.frame(which( h2$u.fbc >200, arr.ind = T))
#outht
df <- h2
df %>% dim 
#  3648 1780
# remove  first 2 elements from names
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
write.csv(nmr, "htpd_fbc_names_fullset.csv")
str(df)
tail(df)
df %>% names %>%  head
df %>% dim

df <- data.frame(df)
df %>% names 

df2 <- df  # save after changes and continue to work with df
df %>% dim 
df <- df2
df %>%dim
df <- data.frame(df)
cat('Number of Columns in the original table: ' , ncol(df),'\n')   #1616
cat(ncol(df),'\n')   # 1737

# Columns with more than 20% missing values
na <- (colSums(is.na(df) | df=='', na.rm = TRUE)/nrow(df) > 0.2)
na
na1 <- na[na==TRUE]   #402 
na1.names <- names(na1)
na1.names
df <- df[ , !names(df) %in% na1.names]
df3 <-df
df %>% dim
# 

cat('Number of columns with more than 20% missing values: ',length(na1),'\n')   
#   shrinked to 1257
cat('Name of these columns: ', na1.names,'\n', sep = ',' )
#cat(na1.names,'\n', sep = ',')
#print('Omitting more than 20% missing result')
print(ncol(df))
#

# cleaning all columns close to 0
nzv1 <- nearZeroVar(df, saveMetrics= TRUE)
num_a<-row.names(nzv1)[nzv1$nzv == TRUE]
df<-df[, !names(df) %in% num_a]
print('Omitting nzv result')
print(ncol(df))
# 807
cat('Number of columns with near zero variance: ,',length(num_a),'\n')
cat('Name of These columns: ,')
cat(num_a,'\n', sep = ',')
df4 <- df
df %>% dim
#[1] 3645  807
write.csv(df4, "H_HTPD_bifore_correlation.csv")
# names_b_cor <- names(df3)
# names_b_cor
# write.csv(names_b_cor, "names_b_cor.csv")

# find correlation
df.cor <- data.matrix(df)

df.cor <- cor(df.cor, use = "pairwise.complete.obs")
CorPath <- capture.output(cat(substr(outputPath,1,nchar(outputPath)-4),'H_CorMatrix.csv',sep = ""))
write.csv(df.cor, CorPath, row.names=FALSE, na="")
write.csv(df.cor, "H_CorMatrix_HTPD.csv", row.names=FALSE, na="")

#The absolute values of pair-wise correlations are considered. If two variables have a high correlation, the function looks at the mean absolute correlation 
#of each variable and removes the variable with the largest mean absolute correlation.
hccor = findCorrelation(df.cor, cutoff = 0.75, names = TRUE)
hccor
df <- df[ , !names(df) %in% hccor]
print('Omitting dependent result')

cat('Number of elemenated columns:     ',length(hccor),'\n')  #487 
cat('Name of these columns:     ')
cat(hccor,'\n', sep = ',')
cat('Number of   columns:    ',length(df),'\n')
print(dim(df))
##  df$FBC n  id df$u._c6 rename it
#df <- rename(df, c("u._c6" = "FBC"))
# will make copy of the data after cleaning
df5 <- df
#col.names_S_Htpd <- colnames(df)
#df$slop
##  save as csv file for next step
#write.csv(df, outputPath, row.names=FALSE, na="")

#sink()
#
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
df %>% names
# [1] 3645   344


df6_fbcHTPD <- df
# final resul after cleaning [1] [1] 3645  344

##  save as csv file for next step
write.csv(df, outputPath, row.names=FALSE, na="")
write.csv(df6_fbcHTPD, "H_HTPD_formodel.csv", row.names=FALSE, na="")
df %>% dim
df6_fbcHTPD[,340:344] %>% head


#-----------------LASSO--------------------------------------------------------
 df <- df6_fbcHTPD
df[,340:344] %>% head

# # model with CV and lambda min
# set.seed(777)
# df$fbc %>% summary
# df <- na.omit(df)
# #df[!complete.cases(df),]
# #row.has.na <- apply(df, 1, function(x){any(is.na(x))})
# #row.has.na    ##  row 35 was been removed
# 
# df %>% dim
# df %>% names
# x <- model.matrix(fbc~. , -342, data=df )
# # x <- data.frame(x)
# # x %>% dim
# # drop.cols <- grep("avro_file_name$", colnames(x))
# # drop.cols
# # # 197
# # #df[, (drop.cols) := NULL]
# # df %>% dim
# # df %>% names
# # drop.cols <- grep("original_file$", colnames(x))
# # drop.cols
# # df[, (drop.cols) := NULL]
# x %>% dim
# x[, 340:344] %>% head
# df[, 340:343] %>% head
# y <- as.matrix(df[, 342]) # Only fbc
# y %>% dim
# y %>% head
# cv = cv.glmnet(x, y)
# cv
# cv %>% names
# cv$lambda.min
# #[1] 0.003980341
# model = glmnet(x, y, type.gaussian="covariance",  lambda=cv$lambda.min,standardize = TRUE, standardize.response = TRUE)
# model
# # Call:  glmnet(x = x, y = y, lambda = cv$lambda.min, standardize = TRUE,      type.gaussian = "covariance", standardize.response = TRUE) 
# # 
# # Df   %Dev  Lambda
# # [1,] 169 0.5646 0.002743
# summary(model)
# 
# plot(cv, main="HTPD LASSO coefficients capture based on Lambda.min 0.003980341 ")
# cv %>% names
# cv$lambda.min
# #[1] 0.003980341
# 
# # need to standardize all predictors 
# # install.packages("ggfortify")
# library(ggfortify)
# model_s = glmnet(x, y, type.gaussian="covariance",  lambda=cv$lambda.min, standardize = TRUE, standardize.response = TRUE )
# model_s
# #p_model_s <-glmnet::glmnet(x, y, type.gaussian="covariance",  lambda=cv$lambda.min, standardize = TRUE, standardize.response = TRUE )
# #autoplot(model_s, pch=19)
# #plot(model_s, xvar = "lambda", label = TRUE)
# #plot(model_s, xvar = "dev", label = TRUE)
# 
# summary(model_s)
# 
# # extracting names of the LASSO MODEL------------------------------------------------
# model %>% names
# model_s$lambda
# # [1] 0.04574851
# pred1 <- predict(model_s, type="coefficients")
# pred1
# pred_100<-predict(model_s, newx = x[1:100,], s = "lambda.min")
# pred_100
# 
# pred2 <- predict(model_s,x, s="lambda.min",type="response")
# plot(pred2,y)
# plot(pred2, y,  xlab = "prediction", ylab = "fbc",col = " red", main = "Expectation  fbc HTPD on model LASSO pred2, lambda min=0.003980341") 
# abline(lm(y ~ pred2, data=df))
# # plot prediction as a slop for the 151 dies
# #abline(lm(y ~ pred2 + 0),  col="blue")
# 
# # I want to make the result table with coloms
# temp <- pred1%>% summary
# temp$i
# 
# #  [1]   1   5  22  45  64  69  78  80  81 163 166 168 174 189 196 205 215 220 289 290 303 304 312 314 331 334 335 339 344
# #[30] 347 348 349 351 352 353
# 
# 
# 
# pr <- as.matrix(pred1)
# pr
# res <- data.frame(which( !pr ==0, arr.ind = T))
# res$col<- NULL
# res
# res$sl_coef <- pr[which( !pr ==0, arr.ind = T)]
# res %>% summary
# res 
# #
# res_sort <- res[order(- res$sl_coef),]  # sorting coeff
# res_sort
# write.csv(res_sort, "H_HTPD_lasso.csv")
######----------------LASSO 2-------------------------------------
set.seed(777)
df <- data.table(df6_fbcHTPD)
df <- na.omit(df)
df %>% names
x <- model.matrix(fbc~. , -344, data=df )

y <- as.matrix(df[, 344]) # Only fbc
df %>% dim
cv.lasso <- cv.glmnet(x, y, nfold=10, alpha=1, parallel=TRUE, standardize=TRUE, standardize.response = TRUE, type.measure='mae')
cv.lasso
# Results
plot(cv.lasso, main = "HTPD LASSO coefficients capture with type.measure='mae', Lambda min 0.01215539 ")
plot(cv.lasso$glmnet.fit, xvar = "lambda", label = TRUE, main ="HTPD LASSO coefficients capture with type.measure='mae', Lambda.min")
plot(cv.lasso$glmnet.fit, xvar = "dev", label = TRUE, main="HTPD LASSO coefficients capture with type.measure='mae', Lambda.min  ")
cv.lasso$lambda.min
 # 0.01215539
cv.lasso$lambda.1se
#[1] 0.3799428

coeff_lasso <- coef(cv.lasso, s=cv.lasso$lambda.min,exact=TRUE) [which(coef(cv.lasso, s = "lambda.min") != 0)]

coeff_lasso
#extract coefficient with min lambda and names of LASSO result
# c<-coef(glmnet2, s='lambda.min',exact=TRUE) 
# c
# coef_l <- coef(glmnet2, s='lambda.min',exact=TRUE) [which(coef(glmnet2, s = "lambda.min") != 0)]
colnames <- colnames(df)[which(coef(cv.lasso, s = "lambda.min") != 0)]
colnames
##  Updated frame of coeff with names of the variable
l_coeffs <- coef(cv.lasso, s = "lambda.min")
l_name_coeff<- data.frame(name = l_coeffs@Dimnames[[1]][l_coeffs@i + 1], coefficient = l_coeffs@x)
l_name_coeff <- l_name_coeff[order(- l_name_coeff$coefficient),]  # sorting coeff
l_name_coeff 
# result in D_result of htpd" in Exel"





####--------------------------    GenLinModel based on the LASSO -----------------------------
# try to make subset of variables picked by Lasso mae 
# vector of columns you DON'T want
f <- as.vector(l_name_coeff[,1])
f
f <- f[-1]  # do not need intercept
f %>% str
# subset of all data with selected  21 veriables
df %>% dim
#   need to extract level in matrix for 3
df_m <- df
df_l <- model.matrix(~df$level-1, data=df$level)
df_l 
df_m <- cbind(df_m, df_l)
df_m %>% names

#rename all 3 and delete level
colnames(df_m)[345] <- "levelLP" 
colnames(df_m)[346] <- "levelMP"
colnames(df_m)[347] <- "levelUP"
df_m <- df_m[, -"level"]
df_m %>% names
ss <-df_m %>% select(f)
ss %>% dim
ss %>% names
ss
# add responce 
df$fbc
ss$fbc <-df$fbc
write.csv(ss, "H_HTPDlasso_subseton_FBC.csv")
lmod_ss <- lm(fbc~., ss)
lmod_ss
summary(lmod_ss)
lmod_summary <-lmod_ss %>% summary
capture.output(lmod_summary, file = "H_HTPD_lasso_GLMon_FBC.txt")


# Percentage of vriance explaned  Contribution Percent (%) (Ssi/SSt)
# af.rf <- anova(lmod_ss)
# afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
# #  trivially, scale them by 100 divided by their sum.
# print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
# result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
# res_sort <- result[order(- result$PctExp),]  # sorting coeff
# res_sort1 <- res_sort[-1,]
# res_sort1 <- res_sort1[1:30,]
# res_sort1

#write.csv(result1, file = "H_contribution_HTPD_Lasso.csv")

# anova with stndardized Lasso var
# standartization  # normalize

ss %>% names
ss1 <- lapply(ss, scale)

# ss$level <-as.numeric(ss$level)
# ss$level <- ss$level
ss1 %>% names
st_lmod_h <- lm(fbc~., ss1)
st_lmod_h %>% summary
af.rf <- anova(st_lmod_h)
afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
#  trivially, scale them by 100 divided by their sum.
print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
res_sort <- result[order(- result$PctExp),]  # sorting coeff
res_sort1 <- res_sort[-1,]
res_sort1 <- res_sort1[1:30,]
res_sort1
write.csv(res_sort1, file = "H_HTPD_contribution_Lasso_stan_anova.csv")

##----------       GBM    -----------------------------X--------------------------------------
df %>% dim
df <- df6_fbcHTPD

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
# Time difference of 6.068622 mins for 5000 trees
#Time difference of 1.964661 mins for 2000 trees
#gbm(formula = fbc ~ ., distribution = "gaussian", data = df[train_ind, 
# ], n.trees = 10000, interaction.depth = 4, shrinkage = 0.01)
# A gradient boosted model with gaussian loss function.
# 10000 iterations were performed.
# There were 342 predictors of which 338 had non-zero influence.
end.time <- Sys.time()
time.taken <- end.time- st.time
time.taken
s<-summary(df.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance
s 
s<- data.table(s)
gbm.imp <-  s[1:30,]
gbm.imp
write.csv(gbm.imp, "H_fbcHTPD_GBMachine_var11.csv")



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
f.predict <- predict(df.boost,test[,-344], best.iter)
f.predict 
head(data.frame("PredictedProbability" = f.predict,"Actual" = test$fbc))
df %>% dim
f1.predict <- predict(df.boost, train[,-344], n.trees = 1000, type = "response")
f1.predict
gbm.preddf <- data.frame(train[,344], f1.predict)

head(data.frame("PredictedProbability" = f1.predict,"Actual" = train$fbc))

plot( f.predict, test$fbc, 
      xlab = "prediction", ylab = "fbc",col = "dark red", 
      main = "Rate of actual HTPD FBC rate againt of predicted GBM") 
plot( train$fbc, f1.predict,  xlab = "FBC", ylab = "Prediction",col = " red", main = "Rate of actual HTPD FBC against to predicted, 
     based on Gradient Boosting Machine")            
# plot prediction as a slop for the 151 dies
#abline(lm( train$fbc ~ f1.predict, data=df))
# plot  perfect prediction line red
# abline(lm(train$fbc ~ f1.predict + 0),  col="blue")

#  Contribution ----------------------------------

# vector of columns we want
gbm.imp <- data.frame(gbm.imp)
gbm.imp %>% names
v <- as.vector(gbm.imp[,"var"])
v
#f <-f[-1]  # do need intercept
v %>% str
# subset of DS data with selected  21 veriables
df %>% dim
sg <-df%>% select(v)
sg %>% dim
sg %>% names
sg
# # add responce 
 df$fbc
 sg$fbc <-df$fbc
 write.csv(sg, "H_HTPD_GBM_subset.csv")
# lmod_GBM_Htpd <- lm(fbc~., sg)
# lmod_GBM_Htpd
# summary(lmod_GBM_Htpd)
# lmod_GBM_Htpr <-lmod_GBM_Htpd %>% summary
# capture.output(lmod_GBM_Htpr, file = "H_HTPD_GMB_linearModel.txt")
# 
# # Linear model with standardizing predictors
# sg %>% dim
# sg %>% names
# # Percentage of vriance explaned  Contribution Percent (%) (Ssi/SSt)
# af.rf <- anova(lmod_GBM_Htpd)
# afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
# #  trivially, scale them by 100 divided by their sum.
# print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
# result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
# res_sort <- result[order(- result$PctExp),]  # sorting coeff
# res_sort1 <- res_sort[-1,]
# res_sort1 <- res_sort1[1:30,]
# res_sort1
# write.csv(result, file = "H_HTPD_GBMcontribution_NOTnorm.csv")

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
write.csv(res_sort1, file = "H_HTPDcontribution_GBM_stan_anova11.csv")


#-----------------------------------------------------------------------------end GBM--------------------------

####  the othe way to get contribution to spread level to 3 before
# standardizing predictors
library(dplyr)
sg <-df%>% select(v)
sg %>% dim
sg %>% names
sg$level
# standartization  # normalize
# Linear model with standardizing predictors
library(tidyr)
 sgg <- sg %>% spread(sg$level) 
sgg
  
sg %>%  dim

sg_m <- sg
sg_l <- model.matrix(~sg$level-1, data=sg$level)
sg_l 
sg_m <- cbind(sg_m, sg_l)
sg_m %>% names
sg_m <- data.table(sg_m)
#df_m$levelMP <- rename(df_m$`df$levelMP`)
colnames(sg_m)[31] <- "levelLP" 
colnames(sg_m)[32] <- "levelMP"
colnames(sg_m)[33] <- "levelUP"
sg_m$level <- NULL
sg_m %>% names

ss2 <- lapply(sg_m, scale)
plot(ss2)
# ss$level <-as.numeric(ss$level)
# ss$level <- ss$level
ss2 %>% names
st_lmod_LGBM <- lm(slop~., ss2)
st_lmod_LGBM %>% summary


# Percentage of vriance explaned  Contribution Percent (%) (Ssi/SSt)
# af.rf <- anova(lmod_GBM_Ltpd)
# afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
# #  trivially, scale them by 100 divided by their sum.
# print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
# result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
# capture.output(result, file = "S_contribution_LTPD_GBM.txt")
# # 
# anova with stndardized Lasso var
af.rf <- anova(st_lmod_LGBM)
afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
#  trivially, scale them by 100 divided by their sum.
print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
res_sort <- result[order(- result$PctExp),]  # sorting coeff
res_sort1 <- res_sort[-1,]
res_sort1 <- res_sort1[1:30,]
res_sort1
write.csv(res_sort1, file = "S_LTPD_GBMcontribution_stan_anova.csv")


#-----------------------------------------------------------------------------end GBM--------------------------



#-----------------------------------------------------------------------------end GBM--------------------------

   ###   NOT RUN






# ------------------------------------Random Forest -----------------------------------------------


####    XGBoost---------------------------------------
library(data.table)
require(gbm)
library(gbm)
df <- df6_fbcHTPD
#separating training and test data
set.seed(123)
smp_size <- floor(0.90 * nrow(df))

train_ind <- sample(seq_len(nrow(df)), size = smp_size, replace = FALSE  )

train <- df[train_ind, ]
test <- df[-train_ind, ]
#   AGAIN SOME ORIGINALFILE NAME POP UP :!!!
drop.cols <- grep("original_file_name$", colnames(df))
drop.cols
df <-df[, (drop.cols) := NULL]
df.boost=gbm(fbc ~ . ,data = df[train_ind, ],distribution = "gaussian",n.trees = 1000,
             shrinkage = 0.01, interaction.depth = 4)
df.boost
# time was about 1 min
#gbm(formula = fbc ~ ., distribution = "gaussian", data = df[train_ind, 
# ], n.trees = 1000, interaction.depth = 4, shrinkage = 0.01)
# A gradient boosted model with gaussian loss function.
# 1000 iterations were performed.
# There were 346 predictors of which 231 had non-zero influence.

s<-summary(df.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance
s 
s<- data.table(s)
vimp.gbm <-s[1:30,]
write.csv(vimp.gbm, "H_HTPD_GBMvarImp.csv", row.names=FALSE, na="")
#                                  var    rel.inf
# 1:                            level 41.9440284
# 2:                          cycling 15.0262106
# 3:       sfbc_b32wlsaup_drpost_pcs_  6.5805250
# 4:                vpgmslooptrial1__  4.6530062
# 5:               original_file_name  4.3365752
# 6:               vth_08pwl60_lt_mv_  2.7627158
# 7:            vth_wlds0last_l3s_mv_  1.9696172
# 8:   sfbc_t32wlsaerx_far_fresh_pcs_  1.9663316
# 9:                crd_scrnslcbot_p_  1.6540276
# 10:      sfbc_b32wlsalp_dc_post_pcs_  1.4953712
# 11:   tlcwc_we0_wl31_fr_s25_f2g_pcs_  1.4612984
# 12:    tlcwc_we0_wl0_fr_s25_f2g_pcs_  1.4413644
# 13: sfbc_t32wlsalp_far_dc_fresh_pcs_  1.2302215
# 14:         bbk_high_wldd0_ev_d_pcs_  0.9724982
# 15:                  vth_sgs_med_mv_  0.9172421
# 16:             crd_m1bll_224_c_pcs_  0.9039992
# 17:              layer3_vpgms_s_dac_  0.6226756
# 18:        wlleak_each_value_ds0_na_  0.5344709
# 19:       sfbc_b32wlsaerx_fresh_pcs_  0.4789742
# 20:      bbk_wlds0vth_3_u1_ev_c_pcs_  0.4594666
pred.boost <-gbm(formula = fbc ~ ., distribution = "gaussian", data = df[-train_ind], n.trees = 200, interaction.depth = 4, shrinkage = 0.01)
#A gradient boosted model with gaussian loss function.
#10000 iterations were performed.
#There were 13 predictors of which 13 had non-zero influence.

summary(pred.boost)
p_boost <- data.table(summary(pred.boost))
p_boost

library(ggplot2)
p <-ggplot(p_boost,aes(p_boost[1:20,2], p_boost[1:20,1]))

pred.boost$oobag.improve
#plot(p_boost,
     # i.var = 20,
     # n.trees = df.boost$n.trees,
     # continuous.resolution = 1,
     # return.grid = FALSE,
     # type = "link")


#Plot of Response variable with lstat variable
summary(p_boost,n.trees=best.iter)
# plot(df.boost,
#      i.var = 1:20,
#      n.trees = df.boost$n.trees,
#      continuous.resolution = 100,
#      return.grid = TRUE,
#      type = "link"
# )
#abline(model1, col="lightblue")
#Inverse relation with lstat variable

#plot(pred.doost,i="rm") 
#as the average number of rooms increases the the price increases

#Prediction on Test Set
#We will compute the Test Error as a function of number of Trees.

n.trees = seq(from=100 ,to=1000, by=100) #no of trees-a vector of 100 values 

#Generating a Prediction matrix for each Tree
predmatrix<-predict(df.boost,test,n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix
predmatrix <- as.vector(round(predmatrix))
test %>% dim
test_new <- data.table(cbind(test[,347],predmatrix))

test_new <- data.frame(test_new[1:200,])
test_new
library(graphics)
plot(test_new$predmatrix, test_new$fbc,  xlab = "prediction", ylab = "FBC",col = "red", main = "Rate of actual HTPD FBC rate against to predicted, 
     based Gradient Boosting Machine")            
# plot prediction as a slop for the 151 dies
abline(lm(test_new$predmatrix~ test_new$fbc) , data=df)
# plot  perfect prediction line red
abline(lm(test_new$predmatrix~ test_new$fbc,  + 0),  col="blue")


#--------------------------------------------------------------------------------

library(ggplot2)
gg_gbm <-ggplot(test_new)+
  geom_point(aes(x = test_new$predmatrix, y= test_new$fbc,color=predicted-actual),alpha=0.3)+
  ggtitle('Predicted vs Actual on test  HTPD data')

gg_gbm
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
 a <- ggplot(test_new, aes(x = test_new$predmatrix, y= test_new$fbc ))
 a + geom_jitter(height = 150,width = 150)
# b <- ggplot(test_new, aes(x= test_new$predmatrix, test_new$fbc))
# b + geom_bar(stat = "identity")

plot(test_new$fbc, test_new$predmatrix, xlabel= "actual fbc", ylabel= "Predicted fbc")
#Calculating The Mean squared Test Error
#test.error<-with(test,apply( (predmatrix-test$fbc)^2,2,mean))
#head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

#Plotting the test error vs number of trees



plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

#adding the RandomForests Minimum Error line trained on same data and similar parameters
abline(h = min(test.err),col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)


dim(predmatrix)


head(test.error)
a <- ggplot(test)

#
##----------       GBM    -----------------------------X--------------------------------------
df %>% dim
df <- df6_HTPD_clean
#nH <- intersect(h1_coef_slop, df6_HTPD_clean)
library(data.table)
require(gbm)
library(gbm)
df <- df6_HTPD_clean
df %>% dim
#separating training and test data
set.seed(123)
smp_size <- floor(0.90 * nrow(df))

train_ind <- sample(seq_len(nrow(df)), size = smp_size, replace = FALSE  )

train <- df[train_ind, ]
test <- df[-train_ind, ]
#

df.boost=gbm(fbc ~ . ,data = df[train_ind, ],distribution = "gaussian",n.trees = 1000,
             shrinkage = 0.01, interaction.depth = 4)
df.boost
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
write.csv(gbm.imp, "L_HTPD_GBMachine_subset.csv")
# var  rel.inf
# 1:                 ron_po33_t_ua_ 3.831665
# 2:          vth_wlds0prog_l3s_mv_ 2.870611
# 3:               veralooptrial1__ 2.778796
# 4:    sfbc_b32wlsalp_dc_post_pcs_ 2.731983
# 5:  tlcwc_we0_wl0_fr_s25_a2r_pcs_ 2.625051
# 6:                  vcg_br3_t_mv_ 2.610963
# 7:           vread_evfy_mlc_t_mv_ 2.601003
# 8:             vth_wldd01_l3s_mv_ 2.588986
# 9:                  vcg_dv3_t_mv_ 2.567874
# 10:         ron_odt_18_pn_rate_mv_ 2.388949
# 11:                    vddsa_t_mv_ 2.340955
# 12:                    vpgmmaxt_p_ 2.327618
# 13:                vreadk_n1_t_mv_ 2.097900
# 14: tlcwc_we0_wl63_fr_s25_f2g_pcs_ 2.087674
# 15:          wlleak_post_sgd13_na_ 2.002853
# 16:             tproga2slcrand_us_ 1.923708
# 17:                  vcg_gv3_t_mv_ 1.714834
# 18:       wlleak_each_value_53_na_ 1.679829
# 19:    sfbc_64wls01t0_dc_post_pcs_ 1.667886
# 20:                  vcg_erv_t_mv_ 1.662214
# 21:                  vcg_ar3_t_mv_ 1.622298
# 22:                    vref_s_dac_ 1.616701
# 23:          vpgmu_sgld_mon1_t_mv_ 1.596012
# 24:       bbk_high_wldd0_ev_d_pcs_ 1.558161
# 25:                  vcg_bv3_t_mv_ 1.539462
# 26:                 ron_no18_t_ua_ 1.525836
# 27:                    eires20x1__ 1.501419
# 28:                  wlrc_200_pcs_ 1.334044
# 29:    wlleak_each_value_sgd02_na_ 1.312432
# 30:             wlleak_post_32_na_ 1.288583
#var  rel.inf


pred.boost <-gbm(formula = fbc ~ ., distribution = "gaussian", data = df[-train_ind], n.trees = 1000, interaction.depth = 4, shrinkage = 0.01)
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
f.predict <- predict(df.boost,test[,-347],best.iter)
f.predict %>% plot

f1.predict <- predict(df.boost, train[,-347], n.trees = 100, type = "response")
f1.predict
gbm.preddf <- data.frame(test[,347], f1.predict)

head(data.frame("PredictedProbability" = f1.predict,"Actual" = train$fbc))
                
plot( f1.predict, train$fbc, 
    
      xlab = "prediction", ylab = "fbc",col = "dark red", 
      main = "Rate of actual HTPD FBC rate againt of predicted GBM") 
plot(f1.predict, train$fbc,   xlab = "prediction", ylab = "fbc",col = " red", main = "Rate of actual HTPD FBC growth against to predicted, 
     based on Gradient Boosting Machine")            
# plot prediction as a slop for the 151 dies
abline(lm( train$fbc ~ f1.predict, data=df))
# plot  perfect prediction line red
abline(lm(train$fbc ~ f1.predict + 0),  col="blue")

#-----------------------------------------------------------------------------end GBM--------------------------
# plot the performance # plot variable influence
summary(df.boost,n.trees=1) # based on the first tree
summary(df.boost,n.trees=best.iter) # based on the estimated best number of trees
# compactly print the first and last trees for curiosity
print(pretty.gbm.tree(df.boost,1))
print(pretty.gbm.tree(df.boost,df.boost$n.trees))

plot(df.boost,c(1,2,6),best.iter,cont=20)

show(pred.boost)

summary(df.boost,
        cBars=length(df.boost$shrinkage),
        n.trees=df.boost$n.trees,
        plotit=TRUE,
        order=TRUE,
        method=relative.influence,
        normalize=TRUE)







library(ggplot2)
p <-ggplot(p_boost,aes(p_boost[1:30,2], p_boost[1:30,1]))


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
predmatrix<-predict(df.boost,test[,-347], n.trees = n.trees)
predmatrix

dim(predmatrix)


head(test.error)
a <- ggplot(test)

# predict values in test set
y_pred <- predict(df.boost, data.matrix(test[,-347]))

# library(caret)
# library(dplyr)
# library(corrplot)
# library(plyr)
# library(rpart)
# library(randomForest)
# set.seed(5)
# 
# ## 75% of the sample size  or may be 80%  ???
# 
# hdf<- read.csv("D_HTPD_formodel.csv")
# hdf %>% dim
# df <- na.omit(hdf)
# df %>% dim
# set.seed(123)
# smp_size <- floor(0.90 * nrow(df))
# 
# train_ind <- sample(seq_len(nrow(df)), size = smp_size, replace = FALSE  )
# 
# train <- df[train_ind, ]
# test <- df[-train_ind, ]
# 
# #TRAIN
# train %>% dim
# names(train)
# tx <- model.matrix(fbc~. , -346, data=train )
# ty <- as.matrix(train[, 346]) # Only fbc
# ty %>% dim
# 
# # TEST
# test %>% dim
# names(test)
# y_test <- test[,346]
# y_test %>% summary  # we need to to campare result
# #test <- model.matrix(fbc~. , -351, data=test )
# xtest <- test[, -346]
# #Train Random Forest
# df %>% dim
# start.time <- Sys.time()
# 
# rf <-randomForest(df$fbc~.,data=df, teskeep.forest=FALSE, importance=TRUE,ntree=200)
# rf
# # Call:
# # randomForest(formula = df$fbc ~ ., data = df, teskeep.forest = FALSE,      importance = TRUE, ntree = 200) 
# # Type of random forest: regression
# # Number of trees: 200
# # No. of variables tried at each split: 115
# # 
# # Mean of squared residuals: Mean of squared residuals: 5.251101
# # % Var explained: 88.01
# 
# print(rf)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken
# #Time difference of 5.447682 mins
# 
# plot(rf , main =" Random forest HTPD 200 trees")
# 
# #    will see importence var
# impRF<-rf$importance    #worked
# importance(rf, type =1 )
# impRF = importance(rf, type =1)
# impRF <- data.frame(predictors=rownames(impRF),impRF)
# 
# rf$importanceSD
# rf %>% summary
# rf$mse
# # Order the predictor levels by importance 
# # I guess %IncMSE of j'th is (mse(j)-mse0)/mse0 * 100%   so the higher number more importent
# 
# imp.sortRF <- arrange(impRF,desc(impRF$X.IncMSE))
# imp.sortRF
# imp.sortRF$predictors <- factor(imp.sortRF$predictors,levels=imp.sortRF$predictors)
# imp.sortRF$predictors
# imp.sortRF
# # Select the top 20 predictors
# imp.30<- imp.sortRF[1:30,]
# print(imp.30)
# write.csv(imp.30, "D_HTPD_RandomForest_200t_30var.csv")
# 
# # Now we can compare the Out of Bag Sample Errors and Error on Test set
# # The above Random Forest model chose Randomly 4 variables to be considered at each split. We could now try all possible 13 predictors which can be found at each split.
# 
# oob.err=double(13)
# test.err=double(13)
# 
# 
# # let try to do tain and predict on test--------------------------------------------------------------------
# #ptm <- proc.time()
# start.time <- Sys.time()
# set.seed(123)
# rftrain <-randomForest(train$fbc ~., data = train, teskeep.forest=FALSE, importance=TRUE,ntree=300)
# rftrain
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken 
# #Time difference of 3.938105 mins
# # Call:
# # randomForest(formula = train$fbc ~ ., data = train, teskeep.forest = FALSE,      importance = TRUE, ntree = 300) 
# # Type of random forest: regression
# # Number of trees: 300
# # No. of variables tried at each split: 115
# # 
# # Mean of squared residuals: 2.603475
# # % Var explained: 78.74
# varImpPlot(rftrain)
# pred_rf <- predict(rftrain, test)
# pred_rf
# plot(pred_rf, alpha=.2)
# plot(rftrain)
# 
# matRF_H <- data.table(test$fbc, pred_rf)
# matRF_H %>% plot
# #------------------- PLOTING----------------------------------
# library(ggplot2)
# library(plot3D)
# library(randomForestSRC)
# library(ggRandomForests)
# ggplot(df_hclean)+
#   geom_boxplot(aes(y=fbc,x=cycling,fill=level),notch=T,notchwidth=0.8)+
#   facet_grid(~level,margins=T)+
#   theme(axis.text.x = element_text(angle = 50, hjust = 1))+
#   ylab(label='fbc')+
#   xlab('cycling')+
#   ggtitle('Plot of the FBC by cycling on t=85 ( HTPD) ')
# 
# p1=ggplot(df_hclean)+
#   geom_point(aes(y=fbc,x=cycling,color=as.factor(level)))+
#  # scale_x_log10()+scale_y_log10()+
#   facet_grid(~level)+
#   ylab('FBC')+
#   xlab('Cycling')+
#   ggtitle('Plot of the FBC by cycling with t=85 ( HTPD) ')
# p1
# varImpPlot(rftrain, main = "Random Forest on HTPD")
# # all plots saved as "P...HTPD"
# 
# ## plot Predicted VS Actual
# 
# pred<-predict(object=rftrain,newdata=test[,-347])
# actual<-test$fbc
# result.plot<-data.frame(actual=actual,predicted=pred)
# paste('Function Call: ', rftrain$call)
# result.plot
# # [1] "Function Call:  randomForest"  "Function Call:  train$fbc ~ ." "Function Call:  train"        
# # [4] "Function Call:  FALSE"         "Function Call:  TRUE"          "Function Call:  150" 
# 
# paste('Mean Squared error: ',mean(rftrain$mse))
# #[1] "Mean Squared error:  2.77181630549426"
# paste('Root Mean Squared error: ',mean(sqrt(rftrain$mse)))
# #[1] "Root Mean Squared error:  1.66195789615997"
# gg <-ggplot(result.plot)+
#   geom_point(aes(x=actual,y=predicted,color=predicted-actual),alpha=0.3)+
#   ggtitle('Predicted vs Actual on test  HTPD data')
# result.plot
# gg
# # add abline
# 
# # plot  perfect prediction line red
# reg <- lm(actual~predicted, data=result.plot )
# reg
# # Call:
# #   lm(formula = actual ~ predicted, data = result.plot)
# # 
# # Coefficients:
# #   (Intercept)    predicted  
# # -0.4143       1.0508  
# gg1 <-gg + geom_abline(intercept = -0.4143, slop = 1.0508, color = "light blue", linetype = "dashed", size = 1)
# ## perfect prediction
# reg <- lm(actual~predicted+ 0, data=result.plot )
# reg
# 
# 
# #abline(lm(actual ~ predicted +0, data=result.plot, col="green"))
# gg1 + geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 0.5)
# 
# 
# 
# gg_e <- gg_error(rftrain)
# plot(gg_e)
# plot(gg_rfsrc(rftrain), alpha=.2 )  
# 
# ## plot Predicted VS Actual
# 
# pred<-predict(object=rftrain,newdata=test[,-346])
# actual<-test$fbc
# result<-data.frame(actual=actual,predicted=pred)
# paste('Function Call: ', rftrain$call)
# 
# # Plot the VIMP rankings of independent variables.
# #rfsrc_tr <- rfsrc(fbc ~ ., data = train)
# plot(gg_vimp(rftrain), 20)
# 
# # ggplot(rftrain, aes(x=fbc, y=cycling, color=chas))+
# #   geom_point(alpha=.4)+
# #   geom_rug(data=rftrain %>% filter(cycling))+
# #   labs(y="", x=st.labs["mfbc"]) +
# #   #scale_color_brewer(palette="Set2")+
# #   facet_wrap(~fbc, scales="free_y", ncol=3)

