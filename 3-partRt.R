# October14, cleaning new retreaved data from haddop
library(glmnet)
library(dplyr)
library(plyr)
library(corrplot)
library(caret)
library(devtools)
library(data.table)
r2 <- read.csv("K_RTPD_DS_full.csv", header = TRUE, sep = ",", na.strings = c("NULL", "NaN", "NA"), stringsAsFactors = TRUE)
r2 %>% dim
df <- r2
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
write.csv(nmr, "rtpd_fbc_names_fullset.csv")
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
# 822
cat('Number of columns with near zero variance: ,',length(num_a),'\n')
cat('Name of These columns: ,')
cat(num_a,'\n', sep = ',')
df4 <- df
df %>% dim
#[1] 7293  809
write.csv(df4, "R_RTPD_bifore_correlation.csv")
# names_b_cor <- names(df3)
# names_b_cor
# write.csv(names_b_cor, "names_b_cor.csv")

# find correlation
df.cor <- data.matrix(df)


df.cor <- cor(df.cor, use = "pairwise.complete.obs")
CorPath <- capture.output(cat(substr(outputPath,1,nchar(outputPath)-4),'_CorMatrix_RTPD.csv',sep = ""))
write.csv(df.cor, CorPath, row.names=FALSE, na="")
write.csv(df.cor, "R_CorMatrix_Rtpd.csv", row.names=FALSE, na="")

#The absolute values of pair-wise correlations are considered. If two variables have a high correlation, the function looks at the mean absolute correlation 
#of each variable and removes the variable with the largest mean absolute correlation.
hccor = findCorrelation(df.cor, cutoff = 0.75, names = TRUE)
hccor
df <- df[ , !names(df) %in% hccor]
print('Omitting dependent result')

cat('Number of elemenated columns:     ',length(hccor),'\n')
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
df$level <- df.c.f.t$level
df$cycling <- df.c.f.t$cycling
df$fbc <- df.c.f.t$fbc
df %>% dim
# [1] 7293  347
df %>% names

df <- data.table(df)
df <- na.omit(df)
df6_fbcRTPD <- df
# final resul after cleaning [1] [1] 3648  346

##  save as csv file for next step
write.csv(df, outputPath, row.names=FALSE, na="")
write.csv(df, "R_RTPD_formodel.csv", row.names=FALSE, na="")


#-----------------LASSO--------------------------------------------------------
# df <- df6_fbcRTPD
# #df[1,350:352]
# # model with CV and lambda min
# set.seed(5)
# df$fbc %>% summary
# # out1 <- data.frame(which( df$fbc > 200, arr.ind = T))
# # out1
# # no over 200 fbc
# #
# df <- na.omit(df)
# #df[!complete.cases(df),]
# #row.has.na <- apply(df, 1, function(x){any(is.na(x))})
# #row.has.na    ##  row 35 was been removed
# df %>% dim
# df %>% names
# x <- model.matrix(fbc~. , -345, data=df )
# # check df
# df[, 340:345] %>% head
# y <- as.matrix(df[, 345]) # Only fbc
# y %>% dim
# y %>% head
# cv = cv.glmnet(x, y)
# cv
# cv %>% names
# cv$lambda.min
# #[1] 0.0003358404
# model = glmnet(x, y, type.gaussian="covariance",  lambda=cv$lambda.min,standardize = TRUE, standardize.response = TRUE)
# model
# # Call:  glmnet(x = x, y = y, lambda = cv$lambda.min, standardize = TRUE,      type.gaussian = "covariance", standardize.response = TRUE) 
# # 
# # Df   %Dev  Lambda
# # [1,] 169 0.5646 0.002743
# summary(model)
# 
# plot(cv, main="RTPD LASSO coefficients capture based on Lambda.min 0.0004501")
# cv %>% names
# cv$lambda.min
# #[1]  0.001816965
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
# 
# # extracting names of the LASSO MODEL------------------------------------------------
# model %>% names
# model$lambda
# # [1] 0.0004500762
# pred1 <- predict(model_s, type="coefficients")
# pred1
# pred_300<-predict(model_s, newx = x[1:100,], s = "lambda.min")
# pred_300
# 
# pred2 <- predict(model_s, x, s="lambda.min",type="response")
# plot(pred_300,y)
# plot(pred2, y,  xlab = "prediction", ylab = "fbc",col = " green", main = "Expectation  fbc RTPD on model LASSO pred2, lambda min=0.0002788206") 
# abline(lm(y ~ pred2, data=df))
# # plot prediction as a slop for the 151 dies
# abline(lm(y ~ pred2 + 0),  col="blue")
# 
# # I want to make the result table with coloms
# temp <- pred1%>% summary
# temp$i
# 
# #
# 
# pr <- as.matrix(pred1)
# pr
# res <- data.frame(which( !pr ==0, arr.ind = T))
# res$col<- NULL
# res
# res$sl_coef <- pr[which( !pr ==0, arr.ind = T)]
# res %>% summary
# res 
# 
# write.csv(res, "R_RTPD_lasso1014.csv")
# res_sort <- res[order(- res$sl_coef),]  # sorting coeff
# res_sort
# write.csv(res_sort, "R_RTPD_lasso1014_sorted.csv")
######----------------LASSO 2-------------------------------------
set.seed(5)
df %>% dim
df %>% names
x <- model.matrix(fbc~. , -345, data=df )
#x %>% names
x %>% dim
y <- as.matrix(df[, 345]) # Only fbc
df %>% names
cv.lasso <- cv.glmnet(x, y, nfold=10, alpha=1, parallel=TRUE, standardize=TRUE, standardize.response = TRUE, type.measure='mae')
cv.lasso
# Results
plot(cv.lasso, main = "RTPD LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568 ")
plot(cv.lasso$glmnet.fit, xvar = "lambda", label = TRUE, main ="RTPD LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568")
plot(cv.lasso$glmnet.fit, xvar = "dev", label = TRUE, main="RTPD LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568")
cv.lasso$lambda.min
#[1]0.5224589
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
write.csv(l_name_coeff, "R_RTPD_Lasso_mae.csv")
# result in D_result of Rtpd" in Exel"

f <- as.vector(l_name_coeff[,1])
f



####--------------------------    LM based on the LASSO pded1-----------------------------
# try to make subset of variables picked by Lasso pred1 
# vector of columns you DON'T want
f <- as.vector(l_name_coeff[,1])
f <-f[-2]  # do need intercept
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
df_m <- df_m[, -"level"]
df_m %>% names
ss <-df_m%>% select(f)
ss %>% dim
ss %>% names
ss
# add responce 
df$fbc
ss$fbc <-df$fbc
write.csv(ss, "R_RTPDlasso_mae_subset.csv")
lmod_Lasso_Rtpd <- lm(fbc~., ss)
lmod_Lasso_Rtpd
summary(lmod_Lasso_Rtpd)
lmod_summary_Rtpr <-lmod_Lasso_Rtpd %>% summary
capture.output(lmod_summary_Rtpr, file = "R_RTPD_lasso_linearModel.txt")

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



# Percentage of vriance explaned  Contribution Percent (%) (Ssi/SSt)
# af.rf <- anova(lmod_Lasso_Rtpd)
# afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
# #  trivially, scale them by 100 divided by their sum.
# print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
# result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
# capture.output(result, file = "R_RTPD_contribution_Lasso.txt")

# anova with stndardized Lasso var
af.rf <- anova(st_lmod_rLasso)
afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
#  trivially, scale them by 100 divided by their sum.
print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
res_sort <- result[order(- result$PctExp),]  # sorting coeff
res_sort1 <- res_sort[-1,]
res_sort1 <- res_sort1[1:30,]
res_sort1
write.csv(res_sort1, file = "R_RTPD_Lassocontribution_stan_anova11.csv")

# Df      Sum Sq      Mean Sq    F value        Pr(>F)     PctExp
# levelMP                             1 1345.833786 1345.8337856 2517.64972  0.000000e+00 18.5786000
# crd_scrnslcbot_p_                   1  218.289739  218.2897389  408.35436  2.224805e-88  3.0133868
# layer4_vpgmu_s_dac_                 1  229.869404  229.8694040  430.01643  7.772937e-93  3.1732386
# layer3_vpgms_s_dac_                 1   12.289254   12.2892543   22.98949  1.661453e-06  0.1696474
# vth_wlds0last_l3s_mv_               1  208.210107  208.2101071  389.49841  1.727585e-84  2.8742422
# cycling                             1  832.598513  832.5985134 1557.54108 1.107740e-308 11.4936294
# sfbc_b32wlsaup_drpost_pcs_          1  318.649825  318.6498252  596.09786 1.406665e-126  4.3988104
# sfbc_b32wlsalp_dc_post_pcs_         1   65.193049   65.1930486  121.95656  3.955921e-28  0.8999593
# sfbc_t32wlsalp_far_dc_fresh_pcs_    1   43.406098   43.4060985   81.19974  2.572367e-19  0.5992007
# crd_m1bll_224_c_pcs_                1    7.770358    7.7703580   14.53600  1.386494e-04  0.1072661
# ron_no33_s_dac_                     1   63.378339   63.3783393  118.56179  2.129551e-27  0.8749081
# vpgmulooptrial2__                   1   32.576616   32.5766163   60.94104  6.709717e-15  0.4497048
# Residuals                        7232 3865.934911    0.5345596         NA            NA 53.3674063

##----------       GBM    -----------------------------X--------------------------------------
df %>% dim
df <- df6_fbcRTPD
df6_fbcRTPD [,340:345] %>% head
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
write.csv(gbm.imp, "R_RTPD_GBMachine_subset.csv")


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
test %>% names
test[,345] %>% dim
df.boost %>% dim
f.predict <- predict(df.boost,test[,-345], n.trees = 1200, type = "response")#best.iter)
f.predict
test %>% names
f1.predict <- predict(df.boost, train[,-345], n.trees = 1200, type = "response")
f1.predict
gbm.preddf <- data.frame(train[,345], f1.predict)

head(data.frame("PredictedProbability" = f1.predict,"Actual" = train$fbc))

plot( f.predict, test$fbc, 
      
      xlab = "prediction", ylab = "fbc",col = "dark green", 
      main = "Rate of actual HTPD FBC rate againt of predicted GBM") 

plot(train$fbc, f1.predict,   xlab = "Actual FBC", ylab = "Prediction",col = "dark green", main = "Rate of actual RTPD FBC growth against to predicted, 
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
write.csv(ss, "R_RTPD_GBM_subset.csv")
lmod_GBM_Rtpd <- lm(fbc~., sg)
lmod_GBM_Rtpd
summary(lmod_GBM_Rtpd)
lmod_GBM_Rtpr <-lmod_Lasso_Rtpd %>% summary
capture.output(lmod_GBM_Rtpr, file = "R_RTPD_GMB_linearModel.txt")

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
write.csv(res_sort1, file = "R_RTPD_contribution_GBM_stan_anova11.csv")


#-----------------------------------------------------------------------------end GBM--------------------------



###################  NOT RUN ################


# ------------------------------------Random Forest -----------------------------------------------
library(caret)
library(dplyr)
library(corrplot)
library(plyr)
library(rpart)
library(randomForest)
set.seed(5)

## 75% of the sample size  or may be 80%  ???

hdf<- read.csv("D_RTPD_formodel.csv")
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
xtest <- test[, -347]
#Train Random Forest
df %>% dim
start.time <- Sys.time()

rf <-randomForest(df$fbc~.,data=df, teskeep.forest=FALSE, importance=TRUE,ntree=200)
rf
# Call:
#   randomForest(formula = df$fbc ~ ., data = df, teskeep.forest = FALSE,      importance = TRUE, ntree = 200) 
# Type of random forest: regression
# Number of trees: 200
# No. of variables tried at each split: 117
# 
# Mean of squared residuals:  Mean of squared residuals: 5.251101
  ##  % Var explained: 88.01

print(rf)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

plot(rf , main =" Random forest 200 trees RTPD")

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
write.csv(imp.30, "R_RTPD_RandomForest_200t_30var.csv")


# let try to do tain and predict on test---------------Not nessesery-----------------------------------------------------
#ptm <- proc.time()
start.time <- Sys.time()
set.seed(123)
rftrain <-randomForest(train$fbc ~., data = train, teskeep.forest=FALSE, importance=TRUE,ntree=150)
rftrain
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken 
#Call:
# randomForest(formula = train$fbc ~ ., data = train, teskeep.forest = FALSE,      importance = TRUE, ntree = 150) 
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

matRF_H <- data.table(test$fbc, pred_rf)
matRF_H %>% plot
#------------------- PLOTING----------------------------------
library(ggplot2)
library(plot3D)
library(randomForestSRC)
library(ggRandomForests)
df <- df6_fbcRTPD
ggplot(df)+
  geom_boxplot(aes(y=fbc,x=cycling,fill=level),notch=T,notchwidth=0.8)+
  facet_grid(~level,margins=T)+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  ylab(label='fbc')+
  xlab('cycling')+
  ggtitle('Plot of the FBC by cycling on t=25 ( RTPD) ')

p1=ggplot(df)+
  geom_point(aes(y=fbc,x=cycling,color=as.factor(level)))+
  # scale_x_log10()+scale_y_log10()+
  facet_grid(~level)+
  ylab('FBC')+
  xlab('Cycling')+
  ggtitle('Plot of the FBC by cycling with t=25 ( RTPD) ')
p1
varImpPlot(rftrain, main = "Random Forest on RTPD")
# all plots saved as "P...RTPD"

## plot Predicted VS Actual

pred<-predict(object=rftrain,newdata=test[,-34])
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
  ggtitle('Predicted vs Actual on test data')
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

pred<-predict(object=rftrain,newdata=test[,-351])
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