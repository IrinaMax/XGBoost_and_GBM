# new data of FBC try to fit parametrs with slop
##   -----HTPD work with diequal_lonf_format (partitons) ['BICS3_256G_D3','DAT6-EXTENDED-MLC','MLC','HTPD-HTCYC_EP25C_2P55VCC-T1_R25C']

# The code for DQ data, to fit linear regression for every die
library(glmnet)
library(dplyr)
library(plyr)
library(corrplot)
library(caret)
library(devtools)
library(data.table)

# just look at diequal table
r1 <- read.csv("A_RTPD_maxfbc_DQ.csv")
# on the cluster
##     b3_r <- read.csv("/home/irinam/BICS3/rtpd_maxfbc_0918.csv")
summary(r1)
# rename colomns for easy to use
r1 <- rename(r1,c("X_c6"="FBC", "a.lot"="lot", "a.wafer"="wafer","a.x"= "x", "a.y"= "y", "a.page_type" = "level", "a.cycling" = "cycling"))
r1 %>% names   # [1] "a.lot"     "a.wafer"   "a.x"       "a.y"       "a.blk"     "a.cycling" "X_c6" 
#which.max(h1$FBC)
outrt <- data.frame(which( r1$FBC >200, arr.ind = T))
outrt
r1[c(370,6674, 6707),]
#            lot wafer  x  y level cycling  FBC
#370  CP0938453     6 28 13    LP    3000  801
#6674 CP0938453     6 28 13    UP    3000  552
#6707 CP0938453     6 28 13    MP    3000 1182
# remove outlier
r1 <- r1[-c(370,6674, 6707),]

row.names(r1) <- 1:nrow(r1)
row.names(r1)
r1 %>% dim
r1
write.csv(r1, "B_RTPD_dq.csv", row.names = FALSE)
r1 %>% head(20)

## GLM to add slop 
library(data.table)   

datDTr <- data.table(r1,  key= c("wafer", "x", "y", "lot"))
datDTr %>%dim

r1_coef<-datDTr[, list(coef(lm(FBC~cycling))), by = key(datDTr)]  #give a matrix  of 304 with intercept and slop
r1_coef<- rename(r1_coef, c("V1" = "coeff" ))
r1_coef %>% names
write.csv(r1_coef, "dies_coef_r.csv")


# we need to take only every second line to pull the slop, the first lines are intercept
r1_coef_slop <- r1_coef[-seq(1, NROW(r1_coef), by = 2)]
r1_coef_slop %>% dim
r1_coef_slop <-as.data.frame(r1_coef_slop)
r1_coef_slop %>% head
# wafer  x  y       lot           V1
# 1     2  6 17 CP0938712 0.0016778645
# 2     2  8 16 CP0938712 0.0020694616
# 3     2  9 20 CP0938712 0.0021250425
# 4     2 10 20 CP0938712 0.0027715802
# 5     2 11 16 CP0938712 0.0021318354
# 6     2 12 12 CP0938712 0.0009097086
write.csv(r1_coef_slop, "S_rtpd_dq.csv", row.names = FALSE)


#----------------------END working with D/Q-------------------------------

library(glmnet)
library(dplyr)
library(plyr)
library(corrplot)
library(caret)
library(devtools)
library(data.table)
# work with my ds table with coeff 

df_rs<- read.csv("S_ds_rtpd_full.csv", header = TRUE, sep = ",", na.strings = c("NULL", "NaN", "NA"), stringsAsFactors = TRUE)
df <- df_rs # save original  df as df1

df %>% dim

nmr<- names(df)
nmr
write.csv(nmr, "S_rtpd_slop_names_fullset.csv")
str(df)
tail(df)
# identefy and remove fisrt 36colomn and last 5 and remove them
n1 <- names(df[,1:36])
n1
n2 <- names(df[, 1774:1778])
n2
df <- df[, -c(1:36, 1774:1778) ]


# sub("c.*", "", df[,1])
# names(df) = gsub(pattern = "b.*", replacement = "", x = names(df))

df %>% names %>%  head
df %>% dim
# [1]  304:1778
# remove first 2 letters in col names
names(df) <- substring(names(df) , 3)
substring(names(df) , 3)
names(df)
df2 <- df  # save after changes and continue to work with dfr
df %>% dim 
df %>%  names

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
#[1] 304 822
write.csv(df4, "S_RTPD_bifore_correlation.csv")
# names_b_cor <- names(df3)
# names_b_cor
# write.csv(names_b_cor, "names_b_cor.csv")

# find correlation
df.cor <- data.matrix(df)

df.cor <- cor(df.cor, use = "pairwise.complete.obs")
CorPath <- capture.output(cat(substr(outputPath,1,nchar(outputPath)-4),'_CorMatrix.csv',sep = ""))
write.csv(df.cor, CorPath, row.names=FALSE, na="")
write.csv(df.cor, "S_CorMatrix_RTPD.csv", row.names=FALSE, na="")

#The absolute values of pair-wise correlations are considered. If two variables have a high correlation, the function looks at the mean absolute correlation 
#of each variable and removes the variable with the largest mean absolute correlation.
hccor = findCorrelation(df.cor, cutoff = 0.75, names = TRUE)
hccor
df <- df[ , !names(df) %in% hccor]
print('Omitting dependent result')

cat('Number of elemenated columns:     ',length(hc),'\n')  #487 
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
write.csv(df, outputPath, row.names=FALSE, na="")

sink()
#
df$slop
# # I lost colomns of coeff, i'm going to add it from saving, I pull it from the first saved copy
df[,"slop"]<- df_rs$u.slop
df$coeff  # NULL not exist and  it should not
df$slop
df %>% names
df6 <- df   # last copy of clining dataframe
df %>% colnames

drop.cols <- grep("avro_file_name$", colnames(df))
drop.cols
drop.cols <- grep("original_file_name$", colnames(df))
drop.cols
df <-df[,-188]
df %>% colnames
df %>% dim
#
#write.csv(df, "S_HTPD_DS_clean.csv", row.names=FALSE, na="")

#we need to omit NA so we can use it in model so we finished with dim 
df <- na.omit(df)
df %>% dim
df6_RTPD_clean <- df
#[1] 302 352   after additional cleaning and removing 2 row with NA
#----------------------- This is complit file for safe and modeling ------------------------------------------------
write.csv(df, "S_RTPD_DS_clean.csv", row.names=FALSE, na="")

#      MODELING 
#-----------------LASSO--------------------------------------------------------
library(glmnet)
# model with CV and lambda min
set.seed(123)
df <- df6_RTPD_clean
df %>% dim
x <- model.matrix(slop~. , -351, data=df )
x[, 350:351]
df[, 350:351]
x %>% dim
y <- as.matrix(df[, 351]) # Only slop
y %>% dim
cv = cv.glmnet(x, y)
cv
cv %>% names
cv$lambda.min
#[1] 1.346153e-05
model1 = glmnet(x, y, type.gaussian="covariance",  lambda=cv$lambda.min, standardize=TRUE,standardize.response = TRUE)
model1
summary(model1)
# Call:  glmnet(x = x, y = y, lambda = cv$lambda.min, type.gaussian = "covariance") 

#Df  %Dev    Lambda
#[1,]21 0.3958 1.346e-05

model1 %>% names
model1$lambda
pred1 <- predict(model1, type="coefficients")
pred1

# I want to make the result table with coloms what inicialised in colomn i
temp <- pred1%>% summary
temp %>% names
temp$i
#  [1]   1   3  80  84  93 159 184 197 207 209 222 228 310 311 356 364 368 376 377
pr <- as.matrix(pred1)
pr
res <- data.frame(which( !pr ==0, arr.ind = T))
res$col<- NULL
res
res$sl_coef <- pr[which( !pr ==0, arr.ind = T)]
res %>% summary
res

write.csv(res, "S_Lasso_RTPD_mod1.csv")
eliminat <- data.frame(which( pr ==0, arr.ind = T))
eliminat <- eliminat[,1:2]
eliminat
write.csv(eliminat, "S_Var_NOT_inLasso_RTPD.csv")

##----------------LASSO 2-------------------------------  the same as the first  STANDARDIZED mae
set.seed(5)
x <- model.matrix(slop~. , -351, data=df )
y <- as.matrix(df[, 351]) # Only fbc
df %>% dim
cv.lasso <- cv.glmnet(x, y, nfold=10, alpha=1, parallel=TRUE, standardize=TRUE, standardize.response = TRUE, type.measure='mae')
cv.lasso
# Results
plot(cv.lasso, main = "S_RTPD LASSO coefficients capture with type.measure='mae'")
plot(cv.lasso$glmnet.fit, xvar = "lambda", label = TRUE, main ="S_RTPD LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568")
plot(cv.lasso$glmnet.fit, xvar = "dev", label = TRUE, main="S_RTPD LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568 ")
cv.lasso$lambda.min
#[1]  1.410251e-05
cv.lasso$lambda.1se
#[1]

coeff_lasso <- coef(cv.lasso, s=cv.lasso$lambda.min,exact=TRUE) [which(coef(cv.lasso, s = "lambda.min") != 0)]

coeff_lasso
#extract coefficient with min lambda and names of LASSO result
# c<-coef(glmnet2, s='lambda.min',exact=TRUE) 
# c
# coef_l <- coef(glmnet2, s='lambda.min',exact=TRUE) [which(coef(glmnet2, s = "lambda.min") != 0)]
colnames <- colnames(df)[which(coef(cv.lasso, s = "lambda.min") != 0)]
colnames
##  Updated frame of coeff with names of the variable
l_coeff <- coef(cv.lasso, s = "lambda.min")
l_name_coeff<- data.frame(name = l_coeff@Dimnames[[1]][l_coeff@i + 1], coefficient = l_coeff@x)
l_name_coeff_SR <- l_name_coeff[order(- l_name_coeff$coefficient),]  # sorting coeff
l_name_coeff_SR 
write.csv(l_name_coeff_SR, "S_coefLasso_RTPD_slop.csv")
# result in D_result of htpd" in Exel"

f <- as.vector(l_name_coeff_SR[,1])
f



####--------------------------    GLM based on the LASSO pded1-----------------------------
# try to make subset of variables picked by Lasso pred1 
# vector of columns you DON'T want
f <- as.vector(l_name_coeff_SR[,1])
f <- f[-1]
f %>% str
# subset of all data with selected  21 veriables
ss <-df%>% select(f)
ss %>% dim
ss %>% names
ss
# add responce 
df$slop
ss$slop <-df$slop

write.csv(ss, "S_RTPD_lasso2_subset.csv")
lmod_ss <- lm(slop~., ss)
lmod_ss
summary(lmod_ss)
lmod_summary <-lmod_ss %>% summary
capture.output(lmod_summary, file = "S_RTPD_lasso_GLM.txt")
#as.data.frame(l_name_coeff)
#op <- par(mfrow=c(1, 2))
plot(cv.lasso$glmnet.fit, "norm",   label=TRUE)
plot(cv.lasso$glmnet.fit, "lambda", label=TRUE)
#par(op)
predHTPD <- predict(cv.lasso,x, s= "lambda.min",type="response")
plot(predHTPD, y,  xlab = "prediction", ylab = "SLOP",col = "dark red", main = "Rate of actual RTPD FBC growth against to predicted, 
     based on the Lasso lambda min= 0.0001640196, CV: 10 Fold")            
# plot prediction as a slop for the 151 dies
abline(lm(y ~ predHTPD, data=df))
# plot  perfect prediction line red
abline(lm(y ~ predHTPD + 0),  col="blue")
df_lasso_predHTPD <- data.frame(predHTPD, y)
write.csv(df_lasso_predHTPD, "S_Actual_toPredicted_onLasso_RTPD.csv")

### 
####    XGBoost-----------------------------------------------GBM-----------------------------------------------------------
library(data.table)
require(gbm)
library(gbm)
df <- df6_RTPD_clean
df %>% dim
#separating training and test data
set.seed(123)
smp_size <- floor(0.90 * nrow(df))

train_ind <- sample(seq_len(nrow(df)), size = smp_size, replace = FALSE  )

train <- df[train_ind, ]
test <- df[-train_ind, ]
#

df.boost=gbm(slop ~ . ,data = df[train_ind, ],distribution = "gaussian",n.trees = 10000,
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
write.csv(gbm.imp, "S_RTPD_GBMachine_subset.csv")
# var  rel.inf
# 1:       sfbc_b32wlsalp_dc_post_pcs_ 9.597297
# 2:                   vreadk_n1_t_mv_ 8.424432
# 3:                    ron_po33_t_ua_ 5.966794
# 4:                  veralooptrial1__ 5.208814
# 5:                       eires20x1__ 4.147743
# 6:             wlleak_post_sgd13_na_ 4.117915
# 7:                     vcg_dv3_t_mv_ 4.054143
# 8:                tproga2slcrand_us_ 3.099449
# 9:                       vddsa_t_mv_ 2.950073
# 10:     tlcwc_we0_wl0_fr_s25_a2r_pcs_ 2.911394
# 11:          wlleak_each_value_01_na_ 2.314185
# 12:                wlleak_post_08_na_ 2.211929
# 13:       sfbc_64wls01t0_dc_post_pcs_ 2.178104
# 14:                wlleak_post_31_na_ 2.127876
# 15:        sfbc_b32wlsalp_drpost_pcs_ 2.070196
# 16:             vth_wlds0prog_l3s_mv_ 2.015152
# 17:          wlleak_each_value_30_na_ 2.011259
# 18:                wlleak_post_58_na_ 1.973091
# 19:          wlleak_each_value_53_na_ 1.910983
# 20:                     vcg_br3_t_mv_ 1.883990
# 21:                       vpgmmaxt_p_ 1.875202
# 22:        sfbc_b32wlsaup_drpost_pcs_ 1.183369
# 23:                wlleak_post_00_na_ 1.154848
# 24:                wlleak_post_17_na_ 1.133281
# 25:          wlleak_each_value_33_na_ 1.091577
# 26:               vpgms_sgdprog_t_mv_ 1.083660
# 27:                wlleak_post_44_na_ 1.082212
# 28:                wlleak_post_22_na_ 1.066470
# 29: bbk_mhopen1_000_vblc015_ev_c_pcs_ 1.066180
# 30:              vread_evfy_mlc_t_mv_ 1.052562
# var  rel.inf


pred.boost <-gbm(formula = slop ~ ., distribution = "gaussian", data = df[-train_ind], n.trees = 1000, interaction.depth = 4, shrinkage = 0.01)
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
f.predict <- predict(df.boost,test[,-354],best.iter)
f.predict %>% plot

f1.predict <- predict(df.boost, train[,-354], n.trees = 1500, type = "response")
f1.predict
gbm.preddf <- data.frame(test[,354], f1.predict)

head(data.frame("Actual" = train$slop, 
                "PredictedProbability" = f1.predict))
plot( f1.predict, train$slop, 
     
      xlab = "prediction", ylab = "SLOP",col = "dark red", 
      main = "Rate of actual RTPD FBC growth GBM")    

abline(lm(train$slop ~ f1.predict, data=df))
# plot  perfect prediction line red

abline(lm(train$slop ~ f1.predict + 0),  col="blue")


# plot the performance # plot variable influence
summary(df.boost,n.trees=1) # based on the first tree
summary(df.boost,n.trees=best.iter) # based on the estimated best number of trees
# compactly print the first and last trees for curiosity