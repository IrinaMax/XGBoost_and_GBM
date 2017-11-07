# new data of FBC try to fit parametrs with slop
##   -----LTPD work with diequal_lonf_format (partitons) ['BICS3_256G_D3','DAT6-EXTENDED-MLC','MLC','HTPD-HTCYC_EP25C_2P55VCC-T1_R25C']

# The code for DQ data, to fit linear regression for every die
library(glmnet)
library(dplyr)
library(plyr)
library(corrplot)
library(caret)
library(devtools)
library(data.table)

# just look at diequal table
l1 <- read.csv("A_LTPD_maxfbc_DQ.csv")
# on the cluster
##     b3_r <- read.csv("/home/irinam/BICS3/rtpd_maxfbc_0918.csv")
summary(l1)
# rename colomns for easy to use
l1 <- rename(l1,c("X_c6"="FBC", "a.lot"="lot", "a.wafer"="wafer","a.x"= "x", "a.y"= "y", "a.page_type" = "level", "a.cycling" = "cycling"))
l1 %>% names   # [1] "a.lot"     "a.wafer"   "a.x"       "a.y"       "a.blk"     "a.cycling" "X_c6" 
#which.max(h1$FBC)
outlt <- data.frame(which( l1$FBC >200, arr.ind = T))
outlt
l1[c(370, 6674, 6707),]
#      lot wafer  x  y level cycling  FBC
# 370  CP0938453     6 28 13    LP    3000  754
# 6674 CP0938453     6 28 13    UP    3000  472
# 6707 CP0938453     6 28 13    MP    3000 1017

# remove outlier
l1 <- l1[-c(370, 6674, 6707), ]
row.names(l1) <- 1:nrow(l1)
row.names(l1)
l1 %>% dim
l1 %>% head
write.csv(l1, "B_LTPD_dq.csv", row.names = FALSE)
head(l1)
l1 %>% head(20)

## GLM to add slop 
## GLM to add slop 
library(data.table)   

datDT <- data.table(l1,  key= c("wafer", "x", "y", "lot","level"))
datDT %>%dim

h1_coef<-datDT[, list(coef(lm(FBC~cycling))), by = key(datDT)]  #give a matrix  of 304 with intercept and slop
h1_coef<- rename(h1_coef, c("V1" = "coeff" ))
h1_coef %>% dim
write.csv(h1_coef, "dies_coef_L.csv")


# we need to take only every second line to pull the slop, the first lines are intercept
h1_coef_slop <- h1_coef[-seq(1, NROW(h1_coef), by = 2)]
h1_coef_slop  %>% head
h1_coef_slop %>% dim
#    wafer x  y       lot level        coeff
# 1:     2 6 17 CP0938712    LP 8.572970e-04
# 2:     2 6 17 CP0938712    MP 3.724627e-03
# 3:     2 6 17 CP0938712    UP 1.332101e-03
# 4:     2 8 16 CP0938712    LP 7.318955e-05
# 5:     2 8 16 CP0938712    MP 2.324203e-03
# 6:     2 8 16 CP0938712    UP 1.617193e-03
h1_coef_slop <-as.data.frame(h1_coef_slop)

write.csv(h1_coef_slop, "Sl_Ltpd_dq.csv", row.names = FALSE)

#----------------------END working with D/Q-------------------------------

library(glmnet)
library(dplyr)
library(plyr)
library(corrplot)
library(caret)
library(devtools)
library(data.table)
# work with my ds table with coeff 
ldf <- read.csv("K_LTPD_DS_full.csv")
ldf %>% dim
names(ldf) <- substring(names(ldf) , 3)
substring(names(ldf) , 3)
names(ldf)
df <- inner_join(h1_coef_slop, ldf, by= c("lot","wafer", "x", "y","level"), copy=F)
df %>% dim
dfs <-df
#df1 <- df

#[1] 10935  1782
##[1] 10935  1782
n1 <- names(df[,1:44])
n1
n2 <- names(df[, 1780:1781])  
n2
df <- df[, -c(1:44, 1780:1781) ]


#names(df) <- substring(names(df[,1:348]), 3) remove  first 2 elements from names

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
write.csv(nmr, "Ltpd_slop_names_fullset.csv")
str(df)
tail(df)
df %>% names %>%  head
df %>% dim
# [1]  304:1778
# remove first 2 letters in col names
# names(df) <- substring(names(df) , 3)
# substring(names(df) , 3)
# names(df)
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
#[1] 304 822
write.csv(df4, "S_LTPD_bifore_correlation.csv")
# names_b_cor <- names(df3)
# names_b_cor
# write.csv(names_b_cor, "names_b_cor.csv")

# find correlation
df.cor <- data.matrix(df)

df.cor <- cor(df.cor, use = "pairwise.complete.obs")
CorPath <- capture.output(cat(substr(outputPath,1,nchar(outputPath)-4),'_CorMatrix.csv',sep = ""))
write.csv(df.cor, CorPath, row.names=FALSE, na="")
write.csv(df.cor, "S_CorMatrix_LTPD.csv", row.names=FALSE, na="")

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
write.csv(df, outputPath, row.names=FALSE, na="")
df %>% dim
sink()
#

df[,"level"]<- dfs$level
df[,"slop"]<- dfs$coeff
#we need to omit NA so we can use it in model so we finished with dim 
df <- na.omit(df)
df %>% dim
df6_LTPD_clean <- df
write.csv(df, "S2_LTPD_DS_clean.csv", row.names=FALSE, na="")
df[,"wafer"]<- dfs$wafer
df[,"lot"]<- dfs$lot
df[,"x"]<- dfs$x
df[,"y"]<- dfs$y
df$level  # NULL not exist and  it should not
df$slop
df %>% names
df6 <- df   # last copy of clining dataframe
df %>% colnames
# df <- data.table(df)
# drop.cols <- grep("avro_file_name$", colnames(df))
# drop.cols
# df <-df[, (drop.cols) := NULL]
# drop.cols <- grep("vf_sk_mv_", colnames(df))
# drop.cols
# df <-df[, (drop.cols) := NULL]
# drop.cols <- grep("vf_dio_mv_", colnames(df))
# drop.cols
# df <-df[, (drop.cols) := NULL]
# drop.cols <- grep("vf_mode_mv_", colnames(df))
# drop.cols
# df <-df[, (drop.cols) := NULL]
# drop.cols <- grep("original_file_name", colnames(df))
# drop.cols
#df <-df[,-188]
df %>% colnames
df %>% dim
#
#write.csv(df, "S_HTPD_DS_clean.csv", row.names=FALSE, na="")

#we need to omit NA so we can use it in model so we finished with dim 
df <- na.omit(df)
df %>% dim
df6_LTPD_clean <- df
#

#----------------------- This is complit file for safe and modeling ------------------------------------------------
write.csv(df, "S2perdie_LTPD_DS_clean.csv", row.names=FALSE, na="")

#      MODELING 




#-----------------LASSO--------------------------------------------------------
# library(glmnet)
# # model with CV and lambda min
# set.seed(555)
# df <- df6_HTPD_clean
# df %>% dim
# x <- model.matrix(slop~. , -354, data=df )
# 
# x %>% names
# x <- data.table(x)
# x %>% names  ## we need to delete  "original_file_name$"
# x <- x[, -c(198:199)]
# 
# x %>% names
# x <- as.matrix(x)
# 
# x[, 350:354]
# df[, 350:354]
# x %>% dim
# y <- as.matrix(df[, 354]) # Only slop
# y %>% dim
# cv = cv.glmnet(x, y)
# cv
# cv %>% names
# cv$lambda.min
# #[1] 0.004164204
# model1 = glmnet(x, y, type.gaussian="covariance",  lambda=0.04164204, standardize=TRUE,standardize.response = TRUE)
# model1
# summary(model1)
# # Call:  glmnet(x = x, y = y, lambda = cv$lambda.min, type.gaussian = "covariance") 
# 
# #Df  %Dev    Lambda
# #[1,]21 0.3958 1.346e-05
# 
# model1 %>% names
# model1$lambda
# pred1 <- predict(model1, type="coefficients")
# pred1
# 
# # I want to make the result table with coloms what inicialised in colomn i
# temp <- pred1%>% summary
# temp %>% names
# temp$i
# #  [1]   1   3  80  84  93 159 184 197 207 209 222 228 310 311 356 364 368 376 377
# pr <- as.matrix(pred1)
# pr
# res <- data.frame(which( !pr ==0, arr.ind = T))
# res$col<- NULL
# res
# res$sl_coef <- pr[which( !pr ==0, arr.ind = T)]
# res %>% summary
# res
# 
# write.csv(res, "S_Lasso_RTPD_mod1.csv")
# eliminat <- data.frame(which( pr ==0, arr.ind = T))
# eliminat <- eliminat[,1:2]
# eliminat
# write.csv(eliminat, "S_Var_NOT_inLasso_RTPD.csv")

##----------------LASSO 2-------------------------------  the same as the first  STANDARDIZED mae
set.seed(5)
x <- model.matrix(slop~. , -343, data=df )
y <- as.matrix(df[, 343]) # Only fbc
y %>% head
cv.lasso <- cv.glmnet(x, y, nfold=10, alpha=1, parallel=TRUE, standardize=TRUE, standardize.response = TRUE, type.measure='mae')
cv.lasso
# Results
plot(cv.lasso, main = "LTPD LASSO coefficients,type.measure='mae'")
# 
plot(cv.lasso$glmnet.fit, xvar = "lambda", label = TRUE, main ="Ltpd LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568")
plot(cv.lasso$glmnet.fit, xvar = "dev", label = TRUE, main="Ltpd LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568 ")
cv.lasso$lambda.min
#[1]  7.55579e-08
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
l_name_coeff_SL <- l_name_coeff[order(- l_name_coeff$coefficient),]  # sorting coeff
l_name_coeff_SL %>% dim
write.csv(l_name_coeff_SL, "S_LTPD_coefLasso_slop.csv")
# result in D_result of htpd" in Exel"

f <- as.vector(colnames)
f



####--------------------------    GLM based on the LASSO pded1-----------------------------
# try to make subset of variables picked by Lasso pred1 
# vector of columns you DON'T want
colnames %>% str
f <- as.vector(colnames[-342])
f %>% str

# subset of all data with selected  21 veriables
ss <-df6_LTPD_clean%>% select(f)
ss %>% dim
ss %>% names
ss
# add responce 
df$slop
ss$slop <-df$slop
ss$slop
write.csv(ss, "S_LTPD_lassoVariables_subset.csv")
lmod_ss <- lm(slop~., ss)
lmod_ss
summary(lmod_ss)
lmod_summary <-lmod_ss %>% summary
capture.output(lmod_summary, file = "S_LTPD_lassoGLM.txt")
as.data.frame(l_name_coeff_SL)
#op <- par(mfrow=c(1, 2))
plot(cv.lasso$glmnet.fit, "norm",   label=TRUE)
plot(cv.lasso$glmnet.fit, "lambda", label=TRUE)
#par(op)
predLTPD <- predict(cv.lasso,x, s= "lambda.min",type="response")
plot(predLTPD, y,  xlab = "prediction", ylab = "SLOPE",col = "light blue", main = "Rate of actual LTPD FBC growth against to predicted, 
     based on the lambda.min, CV: 10 Fold")            
# plot prediction as a slop for the 151 dies
abline(lm(y ~ predLTPD, data=df))
# plot  perfect prediction line red
#abline(lm(y ~ predLTPD + 0),  col="blue")
df_lasso_predLTPD <- data.frame(predLTPD, y)
write.csv(df_lasso_predLTPD, "S_Actual_toPredicted_onLasso_LTPD.csv")
#
# anova with stndardized Lasso var
# standartization
un_st <- lapply(ss, scale)
st_lmod_un <- lm(slop~., un_st)
st_lmod_un %>% summary


af.rf <- anova(st_lmod_un)
afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
#  trivially, scale them by 100 divided by their sum.
print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
res_sort <- result[order(- result$PctExp),]  # sorting coeff
res_sort1 <- res_sort[-2,]
res_sort1 <- res_sort1
write.csv(res_sort1, file = "S_contribution_LTPD_Lasso_stan_anova.csv")

###   XGBOOST------------------
library(data.table)
require(gbm)
library(gbm)
df <- df6_LTPD_clean
df %>% dim
#separating training and test data
set.seed(123)
smp_size <- floor(0.90 * nrow(df))

train_ind <- sample(seq_len(nrow(df)), size = smp_size, replace = FALSE  )

train <- df[train_ind, ]
test <- df[-train_ind, ]
#
start.time <- Sys.time()
df.boost=gbm(slop ~ . ,data = df[train_ind, ],distribution = "gaussian",n.trees = 1000,
             shrinkage = 0.01, interaction.depth = 4)
df.boost
# time was about 5 min sec 
# gbm(formula = slop ~ ., distribution = "gaussian", data = df[train_ind, 
# ], n.trees = 1000, interaction.depth = 4, shrinkage = 0.01)
# A gradient boosted model with gaussian loss function.
# 1000 iterations were performed.
# There were 342 predictors of which 285 had non-zero influence.
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
s<-summary(df.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance
s 
s<- data.table(s)
gbm.imp <-  s[1:30,]
gbm.imp
write.csv(gbm.imp, "S_LTPD_GBMachine_subset.csv")
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

start.time <- Sys.time()
pred.boost <-gbm(formula = slop ~ ., distribution = "gaussian", data = test, n.trees = 500, interaction.depth = 4, shrinkage = 0.01)
#A gradient boosted model with gaussian loss function.
#500 iterations were performed, during less then 1 min! Time difference of 20.84609 secs
time.taken
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

summary(pred.boost)
p_boost <- data.table(summary(pred.boost))
p_boost %>% head(30)
#
# check performance using an out-of-bag estimator
# OOB underestimates the optimal number of iterations
best.iter <- gbm.perf(df.boost,method="OOB")
print(best.iter)
f.predict <- predict(df.boost,test[,-343],best.iter)
f.predict %>% plot

f1.predict <- predict(df.boost, train[,-343], n.trees = 1000, type = "response")
f1.predict
gbm.preddf <- data.frame(test[,343], f1.predict)

head(data.frame("Actual" = train$slop, 
                "PredictedProbability" = f1.predict))
plot( train$slop, 
      f1.predict,
      xlab = "prediction", ylab = "SLOPE",col = "light blue", 
      main = "Rate of actual LTPD FBC growth GBM")    
abline(lm(train$slop ~ f1.predict, data=test))

# plot the performance # plot variable influence
summary(df.boost,n.trees=1) # based on the first tree
summary(df.boost,n.trees=best.iter) # based on the estimated best number of trees
# compactly print the first and last trees for curiosity
print(pretty.gbm.tree(df.boost,1))
print(pretty.gbm.tree(df.boost,df.boost$n.trees))

plot(df.boost,c(1,2,6),best.iter,cont=20)

show(pred.boost)
#  Contribution ----------------------------------

# vector of columns we want
gbm.imp <- data.frame(gbm.imp)
gbm.imp %>% names
f <- as.vector(gbm.imp[,"var"])
f
#f <-f[-1]  # do need intercept
f %>% str
# subset of all data with selected  21 veriables
df %>% dim
# need to convert level to the 3 col with binary 
# df_m <- df
# df_l <- model.matrix(~df$level-1, data=df$level)
# df_l 
# df_m <- cbind(df_m, df_l)
# df_m %>% names
# 
# df_m %>% names
# #df_m$levelMP <- rename(df_m$`df$levelMP`)
# colnames(df_m)[347] <- "levelLP" 
# colnames(df_m)[348] <- "levelMP"
# colnames(df_m)[349] <- "levelUP"
# df_m <- df_m[, -"level"]
# df_m %>% names
ss <-df%>% select(f)
ss %>% dim
ss %>% names
ss
# add responce 
df$slop
ss$slop <- df$slop
write.csv(ss, "S_LTPD_GBM_DSsubset.csv")
lmod_GBM_Ltpd <- lm(slop~., ss)
lmod_GBM_Ltpd
summary(lmod_GBM_Ltpd)
lmod_GBM_Lt <-lmod_GBM_Ltpd %>% summary
capture.output(lmod_GBM_Lt, file = "S_LTPD_GMB_linearModel.txt")

# Linear model with standardizing predictors
ss %>% dim
ss %>% names

# standartization  # normalize
ss %>% names
ss1 <- lapply(ss, scale)

# ss$level <-as.numeric(ss$level)
# ss$level <- ss$level
ss1 %>% names
st_lmod_LGBM <- lm(slop~., ss1)
st_lmod_LGBM %>% summary



# Percentage of vriance explaned  Contribution Percent (%) (Ssi/SSt)
af.rf <- anova(lmod_GBM_Ltpd)
afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
#  trivially, scale them by 100 divided by their sum.
print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
capture.output(result, file = "S_contribution_LTPD_GBM.txt")

# anova with stndardized Lasso var
af.rf <- anova(st_lmod_LGBM)
afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
#  trivially, scale them by 100 divided by their sum.
print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
res_sort <- result[order(- result$PctExp),]  # sorting coeff
res_sort1 <- res_sort[-2,]
res_sort1 <- res_sort1[1:30,]
res_sort1
capture.output(res_sort1, file = "S_LTPD_GBM_contribution_stan_anova.txt")


#-----------------------------   some  extra plotting--------------------


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
predmatrix<-predict(df.boost,test[,-343], n.trees = n.trees)
predmatrix

dim(predmatrix)


head(test.error)
a <- ggplot(test)

# predict values in test set
y_pred <- predict(df.boost, data.matrix(test[,343]))

#Laaso with set bifore correlation
library(glmnet)
# model with CV and lambda min
set.seed(123)
#df <- df6_HTPD_clean  give onli one variable
df <- df4
df[,"slop"]<- df_hs$u.slop
drop.cols <- grep("avro_file_name$", colnames(df))
drop.cols
df <- df[,-410]
df %>% names
df %>% dim
x <- model.matrix(slop~. , -821, data=df )
#x[, 350:354]
#df[, 350:354]
x %>% dim
y <- as.matrix(df[, 821]) # Only slop
y %>% head
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
#[1,] 0    0 0.004164

model1 %>% names
model1$lambda
pred1 <- predict(model1, type="coefficients")
pred1

# I want to make the result table with coloms what inicialised in colomn i
temp <- pred1%>% summary
temp %>% names
temp$i
#  [1]    1   3   7  17  19  33  64  70  72  93 160 166 209 246 314 325 327 330 334 342 348 351
pr <- as.matrix(pred1)
pr
res <- data.frame(which( !pr ==0, arr.ind = T))
res$col<- NULL
res
res$sl_coef <- pr[which( !pr ==0, arr.ind = T)]
res %>% summary
res

write.csv(res, "S_Lasso_LTPD_modfor810var.csv")
eliminat <- data.frame(which( pr ==0, arr.ind = T))
eliminat <- eliminat[,1:2]
eliminat
write.csv(eliminat, "S_Var_NOT_inLasso_LTPD.csv")
