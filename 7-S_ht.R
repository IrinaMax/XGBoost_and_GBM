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
h1 <- read.csv("A_HTPD_maxfbc_DQ.csv")
# on the cluster
##    h1 <- read.csv("/home/irinam/BICS3/rtpd_maxfbc_0918.csv")
summary(h1)
# rename colomns for easy to use
h1 <- rename(h1,c("X_c6"="FBC", "a.lot"="lot", "a.wafer"="wafer","a.x"= "x", "a.y"= "y", "a.page_type" = "level", "a.cycling" = "cycling"))
h1 %>% names   # [1] "a.lot"     "a.wafer"   "a.x"       "a.y"       "a.blk"     "a.cycling" "X_c6" 
#which.max(h1$FBC)
outht <- data.frame(which( h1$FBC >200, arr.ind = T))
outht
h1[c(152,3320, 3336),]
h1 <- h1[-c(152,3320, 3336),]
#            lot wafer  x  y level cycling  FBC
# 152  CP0938453     6 28 13    LP    3000  826
# 3320 CP0938453     6 28 13    UP    3000  473
# 3336 CP0938453     6 28 13    MP    3000 1116
# remove outlier
row.names(h1) <- 1:nrow(h1)
row.names(h1)
h1 %>% dim
h1
write.csv(h1, "B_HTPD_dq.csv", row.names = FALSE)
head(h1)
h1 %>% head(20)

## GLM to add slop 
library(data.table)   

datDT <- data.table(h1,  key= c("wafer", "x", "y", "lot","level"))
datDT %>%dim

h1_coef<-datDT[, list(coef(lm(FBC~cycling))), by = key(datDT)]  #give a matrix  of 304 with intercept and slop
h1_coef<- rename(h1_coef, c("V1" = "coeff" ))
h1_coef %>% dim
write.csv(h1_coef, "dies_coef_h.csv")


# we need to take only every second line to pull the slop, the first lines are intercept
h1_coef_slop <- h1_coef[-seq(1, NROW(h1_coef), by = 2)]
h1_coef_slop  %>% head
h1_coef_slop %>% dim
# wafer  x  y       lot        coeff
# 1:     2  6 17 CP0938712 0.0007351996
# 2:     2  8 16 CP0938712 0.0007486906
# 3:     2 11 16 CP0938712 0.0003934687
# 4:     2 12 12 CP0938712 0.0003496718
# 5:     2 12 17 CP0938712 0.0003597838
# 6:     2 13  9 CP0938712 0.0018374102
h1_coef_slop <-as.data.frame(h1_coef_slop)

write.csv(h1_coef_slop, "Sl_htpd_dq.csv", row.names = FALSE)

#----------------------END working with D/Q-------------------------------

library(glmnet)
library(dplyr)
library(plyr)
library(corrplot)
library(caret)
library(devtools)
library(data.table)
# work with my ds table with coeff 
hdf <- read.csv("K_HTPD_DS_full.csv")
hdf %>% dim
names(hdf) <- substring(names(hdf) , 3)
substring(names(hdf) , 3)
names(hdf)
df <- inner_join(h1_coef_slop, hdf, by= c("lot","wafer", "x", "y"), copy=F)
df %>% dim
df1 <- df
#[1] 10935  1782
##[1] 10935  1782
n1 <- names(df[,1:44])
n1
n2 <- names(df[, 1780:1782])  
n2
df <- df[, -c(1:44, 1780:1782) ]


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
write.csv(nmr, "htpd_slop_names_fullset.csv")
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
write.csv(df4, "S_HTPD_bifore_correlation.csv")
# names_b_cor <- names(df3)
# names_b_cor
# write.csv(names_b_cor, "names_b_cor.csv")

# find correlation
df.cor <- data.matrix(df)

df.cor <- cor(df.cor, use = "pairwise.complete.obs")
CorPath <- capture.output(cat(substr(outputPath,1,nchar(outputPath)-4),'_CorMatrix.csv',sep = ""))
write.csv(df.cor, CorPath, row.names=FALSE, na="")
write.csv(df.cor, "S_CorMatrix_HTPD.csv", row.names=FALSE, na="")

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

sink()
#

# # I lost colomns of coeff, i'm going to add it from saving, I pull it from the first saved copy
df[,"slop"]<- df1$coeff
df$coeff  # NULL not exist and  it should not
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
df6_HTPD_clean <- df
#

#----------------------- This is complit file for safe and modeling ------------------------------------------------
write.csv(df, "S1_HTPD_DS_clean.csv", row.names=FALSE, na="")

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
x <- model.matrix(slop~. , -342, data=df )
y <- as.matrix(df[, 342]) # Only fbc
y %>% head
cv.lasso <- cv.glmnet(x, y, nfold=10, alpha=1, parallel=TRUE, standardize=TRUE, standardize.response = TRUE, type.measure='mae')
cv.lasso
# Results
plot(cv.lasso, main = "HTPD LASSO coefficients,type.measure='mae'")
# 
plot(cv.lasso$glmnet.fit, xvar = "lambda", label = TRUE, main ="htpd LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568")
plot(cv.lasso$glmnet.fit, xvar = "dev", label = TRUE, main="htpd LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568 ")
cv.lasso$lambda.min
#[1] 4.956717e-06
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
write.csv(l_name_coeff_SR, "S_HTPD_coefLasso_slop.csv")
# result in D_result of htpd" in Exel"

f <- as.vector(colnames)
f



####--------------------------    GLM based on the LASSO pded1-----------------------------
# try to make subset of variables picked by Lasso pred1 
# vector of columns you DON'T want
colnames %>% str
f <- as.vector(colnames[-148])
f %>% str
# subset of all data with selected  21 veriables
ss <- df6_HTPD_clean%>% select(f)
ss %>% dim
ss %>% names
ss
# add responce 
#df$slop
#ss$slop <-df$slop

write.csv(ss, "S_HTPD_lasso2_subset.csv")
lmod_ss <- lm(slop~., ss)
lmod_ss
summary(lmod_ss)
lmod_summary <-lmod_ss %>% summary
capture.output(lmod_summary, file = "S_HTPD_lasso_GLM.txt")
#as.data.frame(l_name_coeff)
#op <- par(mfrow=c(1, 2))
plot(cv.lasso$glmnet.fit, "norm",   label=TRUE)
plot(cv.lasso$glmnet.fit, "lambda", label=TRUE)
#par(op)
predHTPD <- predict(cv.lasso,x, s= "lambda.min",type="response")
plot(predHTPD, y,  xlab = "prediction", ylab = "SLOP",col = "dark red", main = "Rate of actual HTPD FBC growth against to predicted, 
     based on the lambda min= 4.956717e-06, CV: 10 Fold")            
# plot prediction as a slop for the 151 dies
abline(lm(y ~ predHTPD, data=df))
# plot  perfect prediction line red
#abline(lm(y ~ predHTPD + 0),  col="blue")
df_lasso_predHTPD <- data.frame(predHTPD, y)
write.csv(df_lasso_predHTPD, "S_Actual_toPredicted_onLasso_HTPD.csv")




####    XGBoost----------------------------------------------------------------------------------------------------------
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
write.csv(gbm.imp, "S_HTPD_GBMachine_var_imp.csv")
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
plot( train$slop, 
      f1.predict,
xlab = "prediction", ylab = "SLOP",col = "dark red", 
main = "Rate of actual HTPD FBC growth GBM")    


# plot the performance # plot variable influence
summary(df.boost,n.trees=1) # based on the first tree
summary(df.boost,n.trees=best.iter) # based on the estimated best number of trees
# compactly print the first and last trees for curiosity
print(pretty.gbm.tree(df.boost,1))
print(pretty.gbm.tree(df.boost,df.boost$n.trees))

plot(df.boost,c(1,2,6),best.iter,cont=20)

show(pred.boost)
shrink.gbm(df.boost,
           n.trees=n.trees,
           lambda = rep(5, length(df.boost$var.names))
         )


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
predmatrix<-predict(df.boost,test[,-352], n.trees = n.trees)
predmatrix

dim(predmatrix)


head(test.error)
a <- ggplot(test)

# predict values in test set
y_pred <- predict(df.boost, data.matrix(test[,-352]))

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
df <- na.omit(df)
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

write.csv(res, "S_Lasso_HTPD_mod1.csv")
eliminat <- data.frame(which( pr ==0, arr.ind = T))
eliminat <- eliminat[,1:2]
eliminat
write.csv(eliminat, "S_Var_NOT_inLasso_HTPD.csv")

