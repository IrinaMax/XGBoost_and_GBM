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
## GLM to add slop 
library(data.table)   

datDT <- data.table(r1,  key= c("wafer", "x", "y", "lot","level"))
datDT %>%dim

h1_coef<-datDT[, list(coef(lm(FBC~cycling))), by = key(datDT)]  #give a matrix  of 304 with intercept and slop
h1_coef<- rename(h1_coef, c("V1" = "coeff" ))
h1_coef %>% dim
write.csv(h1_coef, "dies_coef_R.csv")


# we need to take only every second line to pull the slop, the first lines are intercept
h1_coef_slop <- h1_coef[-seq(1, NROW(h1_coef), by = 2)]
h1_coef_slop  %>% head
h1_coef_slop %>% dim
# wafer x  y       lot level        coeff
# 1:     2 6 17 CP0938712    LP 0.0007976583
# 2:     2 6 17 CP0938712    MP 0.0037120190
# 3:     2 6 17 CP0938712    UP 0.0005239162
# 4:     2 8 16 CP0938712    LP 0.0008809399
# 5:     2 8 16 CP0938712    MP 0.0043663079
# 6:     2 8 16 CP0938712    UP 0.0009611370
h1_coef_slop <-as.data.frame(h1_coef_slop)

write.csv(h1_coef_slop, "Sl_rtpd_dq.csv", row.names = FALSE)

#----------------------END working with D/Q-------------------------------

library(glmnet)
library(dplyr)
library(plyr)
library(corrplot)
library(caret)
library(devtools)
library(data.table)
# work with my ds table with coeff 
rdf <- read.csv("K_RTPD_DS_full.csv")
rdf %>% dim
names(rdf) <- substring(names(rdf) , 3)
substring(names(rdf) , 3)
names(rdf)
df <- inner_join(h1_coef_slop, rdf, by= c("lot","wafer", "x", "y","level"), copy=F)
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
# [1] 569
drop.cols <- grep("original_file_name", colnames(df))
drop.cols
#[1] 568
df <-df[, (drop.cols) := NULL]
# sub("c.*", "", df[,1])
# names(df) = gsub(pattern = "b.*", replacement = "", x = names(df))

nmr<- names(df)
nmr
write.csv(nmr, "Rtpd_slop_names_fullset.csv")
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
#df <- df2
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
df[,"level"]<- dfs$level
df[,"slop"]<- dfs$coeff
#we need to omit NA so we can use it in model so we finished with dim 
df <- na.omit(df)
df %>% dim
df6_LTPD_clean <- df
write.csv(df, "S2_RTPD_DS_clean.csv", row.names=FALSE, na="")
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
df6_RTPD_clean <- df
#

#----------------------- This is complit file for safe and modeling ------------------------------------------------
write.csv(df6_RTPD_clean, "S2perdie_RTPD_DS_clean.csv", row.names=FALSE, na="")

#      MODELING 





##----------------LASSO 2-------------------------------  the same as the first  STANDARDIZED mae
set.seed(5)
x <- model.matrix(slop~. , -343, data=df )
y <- as.matrix(df[, 343]) # Only fbc
y %>% head
cv.lasso <- cv.glmnet(x, y, nfold=10, alpha=1, parallel=TRUE, standardize=TRUE, standardize.response = TRUE, type.measure='mae')
cv.lasso
# Results
plot(cv.lasso, main = "RTPD LASSO coefficients,type.measure='mae'")
# 
plot(cv.lasso$glmnet.fit, xvar = "lambda", label = TRUE, main ="Rtpd LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568")
plot(cv.lasso$glmnet.fit, xvar = "dev", label = TRUE, main="Rtpd LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568 ")
cv.lasso$lambda.min
#[1] 8.361571e-08
cv.lasso$lambda.1se
#[1] 1.64141e-06

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
write.csv(l_name_coeff_SR, "S_RTPD_coefLasso_slop.csv")
# result in D_result of htpd" in Excel"

f <- as.vector(colnames)
f



####--------------------------    GLM based on the LASSO pded1-----------------------------
# try to make subset of variables picked by Lasso pred1 
# vector of columns you DON'T want
colnames %>% str
f <- as.vector(colnames[-339])
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
cv.lasso$lambda.min
predRTPD <- predict(cv.lasso,x, s= "lambda.min",type="response")
plot(predRTPD, y,  xlab = "prediction", ylab = "SLOPE",col = "dark green", main = "Rate of actual RTPD FBC growth against to predicted, 
     based on the Lasso lambda min= 8.361571e-08, CV:10 Fold")            
# plot prediction as a slop for the 151 dies
abline(lm(y ~ predRTPD, data=df))
# plot  perfect prediction line red
#abline(lm(y ~ predHTPD + 0),  col="blue")
df_lasso_predRTPD <- data.frame(predRTPD, y)
write.csv(df_lasso_predRTPD, "S_Actual_toPredicted_onLasso_RTPD.csv")

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
write.csv(res_sort1, file = "S_contribution_RTPD_Lasso_stan_anova.csv")

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
start.time <- Sys.time()
df.boost=gbm(slop ~ . ,data = df[train_ind, ],distribution = "gaussian",n.trees = 1000,
             shrinkage = 0.01, interaction.depth = 4)
df.boost
# time was about 10 min sec 
#gbm(formula = slop ~ ., distribution = "gaussian", data = df[train_ind, 
# ], n.trees = 1000, interaction.depth = 4, shrinkage = 0.01)
# A gradient boosted model with gaussian loss function.
# 1000 iterations were performed.
# There were 342 predictors of which 279 had non-zero influence.
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
s<-summary(df.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance
s 
s<- data.table(s)
gbm.imp <-  s[1:30,]
gbm.imp$var
write.csv(gbm.imp, "S_RTPD_GBMachine_VarImp.csv")
# var  rel.inf
# 1:               vth_08pwl60_lt_mv_ 23.4352970
# 2:                crd_scrnslcbot_p_ 11.3861766
# 3:            vth_wlds0last_l3s_mv_  6.9807955
# 4:                vpgmulooptrial2__  3.9662408
# 5:       sfbc_b32wlsaup_drpost_pcs_  3.7014443
# 6:       sfbc_b32wlsalp_drpost_pcs_  3.4390672
# 7:         bbk_high_wldd0_ev_d_pcs_  3.1831522
# 8:      bbk_wlds0vth_1_u3_ev_c_pcs_  2.6746707
# 9: sfbc_t32wlsalp_far_dc_fresh_pcs_  1.9454911
# 10:          vpgm_slc_delta_loop_mv_  1.8287571
# 11:                vpgmslooptrial1__  1.7392790
# 12:          bbk_amhshort1_ev_d_pcs_  1.1792792
# 13:    tlcwc_we0_wl0_fr_s25_a2r_pcs_  1.0809447
# 14:    sfbc_t32wlsalp_far_fresh_pcs_  1.0726835
# 15:   sfbc_t32wlsaerx_far_fresh_pcs_  1.0608982
# 16:                        vreadt_p_  0.9146865
# 17:                ron_odt_18n_t_ua_  0.7893106
# 18:            frlt_vcgrsft_gr3_pcs_  0.7670603
# 19:       sfbc_t32wlsalp_drpost_pcs_  0.7618248
# 20:         wlleak_each_value_52_na_  0.7524932
# 21:                 celsrcstep_t_mv_  0.7425539
# 22:      bbk_wlds0vth_3_u1_ev_c_pcs_  0.7341602
# 23:   tlcwc_we0_wl31_fr_s25_f2g_pcs_  0.7340650
# 24:              layer3_vpgms_s_dac_  0.7072898
# 25:    sfbc_t32wlsaerx_dc_fresh_pcs_  0.7022621
# 26:      sfbc_b32wlsalp_dc_post_pcs_  0.6660584
# 27:           bbk_slcreadrand_d_pcs_  0.6127052
# 28:    tlcwc_we0_wl0_fr_s25_f2g_pcs_  0.5945723
# 29:                vth_wlds0_l3s_mv_  0.5910565
# 30:                  vth_sgs_win_mv_  0.5856621


test


pred.boost <-gbm(formula = slop ~ ., distribution = "gaussian", data = test, n.trees = 300, interaction.depth = 4, shrinkage = 0.01)
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
f.predict <- predict(df.boost,test[,-343],best.iter)
f.predict %>% plot

f1.predict <- predict(df.boost, train[,-343], n.trees = 500, type = "response")
f1.predict
gbm.preddf <- data.frame(test[,343], f1.predict)

head(data.frame("Actual" = train$slop, 
                "PredictedProbability" = f1.predict))
plot( f1.predict, train$slop, 
     
      xlab = "prediction", ylab = "SLOPE",col = "dark green", 
      main = "Rate of actual RTPD FBC growth GBM")    

abline(lm(train$slop ~ f1.predict, data=df))
# plot  perfect prediction line red

#abline(lm(train$slop ~ f1.predict + 0),  col="blue")


# plot the performance # plot variable influence
summary(df.boost,n.trees=1) # based on the first tree
summary(df.boost,n.trees=best.iter) # based on the estimated best number of trees
# compactly print the first and last trees for curiosity
print(pretty.gbm.tree(df.boost,1))
print(pretty.gbm.tree(df.boost,df.boost$n.trees))

plot(df.boost,c(1,2,3),best.iter,cont=25, type = "link")

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
write.csv(ss, "S_RTPD_GBM_subset.csv")
lmod_GBM_Rtpd <- lm(slop~., ss)
lmod_GBM_Rtpd
summary(lmod_GBM_Rtpd)
lmod_GBM_Rt <-lmod_GBM_Rtpd %>% summary
capture.output(lmod_GBM_Rt, file = "S_RTPD_GMB_linearModel.txt")

# Linear model with standardizing predictors
ss %>% dim
ss %>% names
# standartization  # normalize

ss %>% names
ss1 <- lapply(ss, scale)

# ss$level <-as.numeric(ss$level)
# ss$level <- ss$level
ss1 %>% names
st_lmod_rGBM <- lm(slop~., ss1)
st_lmod_rGBM %>% summary



# Percentage of vriance explaned  Contribution Percent (%) (Ssi/SSt)
af.rf <- anova(lmod_GBM_Rtpr)
afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
#  trivially, scale them by 100 divided by their sum.
print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
capture.output(result, file = "S_contribution_RTPD_GBM.txt")

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
capture.output(res_sort1, file = "S_RTPD_GBM_contribution_stan_anova.txt")

#-----------------------------------------------------------------------------end GBM--------------------------