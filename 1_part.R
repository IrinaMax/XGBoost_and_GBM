library(glmnet)
library(dplyr)
library(plyr)
library(corrplot)
library(caret)
library(devtools)
library(data.table)


#  diequal table--------- htpd
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
write.csv(h1, "
          ", row.names = FALSE)
head(h1)
h1 %>% head(20)
#-----------------------------------rtpd-----------
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
r1 %>% head
write.csv(r1, "B_RTPD_dq.csv", row.names = FALSE)
r1 %>% head(20)
#-------------------------------------------ltpd----------------------------------------
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
#-----------------------------   END OF CLEANING DQ -----------------
##   stuck all DQ together
ht <-h1 
rt <-r1
lt <-l1

ht$tem <- rep(85, nrow(ht))   # new col with temt 85
rt$tem<- rep(25, nrow(rt))   # new col with temt 25
lt$tem<- rep(-15, nrow(lt))   # new col with temt -15

# stuck all data together with cycling, FBC and temerature
st_dq <- rbind(ht,rt,lt)
st_dq %>% dim
#[1]  18231    8
st_dq %>% head
st_dq[18200, ]
write.table(st_dq, "E_StuckDQ_HTRTPD_withTemp.csv", sep = ',', row.names = FALSE)

##-----------------

# base on the clean files I created 3 table on HDFS and used then to retreave data from D/S
# SELECT *
#   FROM diesort.ds_bics_t6rn1_sme1_plot c 
# join irina_db.b_3temp1 u 
# on u.lot=(regexp_extract(c.lotno,'(.*)\\.',1))
# and u.x=c.originaldiex 
# and u.y=c.originaldiey
# and cast(u.wafer as int)=cast(regexp_extract(c.waferno, '.*\\.(\\d+)',1) as int);

crosst <- read.csv("E_3temp_ds_full.csv", header = TRUE, sep = ",", na.strings = c("NULL", "NaN", "NA"), stringsAsFactors = TRUE)
## split data for 3 sets to study separate
crosst %>% dim
subH <-  subset(crosst, crosst$u.tem == '85')
subH$u.tem <- NULL
subH[,1775:1780] %>% head
write.table(subH, "K_HTPD_DS_full.csv", sep = ',', row.names = FALSE)
subR <-  subset(crosst, crosst$u.tem == '25')
subR %>% dim
subR$u.tem <- NULL
subR[,1775:1780] %>% head

write.table(subR, "K_RTPD_DS_full.csv", sep = ',', row.names = FALSE)
subL <-  subset(crosst, crosst$u.tem == '-15')
subL %>% dim
subL$u.tem <- NULL
subL[,1775:178] %>% head
write.table(subL, "K_LTPD_DS_full.csv", sep = ',', row.names = FALSE)


crosst[,1773:1781] %>% head
df <- crosst
# make copy of original table and work with copy in case if something going wrong...

df %>% dim 
#  3648 1781
# 

#names(df) <- substring(names(df[,1:348]), 3) remove  first 2 elements from names
nm<- names(df)
nm
str(df)
tail(df)# I am going to save cycling fbc and tem columns for future
df.c.f.t <- df[,c(1774:1781)]
df.c.f.t %>% head

# identefy and remove fisrt 36colomn and last 5 and remove them
n1 <- names(df[,1:36])
n1
# [1] "c.key"              "c.product"          "c.module"           "c.process"          "c.lotno"           
# [6] "c.testerno"         "c.stageno"          "c.foupno"           "c.slotno"           "c.waferid"         
# [11] "c.waferno"          "c.dsstartdate"      "c.dsenddate"        "c.testqty"          "c.wafersize"       
# [16] "c.testerrecipe"     "c.testerpgm"        "c.proberrecipe"     "c.chucktemp"        "c.preformanceboard"
# [21] "c.probecard"        "c.notch"            "c.userid"           "c.originaldiex"     "c.originaldiey"    
# [26] "c.indexno"          "c.dutno"            "c.passfail"         "c.bin_pcs_"         "c.ds_xadd__"       
# [31] "c.ds_yadd__"        "c.ds_r__"           "c.ftestnum__"       "c.fstress__"        "c.param__"         
# [36] "c.vf_mode_mv_"
n2 <- names(df[, 1774:1781])
n2
#[1] "u.lot"     "u.wafer"   "u.x"       "u.y"       "u.level"   "u.cycling" "u.fbc"
# cut away n1 and n2
df <- df[, -c(1:36, 1774:1781) ]
df <- data.table(df)
drop.cols <- grep("avro_file_name$", colnames(df))
drop.cols
df <-df[, (drop.cols) := NULL]

drop.cols <- grep("original_file_name", colnames(df))
drop.cols
df <-df[, (drop.cols) := NULL]

# sub("c.*", "", df[,1])
# names(df) = gsub(pattern = "b.*", replacement = "", x = names(df))

df %>% names 
# remove first 2 simbols in col names to make it easy to read
names(df) <- substring(names(df) , 3)

df1 <- df  # save original  df as df1
df %>% dim 
# 3648 1737
df %>%  names

cat('Number of Columns in the original table: ' , ncol(df),'\n')
#cat(ncol(df),'\n')   # 1658 

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

# # cleaning rows with na
# rna <- (rowSums(is.na(df) | df=='', na.rm = TRUE)/ncol(df) > 0.2)
# rna1 <- rna[rna == TRUE]
# df <- df[!rna,]
# cat('Number of rows with more than 20% missing values: ',sum(rna),'\n')

# cleaning all columns close to 0
nzv1 <- nearZeroVar(df, saveMetrics= TRUE)
num_a<-row.names(nzv1)[nzv1$nzv == TRUE]
df<-df[, !names(df) %in% num_a]
print('Omitting nzv result')
print(ncol(df))
# 811
cat('Number of columns with near zero variance: ,',length(a),'\n')
cat('Name of These columns: ,')
cat(a,'\n', sep = ',')
df3 <- df
# names_b_cor <- names(df3)
# names_b_cor
# write.csv(names_b_cor, "names_b_cor.csv")

# find correlation
df.cor <- data.matrix(df)

df.cor <- cor(df.cor, use = "pairwise.complete.obs")
CorPath <- capture.output(cat(substr(outputPath,1,nchar(outputPath)-4),'_CorMatrix_CrossTemp.csv',sep = ""))
write.csv(df.cor, CorPath, row.names=FALSE, na="")
write.csv(df.cor, "CorMatrix_Crosstemp.csv", row.names=FALSE, na="")

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
# now I will add back cycling, FBC, level and temper 
df %>% dim
df.c.f.t %>% names
df$tem <- df.c.f.t$u.tem
df$ level <- df.c.f.t$u.level
df$cycling <- df.c.f.t$u.cycling
df$fbc <- df.c.f.t$u.fbc
df %>% dim
# [1] 3648   347
df[,348:354] %>% head

library(data.table)
df <- data.table(df)
drop.cols <- grep("avro_file_name$", colnames(df))
drop.cols
# 197
df[, (drop.cols) := NULL]
df %>% dim
drop.cols <- grep("original_file_$", colnames(df))
drop.cols
df6_3temp <- df
# final resul after cleaning [1] [1] 3648  346

##  save as csv file for next step
write.csv(df, outputPath, row.names=FALSE, na="")
write.csv(df6_3temp, "E_CrossTemp_formodel.csv", row.names=FALSE, na="")


#-----------------LASSO--------------------------------------------------------
df <- df6_3temp
df[,340:353] %>% head
# model with CV and lambda min
set.seed(777)
df$fbc %>% summary
out1 <- data.frame(which( df$fbc > 200, arr.ind = T))
out1
#df[c(2833, 2853, 2854), 347]
# fbc
# 1:  826
# 2:  473
# 3: 1116
#df <- df[-c(2833, 2853, 2854),]
df <- na.omit(df)
#df[!complete.cases(df),]
#row.has.na <- apply(df, 1, function(x){any(is.na(x))})
#row.has.na    ##  row 35 was been removed
df_hclean <- df
df %>% dim
df %>% names

x <- model.matrix(fbc~. , -353, data=df )
x %>% names
x <- data.table(x)
x %>% names  
## we need to delete  "original_file_name$"-----------------------------------TRASH???_____________________
#x <- x[, -c(189:214)]

x %>% names
x <- as.matrix(x)
x %>% dim
x[, 340:354] %>% head
df[, 340:346] %>% head
y <- as.matrix(df[, 353]) # Only fbc
y %>% head
cv = cv.glmnet(x, y)
cv
cv %>% names
cv$lambda.min
#[1]0.0003424189
model = glmnet(x, y, type.gaussian="covariance",  lambda=cv$lambda.min,standardize = TRUE, standardize.response = TRUE)
model
# Call:  glmnet(x = x, y = y, lambda = cv$lambda.min, standardize = TRUE,      type.gaussian = "covariance", standardize.response = TRUE) 
# 
# Df   %Dev  Lambda
# [1,] 350 0.6667 0.0003424
summary(model)

plot(cv, main="LTPD LASSO coefficients capture based on Lambda.min  ")
cv %>% names
cv$lambda.min
#[1] 0.0003424189

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
model %>% names
model$lambda
# [1]0.0003424189
# pred1 <- predict(model_s, type="coefficients")
# pred1
# pred_300<-predict(model_s, newx = x[1:300,], s = "lambda.min")
# pred_300
df$tem
pred2 <- predict(model_s,x, s="lambda.min",type="response")
plot(pred2,y)
plot(pred2, y,  xlab = "prediction", ylab = "fbc",col = "orange", main = "Prediction VS Actual for Cross Temperature
     of on model Lasso, lambda min= 0.0003424189") 
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
#                                  row       sl_coef
# (Intercept)                        1  1.565442e+01
# crd_ttl_pl0_p_                     5 -5.534852e-03
# vx2_s_dac_                        22 -2.604081e-02
# vcg_av3_t_mv_                     45  2.507298e-06
# vpass3_t_mv_                      64 -1.683300e-04
# psdovccq_t_mv_                    69 -4.372255e-04
# ron_po18_s_dac_                   78 -3.985157e-03
# ron_po33_t_ua_                    80 -1.780538e+00
# ron_no33_s_dac_                   81 -1.900668e-02
# fbc_sdllk4_pcs_                  163 -6.863496e-05
# tempcode_b9_pcs_                 166 -1.528051e-02
# crd_m1bll_224_c_pcs_             168  4.454403e-06
# vreads_p_                        174  7.415582e-02
# crd_m1bll_slc1_p_                189  1.153987e-02
# layer3_vpgms_s_dac_              196  7.892574e-02
# treadm8k_us_                     205 -1.278913e-01
# vth_wlds0last_l3s_mv_            215  2.842987e-04
# wlleak_post_00_na_               220 -3.356609e-04
# vth_12pwl3_lt_mv_                289  3.066994e-04
# vth_08pwl60_lt_mv_               290  2.138443e-04
# prgloop_wl00_mv_                 303 -2.490419e-02
# frlt_vcgrsft_ar3_pcs_            304 -3.402689e-02
# tlcwc_we0_wl0_fr_s25_f2g_pcs_    312  1.588261e-02
# tlcwc_we0_wl31_fr_s25_f2g_pcs_   314  7.862807e-03
# sfbc_b32wlsaup_drpost_pcs_       331  1.363392e-04
# sfbc_slcerslp_5kpost_pcs_        334 -1.190453e-02
# sfbc_slcerslp_7kpost_pcs_        335 -6.842064e-02
# crd_scrnslcbot_p_                339  3.337523e-01
# vpgmulooptrial2__                344 -7.369102e-02
# bbk_wlmh_6_d_pcs_                347 -6.157009e-03
# bbk_high_wldd0_ev_d_pcs_         348  1.177988e-06
# sfbc_t32wlsalp_far_dc_fresh_pcs_ 349  1.297020e-05
# sfbc_t32wlsalp_far_fresh_pcs_    351  5.999705e-05
# cyclinng                         352  2.334356e-04
# tem                              353 -1.415534e-02

write.csv(res, "CrossTemp_result_of_CV_pred1_lasso.csv")


######----------------LASSO 2-------------------------------------
set.seed(777)
df %>% dim()
df[348:353] %>% head
x <- model.matrix(fbc~. , -353, data=df )
y <- as.matrix(df[, 353]) # Only fbc
y
cv.lasso <- cv.glmnet(x, y, nfold=10, alpha=1, parallel=TRUE, standardize=TRUE, standardize.response = TRUE, type.measure='mae')
cv.lasso
# Results
plot(cv.lasso, main = "CrossTemp LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568 ")
#op <- par(mfrow=c(1, 2))
plot(cv.lasso$glmnet.fit, xvar = "lambda", label = TRUE, main ="CrossTemp LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568")
plot(cv.lasso$glmnet.fit, xvar = "dev", label = TRUE, main="CrossTemp LASSO coefficients capture with type.measure='mae', Lambda min 0.2224568 ")
cv.lasso$lambda.min
#[1] 0.0005983867
cv.lasso$lambda.1se
#[1]  0.008885853

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
l_name_coeff
#                         name   coefficient
# 1                (Intercept)  8.533999e+00
# 2 sfbc_b32wlsaup_drpost_pcs_  6.555852e-05
# 3          crd_scrnslcbot_p_  1.043924e-01
# 4                   cyclinng  6.051258e-05
# 5                        tem -9.300889e-03
l_name_coeff_sort <- l_name_coeff[order( l_name_coeff$coefficient),]  # sorting coeff
write.csv(l_name_coeff_sort, "CrossTemp_lasso_name_coeff_mae.csv")
as.data.frame(l_name_coeff_sort)

#####---------------------
# try to make subset of variables picked by Lasso pred1 
# vector of columns you DON'T want
f <- as.vector(l_name_coeff[,1])
f <- f[-1]
f %>% str
# subset of all data with selected  21 veriables
df %>% dim
#   need to extract level in matrix
df_m <- df
df_l <- model.matrix(~df$level-1, data=df$level)
df_l 
df_m <- cbind(df_m, df_l)
df_m %>% names

#df_m$levelMP <- rename(df_m$`df$levelMP`)
colnames(df_m)[354] <- "levelLP" 
colnames(df_m)[355] <- "levelMP"
colnames(df_m)[356] <- "levelUP"
df_m <- df_m[, -"level"]
df_m %>% names
ss <-df_m %>% select(f)
ss %>% dim
ss %>% names
ss
# add responce 
df$fbc
ss$fbc <-df$fbc
write.csv(ss, "E_CrossTemp_lasso_subseton_FBC.csv")
lmod_ss <- lm(fbc~., ss)
lmod_ss
summary(lmod_ss)
lmod_summary <-lmod_ss %>% summary
capture.output(lmod_summary, file = "E_CrossTemp_lasso_GLMod_FBC.txt")
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
write.csv(result, file = "E_Contribution of Variables in the Cross Temp Model_Lasso.csv")


# Linear model with standardizing predictors
# standartization  # normalize

ss %>% names
ss1 <- lapply(ss, scale)

# ss$level <-as.numeric(ss$level)
# ss$level <- ss$level
ss1 %>% names
st_lmod_3temp <- lm(fbc~., ss1)
st_lmod_3temp %>% summary
# anova with stndardized Lasso var
af.rf <- anova(st_lmod_3temp)
afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
#  trivially, scale them by 100 divided by their sum.
print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
res_sort <- result[order(- result$PctExp),]  # sorting coeff
res_sort1 <- res_sort[-2,]
res_sort1 <- res_sort1[1:30,]
write.csv(res_sort1, file = "E_crossTemp_Lasso_stan_anova.csv")






##----------       GBM    -----------------------------X--------------------------------------
df %>% dim

#nH <- intersect(h1_coef_slop, df6_HTPD_clean)
library(data.table)
require(gbm)
library(gbm)
df <- df6_3temp
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

s<-summary(df.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance
s 
s<- data.table(s)
gbm.imp <-  s[1:30,]
gbm.imp
write.csv(gbm.imp, "E_CrossTemp_GBMachine_subset.csv")


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
f.predict <- predict(df.boost,test[,-353],best.iter)
f.predict

f1.predict <- predict(df.boost, train[,-353], n.trees = 300, type = "response")
f1.predict
gbm.preddf <- data.frame(train[,353], f1.predict)

head(data.frame("PredictedProbability" = f1.predict,"Actual" = train$fbc))

plot( f1.predict, train$fbc, 
      
      xlab = "prediction", ylab = "fbc",col = "dark red", 
      main = "Rate of actual Cross Temp FBC rate againt of predicted GBM") 
plot(f1.predict, train$fbc,   xlab = "prediction", ylab = "fbc",col = " darkorange", main = "Rate of actual Cross Temp FBC growth against to predicted, 
     based on Gradient Boosting Machine")            
# plot prediction as a slop for the 151 dies
abline(lm( train$fbc ~ f1.predict, data=df))
# plot  perfect prediction line red
abline(lm(train$fbc ~ f1.predict + 0),  col="blue")

#-----------------------------------------------------------------------------end GBM--------------------------

#  ----------- Feature Selection with Lasso and XGBoost and ranger--------------
library(FeatureSelection)
devtools::install_version("xgboost", version = "0.4-4", repos = "http://cran.us.r-project.org")
install.packages("ranger")
library(dplyr)
library(xgboost)
library(ranger)
library(caret)
library(dplyr)
library(corrplot)
library(plyr)
library(data.table)
start.time <- Sys.time()


df <- df6_3temp
df <- na.omit(df)
X_n = df[, -353]
X_n
y_n = df[, 353]

## 75% of the sample size  or may be 80%  ???
set.seed(123)
smp_size <- floor(0.90 * nrow(df))

train_ind <- sample(seq_len(nrow(df)), size = smp_size, replace = FALSE  )

train <- df[train_ind, ]
test <- df[-train_ind, ]

#TRAIN
train %>% dim
#[1] 5433  352
tx <- train[,-c(353)]
#[1] 16299   353
names(train)
tx <- model.matrix(fbc~. , -353, data=train )
ty <- as.matrix(train[, 353]) # Only slop
ty %>% dim
ty
# TEST
test %>% dim
#[1]  16 353
names(test)
y_st <- test[,353]
y_st %>% summary  # we need to to campare result
test <- model.matrix(fbc~. , -353, data=test )
test <- test[, -c(353)]
test
df_mat <- model.matrix(fbc~., -353, data=df)
y_mat <- as.matrix(df[,353])
#X_train obviously all data
p = df[, 'fbc']

########    NO RUN, it will take 3 hours to work

params_glmnet = list(alpha = 1, family = 'gaussian', nfolds = 5, parallel = TRUE, standardize=TRUE, standardize.response = TRUE)


params_xgboost = list( params = list("objective" = "reg:linear", "bst:eta" = 0.001, "subsample" = 0.75, "max_depth" = 5,
                                     
                                     "colsample_bytree" = 0.75, "nthread" = 6),
                       
                       nrounds = 1000, print.every.n = 250, maximize = FALSE)


params_ranger = list(dependent.variable.name = 'y', probability = FALSE, num.trees = 1000, verbose = TRUE, mtry = 5, 
                     
                     min.node.size = 10, num.threads = 6, classification = FALSE, importance = 'permutation')


params_features = list(keep_number_feat = NULL, union = TRUE)


feat1 <- wrapper_feat_select(X = tx, y = ty, params_glmnet = params_glmnet, params_xgboost = params_xgboost, 
                             
                             params_ranger = params_ranger, xgb_sort = 'Gain', CV_folds = 10, stratified_regr = FALSE, 
                             
                             scale_coefs_glmnet = FALSE, cores_glmnet = 5, params_features = params_features, verbose = TRUE)
##  run all
capture.output(feat1, file = "E_feat1_XGB_Lasso_Ranger_model.csv")
str(feat1)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# about 3 hours
#The feature importance of the object feat can be plotted using the barplot_feat_select function, which takes as an additional argument the params_barplot,
params_barplot = list(keep_features = 30, horiz = TRUE, cex.names = 1.0)

barplot_feat_select(feat1, params_barplot, xgb_sort = 'Cover')
af <- data.table(feat1$all_feat)
feat1$all_feat
feat1$union_feat
capture.output(af, "E_All_feat_Laaso_XGB_Ranger.csv")
union<-data.table(feat1$union_feat %>% head(50))
write.table(union, "E_Union_feat_Lasso_XGB_Ranger.csv", row.names = TRUE)
union
#                             feature importance Frequency
# 1:                          levelMP  1.0000000         3
# 2:                          levelUP  0.9761638         3
# 3:   tlcwc_we0_wl31_fr_s25_f2g_pcs_  0.9342035         3
# 4:                        vf_sk_mv_  0.9257058         3
# 5:    tlcwc_we0_wl0_fr_s25_f2g_pcs_  0.9239548         3
# 6:            vth_wlds0last_l3s_mv_  0.9144037         3
# 7:              layer3_vpgms_s_dac_  0.9045866         3
# 8:                          cycling  0.8802158         3
# 9:                crd_scrnslcbot_p_  0.8747423         3
# 10:   tlcwc_we0_wl63_fr_s25_a2r_pcs_  0.8672016         3
# 11:                  vth_sgs_med_mv_  0.8667431         3
# 12:       sfbc_b32wlsaup_drpost_pcs_  0.8640449         3
# 13:                sde_pmp_vm_s_dac_  0.8594728         3
# 14:         sfbc_b32wlsalp_post_pcs_  0.8438166         3
# 15:               tproga2slcrand_us_  0.8433655         3
# 16:             crd_m1bll_224_c_pcs_  0.8411432         3
# 17: sfbc_t32wlsalp_far_dc_fresh_pcs_  0.8373404         3
# 18:      sfbc_b32wlsalp_dc_post_pcs_  0.8335410         3
# 19:                  vth_sgs_win_mv_  0.8311113         3
# 20:               wlleak_post_10_na_  0.8294615         3
# 21:      bbk_wlds0vth_3_u1_ev_c_pcs_  0.8278117         3
# 22:                      eires20x1__  0.8268088         3
# 23:               vth_08pwl60_lt_mv_  0.8203633         3
# 24:                         tpor_us_  0.8198893         3
# 25:         bbk_high_wldd0_ev_d_pcs_  0.8125649         3
# 26:                 veralooptrial1__  0.8091043         3
# 27:                vth_wlds0_l3s_mv_  0.8065374         3
# 28:                          tpd_ns_  0.8029184         3
# 29:              tproga2slc_wl24_us_  0.7973227         3
# 30:               wlleak_post_32_na_  0.7947020         3

#The *func_correlation * function can be used here to return the predictors that are highly correlated with the response


#---------------------------------------------- END---------------
# correlation of variables
# After the important features of each algorithm are returned, a next step could be to observe if the top features are correlated with the response and how each algorithm treated correlated predictors during feature selection.
# 
# The *func_correlation * function can be used here to return the predictors that are highly correlated with the response
# dat = data.frame(p = ty, tx)
# dat
# cor_feat = func_correlation(dat, target = 'p', correlation_thresh = 0.1, use_obs = 'complete.obs', correlation_method = 'pearson')
# cor_feat
# head(cor_feat)
#
df <- as.data.frame(df)
df_mat <- model.matrix(fbc~. , -353, data=df )
df_mat %>% head   # just check out the level ia 3 dif col
y <- as.matrix(df[, 353])
dat = data.frame(p = y, df_mat)
dat
cor_feat = func_correlation(dat, target = 'p', correlation_thresh = 0.1, use_obs = 'complete.obs', correlation_method = 'pearson')

head(cor_feat)
out_lst = lapply(feat1$all_feat, function(x) which(rownames(cor_feat) %in% x[1:100, 1]))
#                                         p
# levelMP                          0.4353513
# sfbc_b32wlsaup_drpost_pcs_       0.2701464
# cycling                          0.2658624
# sfbc_b32wlsalp_dc_post_pcs_      0.2099147
# sfbc_t32wlsalp_far_dc_fresh_pcs_ 0.2067408
# vth_wlds0last_l3s_mv_            0.1898714

str(out_lst)
#List of 3
# $ glmnet-lasso: int [1:6] 1 6 10 12 18 20
# $ xgboost     : int [1:21] 1 2 3 4 5 6 7 8 9 10 ...
# $ ranger      : int [1:21] 1 2 3 4 5 6 7 8 9 10 ...
#Lets find the number of the highly correlated var on Lasso
cor_lasso = func_correlation(tx[, feat1$all_feat$`glmnet-lasso`[, 1]], target = NULL, correlation_thresh = 0.9, 
                             
                             use_obs = 'complete.obs', correlation_method = 'pearson')

head(cor_lasso$out_df)
# NULL    there is no correlation between features :)
dim(cor_lasso$out_df)[1]

cor_xgb = func_correlation(tx[, feat1$all_feat$xgboost[, 1][1:100]], target = NULL, correlation_thresh = 0.9, 
                           
                           use_obs = 'complete.obs', correlation_method = 'pearson')

head(cor_xgb$out_df)
dim(cor_xgb$out_df)[1]  
# no correlation :)
cor_rf = func_correlation(tx[, feat1$all_feat$ranger[, 1][1:100]], target = NULL, correlation_thresh = 0.9,
                          
                          use_obs = 'complete.obs', correlation_method = 'pearson')


head(cor_rf$out_df)
# no correlation :)


#code

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Percentage of vriance explaned

af <- anova(fit)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))



####--------------------------    GLM based on the Union of Ensemble Learning  pded1-----------------------------
# try to make subset of variables picked by Lasso pred1 
# vector of columns you DON'T want
#aaa <- read.csv("E_Union_feat_Laaso_XGB_Ranger.csv")
aaa <- union
aaa$feature
v <- as.vector(aaa$feature)
v
v %>% str
# subset of all data with selected  21 veriables
df %>% dim
#   need to extract level in matrix
df_m <- df
df_l <- model.matrix(~df$level-1, data=df$level)
df_l 
df_m <- cbind(df_m, df_l)
df_m %>% names

#df_m$levelMP <- rename(df_m$`df$levelMP`)
colnames(df_m)[354] <- "levelLP" 
colnames(df_m)[355] <- "levelMP"
colnames(df_m)[356] <- "levelUP"
#df_m <- df_m[,-351]
df_m$level<- NULL
df_m %>% names

v %>% str
un <- df_m %>% select(v)
un %>% dim
un %>% names

#un <- na.omit(un)
# add responce 
df$fbc
un$fbc <-df$fbc
un %>%dim

write.csv(un, "E_CrossTemp_XGB_Ranger_LASSO_DSsubset.csv")

# Linear model without standardizing predictors
lmod_un <- lm(fbc~., un)
lmod_un
summary(lmod_un)
lmod_s <-lmod_un %>% summary
capture.output(lmod_summary, file = "E_Union_linmodel_crosstemp_subset.txt")
af <- anova(lmod_un)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))
AnovaUn <-cbind(af,PctExp=afss/sum(afss)*100)
AnovaUn <- AnovaUn[-c(51),]
AnovaUn
write.csv(AnovaUn, "E_UnionModel_contribution.csv")
res_sort <- AnovaUn[order(-AnovaUn$PctExp),]  # sorting coeff PctExp
res_sort
res_sortA <- res_sort[1:30,]
res_sortA
write.csv(res_sortA, file ="E_UnionModel_contribution_sorted.csv")


#plot(lmod_un,  main = " Statistics of union of models")

# Linear model with standardizing predictors
un %>% dim

# standartization
un_st <- lapply(un, scale)
st_lmod_un <- lm(fbc~., un_st)
st_lmod_un %>% summary
plot(st_lmod_un)


# all.vars(modelformula)
# st_lmod_un <- lapply(un[, all.vars(modelformula)], scale)
# 
# un <- as.numeric(un)
# 
#st_un <- un %>% mutate_all(funs(scale), all.vars(modelformula ))

# Percentage of vriance explaned
af <- anova(st_lmod_un)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))
AnovaUn <-cbind(af,PctExp=afss/sum(afss)*100)
AnovaUn <- AnovaUn[-c(51),]
AnovaUn
write.csv(AnovaUn, "E_UnionModel_contribution_standardized.csv")
res_sort <- AnovaUn[order(- AnovaUn$PctExp),]  # sorting coeff PctExp

res_sortB <- res_sort[1:50,]
res_sortB
write.csv(res_sortB, file ="E_UnionModel_contribution_sorted_Standardized.csv")

# Df                                       Sum Sq      Mean Sq      F value        Pr(>F)       PctExp
# levelMP                              1 1.167864e+03 1.167864e+03 3.301053e+03  0.000000e+00 6.448722e+00
# vf_sk_mv_                            1 1.605977e+02 1.605977e+02 4.539413e+02  1.670637e-99 8.867902e-01
# vth_sgs_med_mv_                      1 5.935570e+02 5.935570e+02 1.677733e+03  0.000000e+00 3.277509e+00
# sde_pmp_vm_s_dac_                    1 3.413769e+00 3.413769e+00 9.649269e+00  1.897201e-03 1.885019e-02
# vth_wlds0last_l3s_mv_                1 7.220460e+01 7.220460e+01 2.040916e+02  4.760927e-46 3.987002e-01
# tlcwc_we0_wl0_fr_s25_f2g_pcs_        1 2.138191e+01 2.138191e+01 6.043755e+01  8.000807e-15 1.180669e-01
# sfbc_b32wlsaup_drpost_pcs_           1 3.532823e+02 3.532823e+02 9.985785e+02 2.245406e-213 1.950758e+00
# icca_add00_ua_                       1 3.571982e+00 3.571982e+00 1.009647e+01  1.488033e-03 1.972381e-02
# tlcwc_we0_wl31_fr_s25_f2g_pcs_       1 3.905068e+01 3.905068e+01 1.103796e+02  9.598342e-26 2.156305e-01
# crd_m1bll_224_c_pcs_                 1 1.704053e+00 1.704053e+00 4.816632e+00  2.819903e-02 9.409460e-03
# cycling                              1 5.994045e+02 5.994045e+02 1.694261e+03  0.000000e+00 3.309798e+00
# sfbc_t32wlsalp_far_dc_fresh_pcs_     1 7.208608e+01 7.208608e+01 2.037566e+02  5.623051e-46 3.980457e-01
# tproga2slcrand_us_                   1 3.484666e-01 3.484666e-01 9.849667e-01  3.209890e-01 1.924167e-03
# fbc_sdllk4_pcs_                      1 1.094814e+02 1.094814e+02 3.094574e+02  1.070807e-68 6.045358e-01
# sfbc_t32wlsaerx_far_fresh_pcs_       1 1.459919e+01 1.459919e+01 4.126570e+01  1.361949e-10 8.061399e-02
# sfbc_t32wlsalp_drpost_pcs_           1 1.132197e+00 1.132197e+00 3.200238e+00  7.364427e-02 6.251778e-03
# sfbc_t32wlsalp_far_fresh_pcs_        1 2.386382e+01 2.386382e+01 6.745285e+01  2.301883e-16 1.317715e-01
# tpor_us_                             1 3.902079e+01 3.902079e+01 1.102951e+02  1.001373e-25 2.154654e-01
# vth_sgd_med_mv_                      1 5.520178e+00 5.520178e+00 1.560319e+01  7.841898e-05 3.048138e-02
# bbk_high_wldd0_ev_d_pcs_             1 2.895501e+00 2.895501e+00 8.184348e+00  4.230179e-03 1.598841e-02
# vth_wlds1_med_mv_                    1 1.052280e+01 1.052280e+01 2.974348e+01  4.996309e-08 5.810493e-02
# tem                                  1 8.397129e+03 8.397129e+03 2.373510e+04  0.000000e+00 4.636736e+01
# tlcwc_we0_wl63_fr_s25_a2r_pcs_       1 5.217557e-01 5.217557e-01 1.474781e+00  2.246087e-01 2.881036e-03
# sfbc_t32wlsaerx_dc_lijl2_pcs_        1 1.451503e+00 1.451503e+00 4.102779e+00  4.282744e-02 8.014923e-03
# sfbc_slcerslp_20kpost_pcs_           1 1.746275e-02 1.746275e-02 4.935976e-02  8.241836e-01 9.642602e-05
# vcg_gv3_t_mv_                        1 7.024031e+00 7.024031e+00 1.985394e+01  8.409199e-06 3.878537e-02
# vth_wlds0_win_mv_                    1 4.745610e-01 4.745610e-01 1.341382e+00  2.468057e-01 2.620436e-03
# wlleak_each_value_34_na_             1 7.752983e-01 7.752983e-01 2.191438e+00  1.387971e-01 4.281051e-03
# sfbc_slcerslp_7kpost_pcs_            1 1.058168e+01 1.058168e+01 2.990991e+01  4.585944e-08 5.843006e-02
# vfour_t_mv_                          1 8.630045e-02 8.630045e-02 2.439346e-01  6.213847e-01 4.765348e-04
# Residuals                        18080 6.396436e+03 3.537852e-01           NA            NA 3.531991e+01



## Work just with Lasso from ensemble learning

L <- as.matrix(feat$all_feat$`glmnet-lasso`)
L
num_L<-feat$all_feat$`glmnet-lasso`[1:30, 1]
num_L
subset_Lasso_Ensemble<-df_m %>% select(num_L)
subset_Lasso_Ensemble$fbc <- df$fbc

# Linear model without standardizing predictors
lmod_Lasso <- lm(fbc~., subset_Lasso_Ensemble)
lmod_Lasso
summary(lmod_Lasso)
SummaryLasso <-lmod_Lasso %>% summary
capture.output(SummaryLasso, file = "Lasso_model_crosstemp_lassosubset30.txt")
# 
# Call:
#   lm(formula = fbc ~ ., data = subset_Lasso_Ensemble)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.7265 -0.7739  0.4035  0.9912  1.8528 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    29.4787183  5.9089544   4.989 6.13e-07 ***
#   levelMP                         0.8461039  0.0232766  36.350  < 2e-16 ***
#   vera_mlc_rate__                 0.8830700  0.3444438   2.564 0.010363 *  
#   levelUP                         0.2428020  0.0232717  10.433  < 2e-16 ***
#   sde_pmp_vm_s_dac_               0.1930117  0.0169706  11.373  < 2e-16 ***
#   crd_scrnslcbot_p_               0.0640621  0.0125094   5.121 3.07e-07 ***
#   vpgmu_bwl_s_dac_               -0.0016306  0.0098318  -0.166 0.868277    
# vcgrvstep_s_dac_                0.0128610  0.0141673   0.908 0.363998    
# frlt_vcgrsft_er3_pcs_           0.0136838  0.0097107   1.409 0.158810    
# treadm_us_                     -0.0158801  0.0170570  -0.931 0.351865    
# tmps_trim_bgr_s_dac_            0.0344452  0.0169879   2.028 0.042612 *  
#   bbk_sgsld_1_d_pcs_              0.0426653  0.0047879   8.911  < 2e-16 ***
#   crd_blleak1_p_                  0.0232348  0.0098823   2.351 0.018726 *  
#   layer3_vpgms_s_dac_             0.0342110  0.0087944   3.890 0.000101 ***
#   crd_ddbll_slc1_p_              -0.0081577  0.0083601  -0.976 0.329179    
# vcg_av3_t_mv_                   0.0124594  0.0020975   5.940 2.90e-09 ***
#   tlcwc_we0_wl63_fr_s25_a2r_pcs_ -0.0116800  0.0020396  -5.727 1.04e-08 ***
#   vcg_dv3_t_mv_                  -0.0045007  0.0022418  -2.008 0.044699 *  
#   vcelsrct_p_                     0.0026157  0.0014453   1.810 0.070331 .  
# vcg_er3_t_mv_                  -0.0056887  0.0023207  -2.451 0.014242 *  
#   wlleak_each_value_37_na_        0.0013922  0.0009219   1.510 0.131007    
# halfvccq18_t_mv_               -0.0001861  0.0012533  -0.148 0.881966    
# iref_s_dac_                     0.0099852  0.0050285   1.986 0.047080 *  
#   tproga2slcrand_us_              0.0031694  0.0008677   3.653 0.000260 ***
#   wlleak_post_50_na_              0.0023991  0.0008598   2.790 0.005272 ** 
#   wlleak_each_value_34_na_       -0.0040873  0.0009645  -4.238 2.27e-05 ***
#   wlleak_post_51_na_              0.0010789  0.0009336   1.156 0.247843    
# bbk_mlcread2_d_pcs_            -0.0261036  0.0041691  -6.261 3.91e-10 ***
#   wlleak_post_30_na_             -0.0029070  0.0009169  -3.170 0.001525 ** 
#   wlleak_each_value_11_na_       -0.0007307  0.0008617  -0.848 0.396484    
# wlleak_post_29_na_             -0.0001619  0.0009099  -0.178 0.858749    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.279 on 18080 degrees of freedom
# Multiple R-squared:  0.09713,	Adjusted R-squared:  0.09563 
# F-statistic: 64.83 on 30 and 18080 DF,  p-value: < 2.2e-16

plot(lmod_Lasso,  main = " Statistics of Lasso from Ensemble of models")

# Linear model with standardizing predictors
subset_Lasso_Ensemble %>% dim
#[1] 18111    31
# standartization did not change absolutly anything coz it was standurduzed during precidure od Lasso model
st_subset_Lasso_Ensemble <- lapply(subset_Lasso_Ensemble, scale)
st_lmod_subset_Lasso_Ensemble <- lm(fbc~.,  st_subset_Lasso_Ensemble)
st_lmod_subset_Lasso_Ensemble %>% summary
#.....
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.279 on 18080 degrees of freedom
# Multiple R-squared:  0.09713,	Adjusted R-squared:  0.09563 
# F-statistic: 64.83 on 30 and 18080 DF,  p-value: < 2.2e-16

plot(st_lmod_subset_Lasso_Ensemble)

## final Solution with plotting
pred<-predict(object=,newdata=test)
actual<-test$price
result<-data.frame(actual=actual,predicted=pred)
paste('Function Call: ', model1$call)



