library(glmnet)
library(dplyr)
library(plyr)
library(corrplot)
library(caret)
library(devtools)
library(data.table)

Lmp <- read.csv("F_LTPD_MP_DQ_new.csv")
# on the cluster
##     b3_r <- read.csv("/home/irinam/BICS3/rtpd_maxfbc_0918.csv")
summary(Lmp)
dim(Lmp)
#[1] 7296    7
# rename colomns for easy to use
Lmp <- rename(Lmp, c("X_c6"="FBC", "a.lot"="lot", "a.wafer"="wafer","a.x"= "x", "a.y"= "y", "a.page_type" = "level", "a.cycling" = "cycling"))
Lmp %>% names   # [1] "a.lot"     "a.wafer"   "a.x"       "a.y"       "a.blk"     "a.cycling" "X_c6" 
which.max( Lmp$FBC)
outl <- data.frame(which(  Lmp$FBC >100, arr.ind = T))
outl

Lmp[c(1459,2230),]
#            lot wafer  x  y level cycling  FBC
#1459 CP0939223    18  7 15    MP    3000  128
#2230 CP0938453     6 28 13    MP    3000 1017
# remove outlier by removing the row 318 with fbc 993 and all > 9
Lmp <- Lmp[-c(2230), ]
row.names(Lmp) <- 1:nrow(Lmp)
row.names(Lmp)
dim(Lmp)
#[1] 2431    6
write.csv(Lmp, "LMP_LTPD_MP_dq.csv", row.names = FALSE)
head(Lmp)

# sub.MP <- arrange( b3_l, level)
# sub.MP <- filter(b3_l, b3_l$level == "MP")
# sub.MP %>% dim
# write.table(sub.MP, "ltpd_MP.txt", row.names = FALSE)
# write.csv(sub.MP, "ltpd_MP.csv", row.names = FALSE)

##--------------------------------END of Work with DQ LTPD MP-------------------------------

# ---------------    D/S paramerates coordination
#Based on the on the "ltpd_MP.csv" was create table in Hive and retreaved data by query

# SELECT *
#   FROM diesort.ds_bics_t6rn1_sme1_plot c 
# join irina_db.ltpd_mp u 
# on u.lot=(regexp_extract(c.lotno,'(.*)\\.',1))
# and u.x=c.originaldiex 
# and u.y=c.originaldiey
# and cast(u.wafer as int)=cast(regexp_extract(c.waferno, '.*\\.(\\d+)',1) as int) ;


LMPdf <- read.csv("LTPD_PM_avgfbc_DS_FULL.csv", header = TRUE, sep = ",", na.strings = c("NULL", "NaN", "NA"), stringsAsFactors = TRUE)
LMPdf %>% dim
which.max(LMPdf$u.fbc)

# make copy of original table and work with copy in case if something going wrong...
df <- LMPdf

# make copy of original table and work with copy in case if something going wrong...

df %>% dim 
#  7293 1780
# 
df[,1773:1781] %>% head
#names(df) <- substring(names(df[,1:348]), 3) remove  first 2 elements from names

nm<- names(df)
nm
str(df)
tail(df)# I am going to save cycling fbc and tem columns for future
df.c.f.t <- df[,c(1775:1781)]
df.c.f.t %>% head

# identefy and remove fisrt 36colomn and last 5 and remove them
n1 <- names(df[,1:38])
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
df <- df[, -c(1:38, 1774:1781) ]
# sub("c.*", "", df[,1])
# names(df) = gsub(pattern = "b.*", replacement = "", x = names(df))

df %>% names 
# remove first 2 simbols in col names to make it easy to read
names(df) <- substring(names(df) , 3)

df1 <- df  # save original  df as df1
df %>% dim 

df %>%  names
df <- data.table(df)
drop.cols <- grep("avro_file_name$", colnames(df))
drop.cols
df <-df[, (drop.cols) := NULL]

drop.cols <- grep("original_file_name", colnames(df))
drop.cols
df <-df[, (drop.cols) := NULL]
# sub("c.*", "", df[,1])
# names(df) = gsub(pattern = "b.*", replacement = "", x = names(df))
df <-data.frame(df)
df %>%  names

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
write.csv(df.cor, "LMP_CorMatrix_Ltpd.csv", row.names=FALSE, na="")

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
df$level <- df.c.f.t$u.level
df$cycling <- df.c.f.t$u.cycling
df$fbc <- df.c.f.t$u.fbc
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
df6_LMP <- df

##  save as csv file for next step
write.csv(df, outputPath, row.names=FALSE, na="")
write.csv(df6_LMP, "LMP_LTPD_formodel.csv", row.names=FALSE, na="")


#-----------------LASSO--------------------------------------------------------
df <- df6_LMP
df[,340:345] %>% head
# model with CV and lambda min
set.seed(555)
df$fbc %>% summary
# out1 <- data.frame(which( df$fbc > 200, arr.ind = T))
# out1
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

df %>% dim
df %>% names

x <- model.matrix(fbc~. , -345, data=df )
x %>% names
x <- data.table(x)
x %>% names  ## we need to delete  "original_file_name$"-----------------------------------TRASH???_____________________
#x <- x[, -c(189:214)]

x %>% names
x <- as.matrix(x)
x %>% dim
x[, 340:345] %>% head
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
plot(pred2, y,  xlab = "prediction", ylab = "fbc",col = "blue", main = "Rate of actual LTPD FBC vs Predicted on model Lasso, 
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

#                                   row       sl_coef
# (Intercept)                        1 -4.684177e+01
# crd_ttl_pl0_p_                     6 -7.003869e-03
# iccs_moni_1_ua_                    8  1.869472e-02
# fifthratio__                      11  1.789691e+00
# iref_t_ua_                        12  1.130510e+00
# vref_t_mv_                        14 -1.282816e-02
# vref_s_dac_                       15 -1.153877e-01
# vdd_s_dac_                        16  3.007437e-02
# vdda_t_mv_                        17 -4.563566e-03
# vddsa_s_dac_                      20 -1.207324e-01
# sde_s_dac_                        21  2.890616e-02
# sde_pmp_um_s_dac_                 23 -5.937947e-03
# vx2_s_dac_                        25 -2.319779e-01
# vx4_t_mv_                         26 -3.072072e-03
# vfour_bist_s_dac_                 28  1.747268e-01
# vfour_s_dac_                      29  1.711549e-01
# tmps_trim_bgr_s_dac_              31 -2.243620e-02
# tmps_trim_ptat_s_dac_             32  2.366394e-02
# vcgrvstep_s_dac_                  33  1.669295e-01
# celsrcstep_t_mv_                  34  9.251863e-03
# celsrcstep_s_dac_                 35 -1.003093e-01
# vcg_br3_t_mv_                     40 -1.532184e-02
# vcg_fr3_t_mv_                     45 -1.743172e-03
# vcg_gr3_t_mv_                     46 -2.535889e-03
# vcg_slcv_s_dac_                   48 -2.121784e-01
# vcg_av3_t_mv_                     49  2.333560e-02
# vcg_av3_s_dac_                    50  4.303069e-02
# vcg_bv3_t_mv_                     51 -1.070666e-03
# vcg_cv3_t_mv_                     52 -1.329351e-02
# vcg_fv3_t_mv_                     55  3.807243e-02
# vsgd_t_mv_                        57 -1.372887e-03
# vcgrv2_t_mv_                      59 -1.524149e-02
# vcgrv2_s_dac_                     60 -1.296364e-01
# vreadk_n1_t_mv_                   62  1.699932e-03
# vpass3_t_mv_                      67  3.640285e-03
# vread_evfy_mlc_t_mv_              69 -3.265457e-03
# vread_evfy_mlc_s_dac_             70 -1.490978e-01
# vread_evfy_slc_t_mv_              71  3.197035e-03
# psdovccq_t_mv_                    73  6.975055e-03
# halfvccq18_t_mv_                  75 -7.963434e-03
# halfvccq33_t_mv_                  76 -1.739297e-02
# halfvccq_s_dac_                   77  1.151338e-01
# vpgms_sgdprog_t_mv_               79  1.626973e-03
# vpgms_sgdprog_s_dac_              80  2.261639e-03
# ron_po18_s_dac_                   82 -7.738159e-02
# ron_no18_t_ua_                    83  6.052244e+01
# ron_odt_18p_s_dac_                87  4.172861e-01
# ron_odt_18_pn_rate_mv_            89 -1.362251e+01
# ron_odt_33n_t_ua_                 90  2.824161e-04
# ron_odt_33_pn_rate_mv_            91  4.610720e+00
# vfour_flt_t_mv_                   92 -3.568867e-03
# vth_wldd01_l3s_mv_                97  5.757115e-03
# vth_sgs_win_mv_                   98 -1.962510e-03
# vth_sgs_l3s_mv_                   99  2.010816e-03
# wlleak_each_value_ds1_na_        104 -2.642203e-02
# wlleak_each_value_01_na_         106 -5.439568e-05
# wlleak_each_value_02_na_         107  4.720911e-03
# wlleak_each_value_03_na_         108 -6.744778e-03
# wlleak_each_value_08_na_         112  2.274296e-03
# wlleak_each_value_11_na_         115  3.249881e-03
# wlleak_each_value_12_na_         116 -1.464322e-02
# wlleak_each_value_13_na_         117 -2.153729e-03
# wlleak_each_value_14_na_         118 -7.877939e-03
# wlleak_each_value_20_na_         124 -1.334937e-02
# wlleak_each_value_21_na_         125 -5.241958e-03
# wlleak_each_value_24_na_         128 -1.285409e-02
# wlleak_each_value_27_na_         130 -1.498863e-02
# wlleak_each_value_34_na_         134  2.072845e-02
# wlleak_each_value_35_na_         135  3.527685e-03
# wlleak_each_value_37_na_         136  8.856246e-03
# wlleak_each_value_40_na_         139  1.920889e-02
# wlleak_each_value_48_na_         144  5.615596e-03
# wlleak_each_value_50_na_         145  2.357154e-03
# wlleak_each_value_52_na_         147  2.819801e-03
# wlleak_each_value_56_na_         150 -6.170460e-03
# wlleak_each_value_60_na_         152  1.442281e-03
# wlleak_each_value_62_na_         154 -1.777708e-03
# wlleak_each_value_63_na_         155 -9.364365e-03
# fbc_senlk0_pcs_                  159 -3.018160e-02
# fbc_dbuslk3_pcs_                 160 -9.951181e-03
# fbc_sdllk0_pcs_                  162  2.246899e-01
# fbc_sdllk3_pcs_                  163 -3.181598e-04
# tempcode_read_pcs_               165  2.390169e-01
# crd_m1bll_224_c_pcs_             167 -1.857402e-06
# crd_sdllk_p_                     169  6.195317e-03
# tmps_trim_hts_p_                 170  2.850397e-02
# vcelsrcs_p_                      172 -1.362159e-01
# vreadt_p_                        173  2.649503e-03
# vreadlt_p_                       175 -4.358318e-03
# veramaxt_p_                      176  2.445744e-06
# vpgmmaxt_p_                      177 -2.240285e-03
# vth_wlds0prog_l3s_mv_            179 -4.345382e-03
# sgdprog_erase_pass__             181 -3.154352e-01
# vth_sgdprog_win_mv_              183 -1.324057e-03
# bit_mhopen_io3_pcs_              186  1.139826e-01
# bit_mhopen_io5_pcs_              188 -1.935813e-01
# crd_blleak1_p_                   198  7.546736e-02
# layer4_vpgms_s_dac_              201  6.123133e-02
# vpgmu_twl_s_dac_                 205  1.120925e-02
# vpgmu_bwl_s_dac_                 206  2.038487e-01
# terase_us_                       208 -9.103314e-05
# bbk_wlmh_4_d_pcs_                213  8.629462e-03
# bbk_wlld_7_d_pcs_                215 -2.008663e-01
# bbk_slcreadrand_d_pcs_           217 -1.348362e-01
# bbk_mlcread2_d_pcs_              218 -2.019225e-03
# vth_wlds0last_l3s_mv_            219  7.204241e-03
# vera_mlc_rate__                  222 -3.076706e+00
# wlleak_post_00_na_               226 -2.219956e-03
# wlleak_post_01_na_               227  8.943238e-03
# wlleak_post_07_na_               233 -4.213279e-03
# wlleak_post_13_na_               238  1.975423e-03
# wlleak_post_14_na_               239 -9.700124e-04
# wlleak_post_15_na_               240 -4.464352e-04
# wlleak_post_16_na_               241 -6.075683e-03
# wlleak_post_18_na_               243  2.528862e-03
# wlleak_post_23_na_               248 -1.888300e-02
# wlleak_post_25_na_               249  6.975672e-03
# wlleak_post_27_na_               251 -2.441821e-02
# wlleak_post_28_na_               252 -1.147824e-04
# wlleak_post_29_na_               253 -7.761100e-03
# wlleak_post_30_na_               254  7.808499e-03
# wlleak_post_31_na_               255 -8.820128e-03
# wlleak_post_32_na_               256  1.719668e-03
# wlleak_post_34_na_               258 -2.019286e-02
# wlleak_post_38_na_               262  9.377226e-03
# wlleak_post_43_na_               264  2.187426e-03
# wlleak_post_44_na_               265 -1.121106e-02
# wlleak_post_45_na_               266 -1.297282e-03
# wlleak_post_46_na_               267  1.747227e-02
# wlleak_post_50_na_               270  1.987523e-02
# wlleak_post_51_na_               271  3.252529e-02
# wlleak_post_55_na_               275  5.096020e-03
# wlleak_post_56_na_               276  1.880691e-04
# wlleak_post_57_na_               277 -2.067347e-02
# wlleak_post_59_na_               279 -5.774366e-03
# wlleak_post_60_na_               280  1.486322e-02
# wlleak_post_63_na_               283  1.093006e-02
# wlleak_post_dd1_na_              284  4.598933e-03
# wlleak_post_dd0_na_              285  5.433222e-03
# vth_08pwl60_lt_mv_               289 -4.666748e-03
# vth_08pwl31_mh0_lt_mv_           290  3.793929e-03
# wlrc_100_pcs_                    292 -1.473675e-05
# wlds0leak_mid_2_pcs_             294 -1.543233e-06
# wlds0leak_top_1_pcs_             295  1.213713e-02
# frlt_sfbcmin_er3_pcs_            301  8.896494e-05
# frlt_vcgrsft_ar3_pcs_            302 -1.267228e-01
# frlt_vcgrsft_er3_pcs_            306  1.686916e-01
# frlt_vcgrsft_fr3_pcs_            307 -2.819182e-02
# frlt_vcgrsft_gr3_pcs_            308  9.770092e-02
# tlcwc_we0_wl63_fr_s25_a2r_pcs_   313  1.792217e-02
# sfbc_drtime_s_                   315  2.991179e-04
# sfbc_b32wlsaerx_fresh_pcs_       318 -2.229785e-03
# sfbc_t32wlsaerx_dc_lijl2_pcs_    320 -3.852614e-05
# sfbc_b32wlsalp_dc_post_pcs_      321  6.316218e-04
# sfbc_64wls01t0_dc_post_pcs_      322  3.184699e-04
# sfbc_t32wlsalp_drpost_pcs_       323  1.268153e-04
# sfbc_b32wlsaup_drpost_pcs_       325  1.531529e-04
# sfbc_64wls00t1_dc_drpost1_pcs_   326  4.903757e-02
# crd_m1bll_slc2a_p_               332 -6.257313e-01
# crd_scrnslcbot_p_                333  5.676548e-01
# bbk_mhopen2_p_                   334 -2.262491e-02
# veralooptrial1__                 335  4.258576e-02
# vpgmulooptrial2__                338 -3.031528e+00
# crd_fastdin_ev_c_pcs_            339  5.442699e-04
# bbk_wlmh_6_d_pcs_                341 -2.729350e-02
# sfbc_t32wlsalp_far_dc_fresh_pcs_ 343  6.165242e-05
# sfbc_t32wlsaerx_far_fresh_pcs_   344 -5.665194e-04
# levelMP                          346  2.548730e+00
# levelUP                          347 -2.811243e-01
# cycling                          348  9.935521e-04

res_sort <- res[order(- res$sl_coef),]  # sorting coeff
res_sort
write.csv(res_sort, "L_LTPD_lasso1.csv")
######----------------LASSO 2-------------------------------------
set.seed(5)

x <- model.matrix(fbc~. , -345, data=df )
x %>% names
x <- data.table(x)

x %>% names
x <- as.matrix(x)
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

f <- as.vector(l_name_coeff[,1])
f



####--------------------------    GLM based on the LASSO pded1-----------------------------
# try to make subset of variables picked by Lasso pred1 
# vector of columns you DON'T want
f <- as.vector(l_name_coeff[,1])
f <- f[-1]
f %>% str
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
df_m <- df_m[, -343]
df_m %>% names
# subset of all data with selected  21 veriables
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
write.csv(res_sort1, file =  "L_contribution_LTPD_Lasso_stan_anova.csv")

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
write.csv(gbm.imp, "L_LTPD_GBMachine_subset.csv")
#

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
f.predict <- predict(df.boost,test[,-345],best.iter)
f.predict 
df %>% dim
f1.predict <- predict(df.boost, train[,-345], n.trees = 200, type = "response")
f1.predict
gbm.preddf <- data.frame(train[,345], f1.predict)

head(data.frame("PredictedProbability" = f1.predict,"Actual" = train$fbc))

plot( f1.predict, train$fbc, 
      
      xlab = "prediction", ylab = "fbc",col = "", 
      main = "Rate of actual LTPD FBC rate againt of predicted GBM") 
plot(f1.predict, train$fbc,   xlab = "prediction", ylab = "fbc",col = " lightblue", main = "Rate of actual LTPD FBC growth against to predicted, 
     based on Gradient Boosting Machine")            
# plot prediction as a slop for the 151 dies
abline(lm( train$fbc ~ f1.predict, data=df))
# plot  perfect prediction line red
abline(lm(train$fbc ~ f1.predict + 0),  col="blue")


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
# df_m <- df_m[, -"level"]
# df_m %>% names
# #df_m$levelMP <- rename(df_m$`df$levelMP`)
# colnames(df_m)[347] <- "levelLP" 
# colnames(df_m)[348] <- "levelMP"
# colnames(df_m)[349] <- "levelUP"
# 
# df_m %>% names
ss <-df%>% select(f)
ss %>% dim
ss %>% names
ss
# add responce 
df$fbc
ss$fbc <-df$fbc
write.csv(ss, "L_LTPD_GBM_subset.csv")
lmod_GBM_Rtpd <- lm(fbc~., ss)
lmod_GBM_Rtpd
summary(lmod_GBM_Rtpd)
lmod_GBM_Rtpr <-lmod_Lasso_Rtpd %>% summary
capture.output(lmod_GBM_Rtpr, file = "L_LTPD_GMB_linearModel.txt")

# Linear model with standardizing predictors
ss %>% dim
ss %>% names
# standartization  # normalize

ss %>% names
ss1 <- lapply(ss, scale)

# ss$level <-as.numeric(ss$level)
# ss$level <- ss$level
ss1 %>% names
st_lmod_rGBM <- lm(fbc~., ss1)
st_lmod_rGBM %>% summary



# Percentage of vriance explaned  Contribution Percent (%) (Ssi/SSt)
af.rf <- anova(lmod_GBM_Rtpr)
afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
#  trivially, scale them by 100 divided by their sum.
print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
write.csv(result, file = "L_contribution_LTPD_GBM.csv")

# anova with stndardized Lasso var
af.rf <- anova(st_lmod_rGBM)
afss.rf <- af.rf$"Sum Sq"     # there we have the incremental variance explained; how do we get the proportion?
#  trivially, scale them by 100 divided by their sum.
print(cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100))
result<-cbind(af.rf,PctExp=afss.rf/sum(afss.rf)*100)
res_sort <- result[order(- result$PctExp),]  # sorting coeff
res_sort1 <- res_sort[-2,]
res_sort1 <- res_sort1[1:30,]
write.csv(res_sort1,  file = "L_contribution_LTPD_GBM_stan_anova.csv")

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