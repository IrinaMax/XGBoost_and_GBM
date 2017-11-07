 

 #LTPD and MP
  ## 
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
  
  b3_l[c(1459,2230),]
  #            lot wafer  x  y level cycling  FBC
  #1459 CP0939223    18  7 15    MP    3000  128
  #2230 CP0938453     6 28 13    MP    3000 1017
  # remove outlier by removing the row 318 with fbc 993 and all > 9
  b3_l <- b3_l[-c(2230), ]
  row.names(b3_l) <- 1:nrow(b3_l)
  row.names(b3_l)
  dim(b3_l)
  #[1] 2431    6
  write.csv(b3_l, "F_LTPD_MP_dq.csv", row.names = FALSE)
  head(b3_l)
  
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
  # make copy of original table and work with copy in case if something going wrong...
  df <- LMPdf
 
  df %>% dim  # [1] 2429 1781
  
  #names(df) <- substring(names(df[,1:348]), 3) remove  first 2 elements from names
  nm<- names(df)
  nm
  str(df)
  tail(df)# I am going to save cycling fbc and tem columns for future
  df.c.f.t <- df[,c(1779:1781)]
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
  #[1] "u.lot"     "u.wafer"   "u.x"       "u.y"       "u.level"   "u.cycling" "u.fbc"     "u.tem"
  # cut away n1 and n2
 
   # find the slop 
  library(data.table)
  datDTLM <- data.table(df,  key= c("u.wafer", "u.x", "u.y", "u.lot"))
  datDTLM %>%dim
  
  LMP_coef<-datDTLM[, list(coef(lm(u.fbc~u.cycling))), by = key(datDTLM)]  #give a matrix  of 304 with intercept and slop
  LMP_coef
  LMP_coef<- rename(LMP_coef, c("V1" = "coeff" ))
  LMP_coef %>% dim
  write.csv(LMP_coef, "LMP_dies_coef_.csv")
  #write.csv2  (b3r_coef, "dies_coefr.csv")
  
  # we need to take only every second line to pull the slop, the first lines are intercept
  LMP_slop <- LMP_coef[-seq(1, NROW(LMP_coef), by = 2)]
  LMP_slop %>% dim
  LMP_slop <-as.data.frame(LMP_slop)
  write.csv(LMP_slop, "LMP_dq_slop.csv")
  
  
  df <- df[, -c(1:36, 1774:1781) ]
  
  
  # sub("c.*", "", df[,1])
  # names(df) = gsub(pattern = "b.*", replacement = "", x = names(df))
  
  df %>% names 
  # remove first 2 simbols in col names to make it easy to read
  names(df) <- substring(names(df) , 3)
  
  df1 <- df  # save original  df as df1
  df %>% dim 
  df %>%  names
  #df$u._c6
  #which.max(df$u._c6)   #  show the only one row with 999 fbc, Im going to check it out and remove
  #df[13052, 1650:1658] 
  # c.bl_pl_ers_sgs__ c.bbk_singlepulse_ev_d_pcs_     u.lot u.wafer u.x u.y u.blk u.cycling u._c6
  # 13052                NA                          NA CP0938453       6  28  13   99B      3000   999
  #df <- df[ -13052, ]  # remove line 13052 as outlier
  #row.names( df) <- 1:nrow(df)
  # just look the row is gone
  #df[13052, 1650:1658]
  #rename colomn with FBC
  #df <- rename(df, c("u._c6" = "FBC" ))
  
  # df <- rename(df, c("u._c6"="FBC", "u.lot"="lot",     "u.wafer"="wafer",  "u.x"= "x"      , "u.y"= "y",       "u.blk" ="blk" ,   "u.cycling" = "cycling"))
  # df2 <- df   # make copy before 2 step cleaning procedure
  
  # save diequal parametres 
  # saving <-df[,colnames(df) %in% c("lot",     "wafer" ,  "x"  ,     "y" ,      "blk" ,    "cycling", "FBC" ) ]
  # saving %>% head
  # #lm <- lm(FBC~., bf)
  # #summary(lm)
  
  
  ##______________________________________
  
  
  ##Let's look how many columns without missing value to see possibility to shrink it.
  # NoMissingdf <- colnames(df)[colSums(is.na(df)) == 0]
  # NoMissingdf   # 1253 col without NA
  
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
  # 813
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
  write.csv(df.cor, "CorMatrix_CrossTemp.csv", row.names=FALSE, na="")
  
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
  # now I will add back cycling, FBC and temperature
  df %>% dim
  df.c.f.t %>% dim
  df$cycling <- adf$u.cycling
  df$fbc <- df.c.f.t$u.fbc
  df$tem <- df.c.f.t$u.tem
  df$level <- LMPdf$u.level
  df %>% dim
  # [1] 2429  347
  
  library(data.table)
  df <- data.table(df)
  drop.cols <- grep("avro_file_name$", colnames(df))
  drop.cols
  drop.cols <- grep("original_file_$", colnames(df))
  drop.cols
  df[, (drop.cols) := NULL]
  df %>% dim
  df6 <- df
  # final resul after cleaning [1] 6077  352
  
  drop.cols <- grep("original_file_$", colnames(df))
  drop.cols
  ##  save as csv file for next step
  write.csv(df, outputPath, row.names=FALSE, na="")
  write.csv(df, "LTPD_MP_347var.csv", row.names=FALSE, na="")
  
  names(df)
 df %>% dim
  #-----------------LASSO--------------------------------------------------------
  
  #df[1,350:352]
  # model with CV and lambda min
  set.seed(5)
   # cut out the Level and temperature like constant value
  df <- df[, -c(346, 347)]
  df %>%  names
  # now FBC is #345 and omit all na
  df <- na.omit(df)
  df[!complete.cases(df),]
  row.has.na <- apply(df, 1, function(x){any(is.na(x))})
  row.has.na    ##   26rows  was been removed
  
  df %>% dim  #[1] 2413  345
  x <- model.matrix(fbc~. , -345, data=df )
  x %>% dim
  #[1] 2413  370
  x[, 340:345] %>% head   # no fbc
  df[, 340:345] %>% head      # with fbc
  y <- as.matrix(df[, 345]) # Only fbc
  y %>% dim
  y
  train$fbc <- as.character(train$fbc)
 train$fbc <- as.factor(train$fbc)
  cv = cv.glmnet(x, y)
#   Error in elnet(x, is.sparse, ix, jx, y, weights, offset, type.gaussian,  : 
#                    y is constant; gaussian glmnet fails at standardization step
# if you scale a column with constant values, you get zero SD and divide by zero gives NaN. 
# So remove these columns or set the NaNs to zero, these variables aren't predictive anyway.  
  # cv
  # plot(cv)
  # cv %>% names
  # cv$lambda.min
  # #[1] 3.385984e-05
  # model = glmnet(x, y, type.gaussian="covariance",  lambda=cv$lambda.min, standardize = FALSE, standardize.response = FALSE)
  # model_s = glmnet(x, y, type.gaussian="covariance",  lambda=cv$lambda.min, standardize = TRUE, standardize.response = TRUE )
  # model_s
  # plot(model, xvar = "lambda", label = TRUE)
  # plot(model_s, xvar = "dev", label = TRUE)
  # 
  # summary(model)
  # # Call:  glmnet(x = x, y = y, lambda = cv$lambda.min, type.gaussian = "covariance") 
  # # 
  # # Df   %Dev    Lambda
  # # [1,] 21 0.3177 3.386e-05
  # model %>% names
  # model$lambda
  # # [1] 3.385984e-05
  # pred1 <- predict(model_s, type="coefficients")
  # pred1
  # pred<-predict(model_s, newx = x[1:15,], s = "lambda.min")
  # pred
  # # I want to make the result table with coloms
  # temp <- pred1%>% summary
  # temp$i
  # #  [1]   1  10  14  18  23  26  38  75  79  97 171 186 192 195 199 201 224 237 298 307 321 352
  # pr <- as.matrix(pred1)
  # pr
  # res <- data.frame(which( !pr ==0, arr.ind = T))
  # res$col<- NULL
  # res
  # res$sl_coef <- pr[which( !pr ==0, arr.ind = T)]
  # res %>% summary
  # res
  # 
  # write.csv(res, "result_of_CV_prediction1.csv")
 
  # XGBoost----------------------------------------------------------------------------------
  
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
  
  
  df_copy <- df
  df <- na.omit(df)
  X_n = df[, -345]
  X_n
  y_n = df[, 345]
  dim(df)
  ## 75% of the sample size  or may be 80%  ???
  set.seed(123)
  smp_size <- floor(0.90 * nrow(df))
  
  train_ind <- sample(seq_len(nrow(df)), size = smp_size, replace = FALSE  )
  
  train <- df[train_ind, ]
  test <- df[-train_ind, ]
  
  #TRAIN
  train %>% dim
  #[1] 5433  352
  tx <- train[,-c(345)]
  #[1] 135 353
  names(train)
  tx <- model.matrix(fbc~. , -345, data=train )
  ty <- as.matrix(train[, 345]) # Only slop
  ty %>% dim
  ty
  # TEST
  test %>% dim
  #[1]  242 369
  names(test)
  y_st <- test[,345]
  y_st %>% summary  # we need to to campare result
  test <- model.matrix(fbc~. , -345, data=test )
  test <- test[, -c(345)]
  test
  df_mat <- model.matrix(fbc~., -345, data=df)
  y_mat <- as.matrix(df[,345])
  set.seed(123)
  #Train Random Forest
  library(randomForest)
  rf_LMP <-randomForest(fbc~.,data=train,keep.forest=FALSE, importance=TRUE,ntree=200)
  print(rf_LMP)
  # Call:
  #   randomForest(formula = ty ~ ., data = train, keep.forest = FALSE,      importance = TRUE, ntree = 1000) 
  # Type of random forest: regression
  # Number of trees: 1000
  # No. of variables tried at each split: 117
  # 
  # Mean of squared residuals: 1.56867e-08
  # % Var explained: 82.12
  
  rf <-randomForest(fbc~.,data=df, importance=TRUE,ntree=100)
  print(rf)
  # Call:
  #   randomForest(formula = slop ~ ., data = df, keep.forest = FALSE,      importance = TRUE, ntree = 1000) 
  # Type of random forest: regression
  # Number of trees: 1000
  # No. of variables tried at each split: 117
  # 
  # Mean of squared residuals: 7.599835e-08
  # % Var explained: 9.45
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
  
  
  ###   gbm
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
  train %>% dim
  test %>% dim
  df.boost=gbm(fbc ~ . ,data = df[train_ind, ],distribution = "gaussian",n.trees = 200,
               shrinkage = 0.01, interaction.depth = 4)
  df.boost
  # gbm(formula = fbc ~ ., distribution = "gaussian", data = df[train_ind, 
  # ], n.trees = 200, interaction.depth = 4, shrinkage = 0.01)
  # A gradient boosted model with gaussian loss function.
  # 200 iterations were performed.
  # There were 344 predictors of which 0 had non-zero influence.
  
  s<-summary(df.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance
  s 
  s<- data.table(s)
  s[1:20,]
  # var     rel.inf
  # 1:                              tem 68.56837693
  # 2:                            level 14.95446290
  # 3:       sfbc_b32wlsaup_drpost_pcs_  6.98134780
  # 4:                  vth_sgs_med_mv_  4.45158309
  # 5:                          cycling  3.54464492
  # 6: sfbc_t32wlsalp_far_dc_fresh_pcs_  0.85982695
  # 7:             crd_m1bll_224_c_pcs_  0.23196186
  # 8:                vth_12pwl3_lt_mv_  0.11601911
  # 9:      sfbc_b32wlsalp_dc_post_pcs_  0.10441571
  # 10:                  fbc_sdllk4_pcs_  0.08722952
  # 11:            vth_wlds0last_l3s_mv_  0.07596677
  # 12:        sfbc_slcerslp_5kpost_pcs_  0.02416446
  # 13:                        vf_sk_mv_  0.00000000
  # 14:                   iccsb_300a_ua_  0.00000000
  # 15:                   crd_ttl_pl0_p_  0.00000000
  
  pred.boost <-gbm(formula = fbc ~ ., distribution = "gaussian", data = test, n.trees = 200, interaction.depth = 4, shrinkage = 0.01)
  #A gradient boosted model with gaussian loss function.
  #10000 iterations were performed.
  #There were 13 predictors of which 13 had non-zero influence.
  
  pred.boost
  
  ### onother one RF
  library(ggplot2)
  library(plot3D)
  library(randomForestSRC)
  library(ggRandomForests)
  ggplot(train)+
    geom_boxplot(aes(y=fbc,x=cycling,fill=tem),notch=T,notchwidth=0.5)+
    facet_grid(~tem,margins=T)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ylab(label='fbc')+
    xlab('cycling')+
    ggtitle('Plot of the FBC by cycling on the cross temperaure ')
  
  p1=ggplot(train)+
    geom_point(aes(y=fbc,x=cycling,color=as.factor(tem)))+
    scale_x_log10()+scale_y_log10()+
    facet_grid(~cycling)+
    ylab('FBC')+
    xlab('Cycling')+
    ggtitle('Plot of the FBC by cycling with cross temperature')
  p1
  varImpPlot(rftrain)
  
  
  train$fbc <- as.character(train$fbc)
  train$fbc <- as.factor(train$fbc)
  rflm <- randomForest(fbc~.,data=df, importance=TRUE,regression= T, ntree=100)
  df %>% names
  ## plot Predicted VS Actual
  
  pred<-predict(object=rftrain,newdata=test[,-345])
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
  
  
  # Now we can compare the Out of Bag Sample Errors and Error on Test set
  # The above Random Forest model chose Randomly 4 variables to be considered at each split. We could now try all possible 13 predictors which can be found at each split.
  
  oob.err=double(30)
  test.err=double(30)
  #mtry is no of Variables randomly chosen at each split
  for(mtry in 1:100) 
  {
    rf1=randomForest(fbc ~ . , data = df , subset = train, mtry=mtry,ntree=100) 
    oob.err[mtry] = rf1$mse[200] #Error of all Trees fitted
    
    pred<-predict(rf1, test) #Predictions on Test Set for each Tree
    test.err[mtry]= with(test, mean( (medv - pred)^2)) #Mean Squared Test Error
    
    cat(mtry," ") #printing the output to the console
  }
  
  #Test error 
  test.err
  # Out if Bag Error Estimation
  oob.err
  #Plotting both Test Error and Out of Bag Error
  matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
  legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
  
  
  
  proc.time()- ptm
  test
  predRF <- predict(rf, newdata = xtest)
  predRF %>% dim
  table(predRF,train[,353] )
  
  
  
  
  
  
  
  #rf1 <-randomForest(df$fbc~.,data=df, mtry=30 importance=TRUE,ntree=500)
  # matrix interfae seems to be faster...
  df %>% dim
  x %>% dim
  y %>% dim
  ptm <- proc.time()
  rf1_mat <-randomForest(y= y, x= x, mtry=30, importance=TRUE,ntree=300)
  proc.time() - ptm
  print(rf1)
  # Call:
  # randomForest(formula = df$fbc ~ ., data = df, mtry = 30, importance = TRUE,      ntree = 500) 
  # Type of random forest: regression
  # Number of trees: 500
  # No. of variables tried at each split: 30
  # 
  # Mean of squared residuals: 0.5698504
  #% Var explained: 68.48
  
  #Evaluate variable importance
  tx <- model.matrix(fbc~. , -351, data=train )
  ty <- train[, 351] # Only slop
  mse_rf <- mean((df[,351] - rf1$predicted  )^2)
  mse_rf
  
  impRF<-rf1$importance    #worked
  importance(rf1, type =1 )
  impRF = importance(rf1, type =1)
  impRF <- data.frame(predictors=rownames(impRF),impRF)
  rf1$importanceSD
  rf1 %>% summary
  rf1$mse
  # Order the predictor levels by importance 
  # I guess %IncMSE of j'th is (mse(j)-mse0)/mse0 * 100%   so the higher number more importent
  
  imp.sortRF <- arrange(impRF,desc(impRF$X.IncMSE))
  imp.sortRF
  imp.sortRF$predictors <- factor(imp.sortRF$predictors,levels=imp.sortRF$predictors)
  imp.sortRF$predictors
  # Select the top 20 predictors
  imp.30<- imp.sortRF[1:30,]
  print(imp.30)
  write.csv(imp.30, "RandomForest_500t_30impvar.csv")
  #write.csv(imp.20, "RandomForest_100t_20Var.csv")
  
  #try to arrange by IncNodePurity
  imp.sortRFNode <- arrange(impRF,desc(impRF$IncNodePurity))
  imp.sortRFNode
  imp.sortRFNode$predictors <- factor(imp.sortRFNode$predictors,levels=imp.sortRFNode$predictors)
  imp.sortRNodeF$predictors
  # Select the top 20 predictors
  imp.30Node<- imp.sortRFNode[1:30,]
  print(imp.30Node)
  write.csv(imp.30Node, "RandomForest_500t_30impvarIncNodePurity.csv")
  
  imp.max <- as.matrix(imp.sortRF)
  plot(imp.sortRF[1:30,])
  barplot(imp.sortRF[1:30,2])
  
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
  
  