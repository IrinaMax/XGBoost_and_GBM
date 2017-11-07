# work to join ds and cq table
library(glmnet)
library(dplyr)
library(plyr)
library(corrplot)
library(caret)
library(devtools)
# work with my ds table with coeff 

df<- read.csv("ds_slop_0828.csv", header = TRUE, sep = ",", na.strings = c("NULL", "NaN", "NA"), stringsAsFactors = TRUE)
df_copy <- df # save original  df as df1
#df <-df_copy
#names(df) <- substring(names(df[,1:348]), 3) remove  first 2 elements from names
nm<- names(df)
nm
str(df_k)
tail(df_k)
# identefy and remove fisrt 36colomn and last 5 and remove them
n1 <- names(df[,1:36])
n1
n2 <- names(df[, 1652:1657])
n2
df <- df[, -c(1:36, 1652:1657) ]
df <- n

# sub("c.*", "", df[,1])
# names(df) = gsub(pattern = "b.*", replacement = "", x = names(df))

df %>% names %>%  head
# remove first 2 letters in col names
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
df3 <-df
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
# 821
cat('Number of columns with near zero variance: ,',length(a),'\n')
cat('Name of These columns: ,')
cat(a,'\n', sep = ',')
df4 <- df
names_b_cor <- names(df4)
names_b_cor
write.csv(names_b_cor, "names_b_cor.csv")

#df <- df[,-c(1:36,)]
#df <- df[,-c(798:804)]




df.cor <- data.matrix(df)

df.cor <- cor(df.cor, use = "pairwise.complete.obs")
CorPath <- capture.output(cat(substr(outputPath,1,nchar(outputPath)-4),'_CorMatrix.csv',sep = ""))
write.csv(df.cor, CorPath, row.names=FALSE, na="")
write.csv(df.cor, "CorMatrixHTPD.csv", row.names=FALSE, na="")

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
df1$slop
##  save as csv file for next step
write.csv(df, outputPath, row.names=FALSE, na="")
write.csv(df, "htpd_dsslop_clean.csv", row.names=FALSE, na="")
sink()
#

# # I lost colomns of coeff, i'm going to add it from saving, I pull it from the first saved copy
df[,"slop"]<- df_copy$u.coeff

 df %>% names
 df6 <- df   # last copy of clining dataframe
 write.csv(df, "htpd_dsslop_clean.csv", row.names=FALSE, na="")
df$slop
df %>% dim
#[1] 152 354           result of the cleaning data DS with slop
####      E N D  CLEANING #################################---------------------------------

#  0830 update with more elemenatiing columns
#

df %>% colnames

drop.cols <- grep("avro_file_name$", colnames(df))
drop.cols
df[, (drop.cols) := NULL]
write.csv(df, "htpd_dsslop_clean_0830.csv")
####      E N D  CLEANING #################################---------------------------------

#####   0828  -------------------   modeling

df %>% names

df
df_s <- data.table(df)
df_s %>% dim
df_s %>% names
x <- model.matrix(slop~. , -354, data=df_s )     # work but give matrix 152 x 353
#x <- as.numeric(df_wslop[, -356])   # give mistake
x <- as.matrix(df_s[,-354]) # Removes slop
x %>% dim

#Warning message:
#NAs introduced by coercion       WHY???
dfj <- na.omit(df_s)
dfj %>% dim
#[1] 151 354  I guess one die has NA 

x <- model.matrix(slop~. , -354, data=dfj )
y <- as.matrix(dfj[, 354]) # Only slop
y %>% dim
cv = cv.glmnet(x, y)
cv
cv %>% names
cv$lambda.min
#[1] 2.944957e-05
model = glmnet(x, y, type.gaussian="covariance", lambda=cv$lambda.min)
model
# Call:  glmnet(x = x, y = y, lambda = cv$lambda.min, type.gaussian = "covariance") 
# 
# Df   %Dev    Lambda
# [1,] 22 0.3495 3.232e-05
model %>% names
model$lambda
pred1 <- predict(model, type="coefficients")
pred1
# I want to make the table with coloms
temp <- pred1%>% summary
temp$i
pr <- as.matrix(pred1)
pr
res <- data.frame(which( !pr ==0, arr.ind = T))
res$col<- NULL
res
res$sl_coef <- pr[which( !pr ==0, arr.ind = T)]
res %>% summary

write.csv(res, "result_of_prediction1.csv")


ln <- lm(sl_coef~. , res)
ln %>% summary
# Call:
# lm(formula = sl_coef ~ ., data = res)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -0.0066783 -0.0000032  0.0001867  0.0006624  0.0007880 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) -8.084e-04  4.776e-04  -1.693    0.103
# row          3.446e-06  2.534e-06   1.360    0.186
# 
# Residual standard error: 0.001444 on 24 degrees of freedom
# Multiple R-squared:  0.07155,	Adjusted R-squared:  0.03286 
# F-statistic: 1.849 on 1 and 24 DF,  p-value: 0.1865



pred2 <- predict(model,x, s=3.232e-05,type="response")
plot(pred2, y,  xlab = "prediction", ylab = "SLOP",col = "dark red", main = "expectation 2, lambda min= 3.232e-05")             # plot prediction as a slop for the 151 dies


pred3 <- predict(model, x,s=2.944957e-05,type="response")
pred3
plot(pred3, y,  xlab = "prediction", ylab = "SLOP",col = "dark blue", main = "expectation 3, lambda min= 2.944957e-05")             # plot prediction as a slop for the 151 dies
# CV 5 fold
glmnet1<-cv.glmnet(x=x,y=y,type.measure='mse',nfolds=5,alpha=.5)
glmnet1 %>% names   # give lambda even more higher 
glmnet1$lambda.min
# [1] 6.170366e-05
pred2 <- predict(glmnet1, type="coefficient")

# # Fitting the model (Lasso: Alpha = 1)  type.misure 'mse'
glmnet2<-cv.glmnet(x=x,y=y,  type.measure='mse',nfolds=10,alpha=1)
glmnet2 %>% names 
#[1] "lambda"     "cvm"        "cvsd"       "cvup"       "cvlo"       "nzero"      "name"       "glmnet.fit" "lambda.min" "lambda.1se"
glmnet2$lambda.min  
#[1] 3.085183e-05
glmnet2$lambda.1se
#[1] 8.194516e-05

# extract coefficient with min lambda and names of LASSO result
c<-coef(glmnet2, s='lambda.min',exact=TRUE) 
c
coef_lambda <- coef(glmnet2, s='lambda.min',exact=TRUE) [which(coef(glmnet2, s = "lambda.min") != 0)]
colnames <- colnames(dfj)[which(coef(glmnet2, s = "lambda.min") != 0)]
##  Updated frame of coeff with names of the variable
tmp_coeffs <- coef(glmnet2, s = "lambda.min")
lasso_name_coeff<- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
write.csv(lasso_name_coeff, "lasso_name_coeff.csv")

# Fitting the model (Lasso: Alpha = 1)
set.seed(999)
cv.lasso <- cv.glmnet(x, y, alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')

# Results
plot(cv.lasso)
plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
cv.lasso$lambda.min
#[1] 3.385984e-05
cv.lasso$lambda.1se
cs.lasso
#[1] 5.391445e-05
coef(cv.lasso, s=cv.lasso$lambda.min)
system.time(cv.lasso <- cv.glmnet(x, y, alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc'))
#In the above code, we execute glm regression (note the family=’binomial’), in parallel (if a cluster or cores have been previously allocated), 
#internally standardizing (needed for more appropriate regularization) and wanting to observe the results of AUC (area under ROC curve). Moreover, 
#the method already performs 10-fold cross validation to choose the best λλ.
#"mgaussian"

cv.lasso1 <- cv.glmnet(x, y, family= "gaussian", alpha=1, parallel=TRUE, standardize=TRUE, type.measure='mae')
plot(cv.lasso1)
plot(cv.lasso1$glmnet.fit, xvar="lambda", label=TRUE)
cv.lasso1$lambda.min
cv.lasso1$lambda.1se
coef(cv.lasso1, s=cv.lasso1$lambda.min)[which(coef(cv.glmnet.fit, s = "lambda.min") != 0)]
system.time(cv.lasso1 <- cv.glmnet(x, y, family= "gaussian", alpha=1, parallel=TRUE, standardize=TRUE, type.measure='mae'))

# user  system elapsed 
# 0.652   0.021   0.683 


## model of Forward selection  all set------------------------------------------------------
library(leaps)
regfit.fwd = regsubsets(slop ~ ., data = df_s, nvmax = 353, method = "forward")    #working
regf.summary <-summary(regfit.fwd)
regf.summary
names(regf.summary)
 
rfit <- regsubsets(x,y,df_s )
regf.summary$bic
regf.summary$adjr2   ##  waw :O
regf.summary$rss %>% mean
plot(regf.summary$cp, xlab = "number of var", ylab = "Cp", main = "Forward selection for all set")
plot(regf.summary$bic, xlab = "number of var", ylab = "bic", main = "Forward selection for all set")
plot(regf.summary$rss, xlab = "number of var", ylab = "RSS", main = "Forward selection ")
coef(regfit.fwd, 90)

plot(regfit.fwd, scale = "Cp")   #working!

# backward selection
regfit.bwd.train = regsubsets(coeff ~ ., data = df_wslop, nvmax = 353, method = "backward")
reg.bwd.train.summary <-summary(regfit.bwd.train)
reg.bwd.train.summary
names(reg.bwd.train.summary)
plot(reg.bwd.train.summary$cp, xlab = "number of var", ylab = "Cp",col = "dark red", main = "Backward selection")
coef(regfit.bwd.train, 140 )
plot(regfit.bwd.train, scale = "bic")


plot(regfit.bwd.train, scale = "Cp")   #working, but not possible to see anything


############         WORKING WITH TRAINING AND TEST################
#lasso---------------------------------------------------------------------
## 75% of the sample size  or may be 80%  ???
set.seed(123)
smp_size <- floor(0.90 * nrow(df))

train_ind <- sample(seq_len(nrow(dfj)), size = smp_size, replace = FALSE  )

train <- dfj[train_ind, ]
test <- dfj[-train_ind, ]

#TRAIN
train %>% dim
names(train)
tx <- model.matrix(slop~. , -353, data=train )
ty <- as.matrix(train[, 353]) # Only slop
ty %>% dim

# TEST
test %>% dim
names(test)
y_test <- test[,353]
y_test %>% summary  # we need to to campare result
test <- model.matrix(slop~. , -353, data=test )
test

cvtrain = cv.glmnet(tx, ty)
cvtrain
cvtrain %>% names
cvtrain$lambda.min
#[1] 3.241897e-05
model_train = glmnet(tx, ty, type.gaussian="covariance", lambda=cvtrain$lambda.min)
model_train
# Call:  glmnet(x = x, y = y, lambda = cv$lambda.min, type.gaussian = "covariance") 
# 
# Df   %Dev    Lambda
# [1,] 22 0.3495 3.232e-05
model_train %>% names
model_train$lambda
#[1] 3.241897e-05
pred1_test <- predict(model_train, test, type="coefficients")
pred1_test
#n
temp <- pred1_test%>% summary
temp$i
#  [1]   1  10  14  18  23  26  38  75  79  97 171 186 192 195 199 201 224 237 298 307 321 352
pr <- as.matrix(pred1_test)
pr
res <- data.frame(which( !pr ==0, arr.ind = T))
res$col<- NULL
res
res$sl_coef <- pr[which( !pr ==0, arr.ind = T)]
res %>% summary
res

plot(pred1_test, xlab = "prediction", ylab = "expected SLOP",col = "darkred", main = "expectationon 1 on test, ")

pred2_test <- predict(model_train, test, s=3.241897e-05, type="response")
pred2_test %>% summary
plot(pred2_test,  xlab = "prediction", ylab = "expected SLOP",col = "dark red", main = "expectationon 2 on test, lambda min= 3.241897e-05")             # plot prediction as a slop for the 151 dies


pred3 <- predict(model_train, test ,s=2.944957e-05,type="response")
pred3
plot(pred3, y_test ,  xlab = "prediction", ylab = "SLOP",col = "dark blue", main = "expectation 3, lambda min= 2.944957e-05")             # plot prediction as a slop for the 151 dies
# CV 5 fold
glmnet1<-cv.glmnet(x=x,y=y,type.measure='mse',nfolds=5,alpha=.5)
glmnet1 %>% names   # give lambda even more higher 
glmnet1$lambda.min
# [1] 6.170366e-05
pred2 <- predict(glmnet1, type="coefficient")

# # Fitting the model (Lasso: Alpha = 1)  type.misure 'mse'
glmnet2<-cv.glmnet(x=x,y=y,  type.measure='mse',nfolds=10,alpha=1)
glmnet2 %>% names 
#[1] "lambda"     "cvm"        "cvsd"       "cvup"       "cvlo"       "nzero"      "name"       "glmnet.fit" "lambda.min" "lambda.1se"
glmnet2$lambda.min  
#[1] 3.085183e-05
glmnet2$lambda.1se
#[1] 8.194516e-05

# extract coefficient with min lambda and names of LASSO result
c<-coef(glmnet2, s='lambda.min',exact=TRUE) 
c
coef_lambda <- coef(glmnet2, s='lambda.min',exact=TRUE) [which(coef(glmnet2, s = "lambda.min") != 0)]
colnames <- colnames(dfj)[which(coef(glmnet2, s = "lambda.min") != 0)]
##  Updated frame of coeff with names of the variable
tmp_coeffs <- coef(glmnet2, s = "lambda.min")
lasso_name_coeff<- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
lasso_name_coeff
write.csv(lasso_name_coeff, "lasso_name_coeff.csv")






# work with Kirthi table
df_k <- read.csv("query_result-21.csv", header = TRUE, sep = ",", na.strings = c("NULL", "NaN", "NA"), stringsAsFactors = TRUE)
df_copy <- df_k  # save original  df as df1
#df <-df_copy
#names(df) <- substring(names(df[,1:348]), 3) remove  first 2 elements from names
nm_k <- names(df_k)
nm_k
str(df_k)
tail(df_k)




