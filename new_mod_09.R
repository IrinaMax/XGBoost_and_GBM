# The code with DQ data, to fit linear regression for every die
library(glmnet)
library(dplyr)
library(plyr)
library(corrplot)
library(caret)
library(devtools)
library(data.table)

# diequal table HTPD
b3 <- read.csv("0816_HTPD_dqmaxfbc.csv")
summary(b3)
# rename colomns for easy to use
b3 <- rename(b3, c("X_c6"="FBC", "a.lot"="lot",     "a.wafer"="wafer",  "a.x"= "x"      , "a.y"= "y",       "a.blk" ="blk" ,   "a.cycling" = "cycling"))
b3 %>% names   # [1] "a.lot"     "a.wafer"   "a.x"       "a.y"       "a.blk"     "a.cycling" "X_c6" 
which.max(b3$FBC)
# remove outlier
b3[2875,]
out999 <- b3[2875,]
b3 <- b3[-2875, ]
row.names(b3) <- 1:nrow(b3)
row.names(b3)
b3 %>% dim
write.csv(b3, "09_HTPD_dqmaxfbc.csv")
head(b3)
# diequal table RTPD
b3_r <- read.csv("rtpd_maxfbc_0918.csv")
#      on the cluster
##     b3_r <- read.csv("/home/irinam/BICS3/rtpd_maxfbc_0918.csv")
summary(b3_r)
# rename colomns for easy to use
b3_r <- rename(b3_r, c("X_c6"="FBC", "a.lot"="lot", "a.wafer"="wafer","a.x"= "x", "a.y"= "y", "a.blk" ="blk","a.cycling" = "cycling"))
b3_r %>% names   # [1] "a.lot"     "a.wafer"   "a.x"       "a.y"       "a.blk"     "a.cycling" "X_c6" 
which.max(b3_r$FBC)
# remove outlier
out999 <- b3_r[5684,]
b3_r <- b3_r[-5684, ]
row.names(b3_r) <- 1:nrow(b3_r)
row.names(b3_r)
dim(b3_r)
write.csv(b3_r, "09_RTPD_dqmaxfbc.csv")
head(b3_r)

## LTPD
b3_l <- read.csv("Ltpd_maxfbc_0921.csv")
# on the cluster
##     b3_r <- read.csv("/home/irinam/BICS3/rtpd_maxfbc_0918.csv")
summary(b3_l)
# rename colomns for easy to use
b3_l <- rename(b3_l, c("X_c6"="FBC", "a.lot"="lot",     "a.wafer"="wafer",  "a.x"= "x"      , "a.y"= "y",       "a.blk" ="blk" ,   "a.cycling" = "cycling"))
b3_l %>% names   # [1] "a.lot"     "a.wafer"   "a.x"       "a.y"       "a.blk"     "a.cycling" "X_c6" 
which.max(b3_l$FBC)
#[1] 5684
# remove outlier by removing the row 
out999 <- b3_l[5684,]
b3_l <- b3_l[-5684, ]
row.names(b3_l) <- 1:nrow(b3_l)
row.names(b3_l)
dim(b3_l)
write.csv(b3_l, "09_LTPD_dqmaxfbc.csv")
head(b3_l)
b3 <- data.frame(b3)
# add temp column to the data 
b3$tem <- rep(85, nrow(b3))   # new col with temt 85
b3_r$tem<- rep(25, nrow(b3_r))   # new col with temt 25
b3_l$tem<- rep(-15, nrow(b3_l))   # new col with temt -15

# stuck all data together with cycling, FBC and temerature
st_dq <- rbind(b3, b3_r,b3_l)
st_dq %>% dim
#[1] 84104     8
st_dq %>% names
write.csv(st_dq, "ht_rt_lt_DQ_cyc_maxFBC.csv")
# ------------------------------------------------ END OF WORK WITH DIE QUAL----------------------------


#after retreaving data in HIVE join th DQ and DS tables by query
# SELECT *
#   FROM default.ds_bics_t6rn1_sme1_plot c 
# join irina_db.dq_crosstemp u 
# on u.lot=(regexp_extract(c.lotno,'(.*)\\.',1))
# and u.x=c.originaldiex as int
# and u.y=c.originaldiey as int
# and cast(u.wafer as int)=cast(regexp_extract(c.waferno, '.*\\.(\\d+)',1) as int);


# CLEANING DATA FROM DS
adf <- read.csv("crosstemp_DSDQ.csv", header = TRUE, sep = ",", na.strings = c("NULL", "NaN", "NA"), stringsAsFactors = TRUE)
adf %>% dim
df <- adf
# make copy of original table and work with copy in case if something going wrong...
df %>% dim
#df <-df_copy
#names(df) <- substring(names(df[,1:348]), 3) remove  first 2 elements from names
nm<- names(df)
nm
str(df)
tail(df)
# I am going to save cycling fbc and tem columns for future
df.c.f.t <- df[,c(1780:1782)]
df.c.f.t %>% head

# identefy and remove fisrt 36colomn and last 5 and remove them
n1 <- names(df[,1:36])
n1
n2 <- names(df[, 1774:1782])
n2
df <- df[, -c(1:36, 1774:1782) ]


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
# 821
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

# now I will add back cycling, FBC and temperature
df$cyclinng <- df.c.f.t$u.cycling
df$fbc <- df.c.f.t$u.fbc
df$tem <- df.c.f.t$u.tem
df %>% dim
# [1] 84104   352

##  save as csv file for next step
write.csv(df, outputPath, row.names=FALSE, na="")
write.csv(df, "crosstemp_DSDQ_cleaned.csv", row.names=FALSE, na="")
sink()
#
drop.cols <- grep("avro_file_name$", colnames(df))
drop.cols
df[, (drop.cols) := NULL]

mod1 <- lm(fbc~., data= df)
summary(mod1)
# model with CV and lambda min
set.seed(123)
x <- model.matrix(fbc~. , -352, data=df )
x[, 350:353]
dfj[, 350:353]
y <- as.matrix(dfj[, 353]) # Only slop
y %>% dim
cv = cv.glmnet(x, y)

