df <- read.csv("/home/atalebi/qualhgALL1.18.2017.tsv", sep = "\t", header = TRUE)
names(df) <- gsub("ds_1z_t6fk8_sme1_plot.", "", names(df))
t <- grep(".00C", dq$lot, value = T)
t1 <- table(t)
lotnames <- names(t1)

## choosing levels of a factor and dropping the rest:
t <-levels(bcsdq1$lot)
badlot<- t[c(1:7, 47:50)]
goodlot <- t[-c(1:7, 47:50)]
bcsdq1 <- subset(bcsdq, lot %in% goodlot)
bcsdq1$lot <- factor(bcsdq1$lot)


library(plyr)
library(quantreg)
sec_names <- grep("sec", names(df), value = TRUE)
df[, sec_names] <- sapply(df[, sec_names], as.character)
df[, sec_names] <- sapply(df[, sec_names], as.numeric)
df$cycling <- as.numeric(df$cycling)

df$FBC <- apply(df[, sec_names], 1, max)

Znm_128G_ED3_2P <- subset(df, productname =="1Znm_128G_ED3-2P" )
Znm_128G_ED3_2P <- droplevels(Znm_128G_ED3_2P )


z <- rnorm(dim(Znm_128G_ED3_2P)[1]) # add this random noise to FBC to get rid of duplicates
Znm_128G_ED3_2P$fbc <- Znm_128G_ED3_2P$FBC + z

testnames <-split(Znm_128G_ED3_2P, Znm_128G_ED3_2P$testname)
for (i in 1:length(testnames)){
  testnames[[i]] <- testnames[[i]][, c("testname", "lot", "wafer", "x", "y", "cycling", "FBC", "fbc")]
}
lapply(names(testnames), function(x) {
  x1 <- testnames[[x]]
  save(x1, file = paste0(getwd(), '/',x, '.RData'))
})



testnames1 <- dlply(testnames[[1]], .(lot,wafer, x, y))

for (i in 1: length(testnames)){
  write.csv(testnames[[i]], paste0(names(testnames[i]), '.csv' "))
}

## Use droplevels(data) to get rid of unwanted levels of factor)

# Alternatively use: out <- dlply(df, .(productname, testname))
names(out)
dim(out)
qreg <- function(df) tryCatch(rq(log(FBC)~cycling, tau = 0.9999, method = "fn",data = df), error = function(e) NULL)
models <- dlply(bcsdqFinal, .(testname), qreg)
coef <- ldply(models, coef)



#######################################################################################################
############################## Fit Distribution #######################################################

library(MASS)
neg.binom <- function(x) tryCatch({
fitdistr(x$maximum, "negative binomial", lower = c(0.001) )
}, error = function(e) NULL)
neg.bin1 <- dlply(df, .(lot, wafer, x, y,level),neg.binom )
est <- ldply(neg.bin1, coef)
pois <-function(x){
fitdistr(x$maximum, "poisson")$coef
data.frame(Lambda = est)
}
pois1<- dlply(df, .(lot, wafer, x, y,level),pois)
est.pois1<-ldply(pois1,coef)


pois <-function(x) tryCatch({
fitdistr(x$maximum, "poisson")$estimate
}, error = function(e) NULL)

pois.est <- ddply(df, .(lot, wafer, x, y, level),pois)#worked

neg.binom <- function(x) tryCatch({
fitdistr(x$maximum, "negative binomial")$estimate
}, error = function(e) NULL)

neg.bin1 <- ddply(df, .(lot, wafer, x, y, level),neg.binom )# worked

qntReg <- dlply(aneesh.Sample, .(lot, wafer, x, y), qreg)
est_nb <- ddply(dq, .(testname, lot, wafer,x, y cycling), neg.binom)


nb.model <- function(df) tryCatch({
glm.nb(df$FBC ~ df$cycling)}, error = function(e) NULL)



qntRegcoef <- ldply(qntReg, coefficients)



