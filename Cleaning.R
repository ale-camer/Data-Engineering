# OUTLIERS

a = data.frame(c(rnorm(48,500,10),rnorm(2,500,10)*1000),
               c(rt(3,500,10)*1000,rt(47,500,10)),
               c(runif(45,46,123),runif(5,46,123)*-1000),
               c(rlnorm(45,50,10),rlnorm(5,50,10)*1000))
colnames(a) = c("a","b","c","d")

for(i in 1:ncol(a)){ 
  max_in = max(boxplot(a[,i], plot=FALSE)$stats)
  min_in = min(boxplot(a[,i], plot=FALSE)$stats)
  a[a[,i] > max_in, i] = max_in # change maximum outliers with the maximum value of the column
  a[a[,i] < min_in, i] = min_in # change minimum outliers with the minimum value of the column
}
a

for(i in 1:ncol(a)){
  mean = mean(boxplot(a[,i], plot=FALSE)$stats)
  max_in = max(boxplot(a[,i], plot=FALSE)$stats)
  min_in = min(boxplot(a[,i], plot=FALSE)$stats)
  a[a[,i] > max_in | a[,i] < min_in, i] = mean # change minimum and maximum outliers with the mean value of the column
}
a

# MISSING 

a = data.frame(c(rnorm(10,500,10),NA,NA,NA),c(NA,NA,NA,rt(10,500,10)),runif(13,46,123),rlnorm(13,50,10))
colnames(a) = c("a","b","c","d")
for(i in 1:ncol(a)){
  a[is.na(a[,i]), i] = mean(a[,i], na.rm = TRUE) # change missing value with the mean value of the column
}
a

a = data.frame(c(rnorm(10,500,10),NA,NA,NA),c(NA,NA,NA,rt(10,500,10)),runif(13,46,123),rlnorm(13,50,10))
colnames(a) = c("a","b","c","d")
for(i in 1:ncol(a)){
  a[is.na(a[,i]), i] = median(a[,i], na.rm = TRUE) # change missing value with the median value of the column
}
a

a = data.frame(c(rnorm(10,500,10),NA,NA,NA),c(NA,NA,NA,rt(10,500,10)),runif(13,46,123),rlnorm(13,50,10))
colnames(a) = c("a","b","c","d")
for(i in 1:ncol(a)){
  a[is.na(a[,i]), i] = max(a[,i], na.rm = TRUE) # change missing value with the maximum value of the column
}
a

a = data.frame(c(rnorm(10,500,10),NA,NA,NA),c(NA,NA,NA,rt(10,500,10)),runif(13,46,123),rlnorm(13,50,10))
colnames(a) = c("a","b","c","d")
for(i in 1:ncol(a)){
  a[is.na(a[,i]), i] = min(a[,i], na.rm = TRUE) # change missing value with the minimum value of the column
}
a

a = data.frame(c(rnorm(10,500,10),NA,NA,NA),c(NA,NA,NA,rt(10,500,10)),runif(13,46,123),rlnorm(13,50,10))
colnames(a) = c("a","b","c","d")
for(i in 1:ncol(a)){
  a[is.na(a[,i]), i] = quantile(a[,i], probs = 0.25, na.rm = TRUE) # change missing value with the 25th quantile of the column
}
a

a = data.frame(c(rnorm(10,500,10),NA,NA,NA),c(NA,NA,NA,rt(10,500,10)),runif(13,46,123),rlnorm(13,50,10))
colnames(a) = c("a","b","c","d")
for(i in 1:ncol(a)){
  a[is.na(a[,i]), i] = quantile(a[,i], probs = .90, na.rm = TRUE) # change missing value with the 90th percentile of the column
}
a

a = data.frame(c(rnorm(10,500,10),NA,NA,NA),c(NA,NA,NA,rt(10,500,10)),runif(13,46,123),rlnorm(13,50,10))
colnames(a) = c("a","b","c","d")
col_na = colnames(a)[apply(a, 2, anyNA)] # columns with NA's (in this case columns "a" and "b")
col_na

col_1 = as.vector(na.omit(a[,"a"])) # column "a"
a_1 = a[which(!is.na(a[,"a"]), arr.ind = T),]
max_cor_col_1 = order(cor(col_1,a_1), decreasing = T)[2] # column with more correlation with response variable
table_1 = data.frame(col_1,a_1[max_cor_col_1])
coefs_1 = lm(table_1[,1] ~ table_1[,2], table_1) # change the missing values by simple linear regression
a[which(is.na(a[,"a"]), arr.ind = T),"a"] = coef(coefs_1)[1] + coef(coefs_1)[2] * a[which(is.na(a[,"a"]), arr.ind = T),max_cor_col_1]

col_2 = as.vector(na.omit(a[,"b"])) # column "b"
a_2 = a[which(!is.na(a[,"b"]), arr.ind = T),]
max_cor_col_2 = order(cor(col_2,a_2), decreasing = T)[2] # column with more correlation with response variable
table_2 = data.frame(col_2,a_2[max_cor_col_2])
coefs_2 = lm(table_2[,1] ~ table_2[,2], table_2) # change the missing values by simple linear regression
a[which(is.na(a[,"b"]), arr.ind = T),"b"] = coef(coefs_2)[1] + coef(coefs_2)[2] * a[which(is.na(a[,"b"]), arr.ind = T),max_cor_col_2]

# CATEGORICAL 

a = data.frame(c("A","A","A","C","S","F","D","D","D","F"),
               c("D","D","S","D","F","G","E","G","H","C"),
               c("C","S","D","F","G","S","G","H","J","E"),
               c("A","S","F","G","D","G","E","D","F","H"))
colnames(a) = c("a","b","c","d")
for(i in 1:nrow(a)){ # encode character with integer in each column
  a[is.character(a[i,i]), i] = as.numeric(as.factor(a[,i]))
}
a <- as.data.frame(sapply(a, as.numeric))

# STANDARDIZE

stand = function(x) { # stardardize each column
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
a_stand = apply(a, 2, stand)
a_stand

norm = function(x){ # normalize each column
  (x - mean(x, na.rm = TRUE)) / (sd(x, na.rm = TRUE))
}
a_norm = apply(a, 2, norm)
a_norm

a_prob = pnorm(a_norm, mean = 0, sd =1) # probability of each observation in a column
a_prob