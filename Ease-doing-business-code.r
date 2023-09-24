```{r echo=FALSE,message=FALSE, warning=FALSE, results='hide'}
#INTRO TO DATA SCIENCE ASSIGNMENT 2, GROUP 14
if (!require("reshape2")) install.packages("reshape2")
if (!require("corrplot")) install.packages("corrplot")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("plot3D")) install.packages("plot3D")
if (!require("ISLR")) install.packages("ISLR")
if (!require("plotrix")) install.packages("plotrix")
if (!require("boot")) install.packages("boot")
if (!require("GGally")) install.packages("GGally")
if (!require("moments")) install.packages("moments")
if (!require("Metrics")) install.packages("Metrics")
```

### Task 1

```{r ,message=FALSE, warning=FALSE, results='hide',fig.show='hide'}
library(readr)
Team_14_db_data <- read_csv("C:/Users/tamir/Downloads/Team_14_db_data.csv")
Team_14_gei_data <- read_csv("C:/Users/tamir/Downloads/Team_14_gei_data.csv")
gei <- Team_14_gei_data
db <- Team_14_db_data
variables <- c("DATE", "COUNTRY_NAME", "INDICATOR_NAME","VALUE")
gei_subset <- gei[, variables]
gei_2017 <- data.frame(dcast(gei_subset[which(gei_subset$DATE ==
                                                "2017"), ],
                             COUNTRY_NAME ~ INDICATOR_NAME), YEAR = "2017")
gei_2018 <- data.frame(dcast(gei_subset[which(gei_subset$DATE ==
                                                "2018"), ],
                             COUNTRY_NAME ~ INDICATOR_NAME), YEAR = "2018")
gei_2019 <- data.frame(dcast(gei_subset[which(gei_subset$DATE ==
                                                "2019"), ], 
                             COUNTRY_NAME ~ INDICATOR_NAME), YEAR = "2019")
gei_cleaned <- rbind(gei_2017, gei_2018, gei_2019)
colnames(gei_cleaned)[1] <- "COUNTRY"
head(db) # Check DB dataset
db <- db[-1] #delete the first unrelated column
db_cleaned <- subset(db, select = -c(7, 8)) # Remove columns of 2015, 2010-2014
# Rename country, year and DB score columns
names(db_cleaned)[2] <- "COUNTRY"
names(db_cleaned)[5] <- "YEAR"
names(db_cleaned)[6] <- "DB_score"
head(db_cleaned) ## Visualizing processed dataset
db_gei_data <- merge(db_cleaned, gei_cleaned, by = c('COUNTRY', 'YEAR'))#merging
```

### Task 2

```{r ,message=FALSE, warning=FALSE,results='hide',fig.show='hide'}
#2.1)
#computation of correlation
mydata.corr = round(cor(x=db_gei_data[,6], y=db_gei_data[,9]),5) 
print(c("Correlation between db_score and gei_score",mydata.corr))
#plot of DB  score against GEI score
ggplot(db_gei_data, aes(x = Global.Enterpreneurship.Index.Score, y = DB_score)) + 
  geom_point() + labs(title = "DB Score vs GEI Score - Cor: 0.76, R2: 0.58") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold")) +
  geom_smooth(method=lm, se=FALSE, col='red')
#linear regression GEI_score vs DB_score
lm1 <- lm(db_gei_data[,6]~db_gei_data[,9])
summary(lm1)
#transforming gei_score in log
log_geiscore <- log(db_gei_data[,9])
db_gei_data2 <- cbind(db_gei_data, log_geiscore)
#linear regression Log(GEI_score  vs DB_score)
lm2 <- lm(db_gei_data2[,6]~db_gei_data2[,22])
summary(lm2)
#plot of DB score agains Log(GEI_score)
ggplot(db_gei_data2, aes(x = log_geiscore, y = DB_score)) + 
  geom_point() + labs(title = "DB Score vs Log(GEI Score) - Cor: 0.82, R2: 0.67") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold")) +
  geom_smooth(method=lm, se=FALSE, col='red')
#correlation with and without the log
mydata.corr = cor(x=db_gei_data[,6], y=db_gei_data2[,c(9,22)]) 
print(c("Correlation between db_score and gei_score",mydata.corr))
#2.2)
# Correlation for db_score and all gei indicators to decide with to choose
ggpairs(db_gei_data[, c(7:8,10:14,6)])
ggpairs(db_gei_data[, c(15:21,6)])
# plot highest correlation in Attitudes sub_index
ggplot(db_gei_data, aes(x = Cultural.Support, y = DB_score)) + 
  geom_point() + labs(title = "DB Score vs Cultural Support - Cor: 0.64") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold")) +
  geom_smooth(se=FALSE, col='red')
# plot highest correlation in Abilities sub_index
ggplot(db_gei_data, aes(x = Oppurtunity.startup, y = DB_score)) + 
  geom_point() + labs(title = "DB Score vs Opportunity Startup - Cor: 0.68") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold")) +
  geom_smooth(se=FALSE, col='red')
# plot highest correlation in Aspiration sub_index
ggplot(db_gei_data, aes(x = Internationalization, y = DB_score)) + 
  geom_point() + labs(title = "DB Score vs Internationalization - Cor: 0.68") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold")) +
  geom_smooth(se=FALSE, col='red')
```

### Task 3

```{r,message=FALSE, warning=FALSE,results='hide',fig.show='hide'}
#3.1
data <-db_gei_data[,c(7:8,10:21)]
fit_pca <- princomp(data, cor = T, scores = TRUE) #computing PCs
summary(fit_pca)
round(fit_pca$loadings,digits=2)
screeplot(fit_pca, main="") #screeplot to decide on the number of components
cor(data,fit_pca$scores[,1:2]) 
#3.2 Permutation Test
perm_test_data = db_gei_data[,c(7:8,10:21)]
source("permtestPCA.R") # Load the permtestPCA() function
perm_range <- permtestPCA(perm_test_data)
#3.4 Biplot and Loadings
op <- par(mfrow = c(1, 1))
biplot(fit_pca, pc.biplot = TRUE, scale = 1, choices = 1:2, col = c("blue", "red"),
       asp = 1, cex = c(0.5, 1), main = "Biplot Components 1 and 2")
par(op)
print(summary(fit_pca, loadings = TRUE, cutoff = 0.2), digits = 2) #loadings: 
# Diagnostics: fit per variable
fit_pca$scores
cor_pca <- cor(data,fit_pca$scores)[,1:2] #checking for the first two components
temp <- cbind(cor_pca, rowSums(cor_pca^2))
colnames(temp) <- c("Dim1","Dim2", "Fit")
temp <- round(temp, digits=2)
temp
#Task 3.5
#Bootstrap code
# Goal: bootstrap the eigenvalues ->
# 1. Define function that runs PCA and returns 
my_boot_pca <- function(x, ind){
  res <- princomp(x[ind, ], cor = TRUE)
  return(res$sdev^2)
}
# 2. Run bootstrap
boot_data = db_gei_data[,c(7:8,10:21)]
fit_boot  <- boot(data = boot_data, statistic = my_boot_pca, R = 1000)
# 3. Store the bootstrapped statistic (here all eigenvalues)
eigs_boot <- fit_boot$t           # Store the R x p matrix with eigenvalues
head(eigs_boot)
fit_pca$sdev^2 #to see the original eigenvalues of our components 
par(mar = c(5, 4, 4, 1) + 0.1)
hist(eigs_boot[, 1], xlab = "Eigenvalue 1", las = 1, col = "blue", 
     main = "Bootstrap Confidence Interval", breaks = 20, 
     border = "white"); perc.alpha <- quantile(eigs_boot[, 1], c(0.025, 1 - 0.025));
abline(v = perc.alpha, col = "green", lwd = 2);
abline(v = fit_pca$sdev[1]^2, col = "red", lwd = 2)
#Variance explained ->
#A) for the first component
head(eigs_boot)
var_expl <- rowSums(eigs_boot[,1,drop=FALSE])/rowSums(eigs_boot)
var_expl #the variance explained by the first component for each bootstrap sample
hist(var_expl,xlab="Variance Explained",las=1, col="blue",
     main="Bootstrap Confidence Interval", breaks=20, border="white");
perc.alpha <- quantile(var_expl, c(0.025, 1 - 0.025) );
abline(v=perc.alpha, col="green", lwd=2);
abline(v=sum(fit_pca$sdev[1]^2)/sum(fit_pca$sdev^2),col= "red",lwd=2)
#B) of each variable explained by the first component
# 1. Define function that runs PCA and returns 
my_boot_pca2 <- function(x, ind){
  pca <- princomp(x[ind, ], cor = TRUE)
  corpca <- cor(x[ind, ],pca$scores)[,1]
  fit <- corpca^2
  return(fit)
}
# 2. Run bootstrap
boot_data2 = db_gei_data[,c(7:8,10:21)]
fit_boot2  <- boot(data = boot_data2, statistic = my_boot_pca2, R = 1000)
# 3. Store the bootstrapped statistic (here the sum of correlations^2)
fit_val <- fit_boot2$t           # Store the R x p matrix with eigenvalues
# 4. variance for Variable 1
head(fit_val)
par(mar = c(5, 4, 4, 1) + 0.1)
hist(fit_val[, 1], xlab = "Variance Explained by Competition", las = 1, col = "blue", 
     main = "Bootstrap Confidence Interval", breaks = 20, 
     border = "white");perc.alpha <- quantile(fit_val[, 1], c(0.025, 1 - 0.025));
abline(v = perc.alpha, col = "green", lwd = 2);
abline(v = temp[1,3], col = "red", lwd = 2)
# 5. Boxplot of variance explained of all variables
par(mar=c(3,4,3,3))
boxplot(fit_val, las=1, xlab="", ylab="Variance Explained", col="thistle", 
        main="Variance of Each Variable Explained by the First Component", 
        cex=1, cex.axis=0.4, cex.main=1, cex.lab=1)
```

### Task 4

```{r , fig.show='hide', message=FALSE,results='hide',warning=FALSE}
#Q4
#4.1 train + test
set.seed(100)
index <-  sort(sample(nrow(db_gei_data), nrow(db_gei_data)*.7))
train <- db_gei_data[index,]
test <-  db_gei_data[-index,]
#4.2 lm
model.lm <- lm(DB_score~Competition+Cultural.Support+
               High.Growth+Human.capital+Internationalization+Networking+
                 Oppurtunity.startup+Opputunity.perception+Process.Innovation+
                 Product.Innovation+Risk.Acceptance+Risk.Capital+Start.up.skills
               +Technology.Absorption,
               data=train
)
summary(model.lm)
#4.3 pcr
chooseCRANmirror(graphics = FALSE, ind = 10)
if (!require("pacman")) install.packages("pacman")
# This used the function p_load from the pacman
# package to load the other packages
pacman::p_load(pls)
model.pcr <- pcr(data = train, DB_score~Competition+Cultural.Support+
                   High.Growth+Human.capital+Internationalization+Networking+
                   Oppurtunity.startup+Opputunity.perception+Process.Innovation+
                   Product.Innovation+Risk.Acceptance+Risk.Capital+Start.up.skills
                 +Technology.Absorption,
                 validation = NULL,
                 scale = TRUE)
summary(model.pcr)
#4.4 predict test
model.pcr.pred <- predict(model.pcr, test, ncomp = 2)
pcr.RMSE<-sqrt(mean((test$DB_score-model.pcr.pred)^2))
model.lm.pred <- predict(model.lm, test)
lm.RMSE<-sqrt(mean((test$DB_score-model.lm.pred)^2))
Weights <- cbind(pcr.RMSE,lm.RMSE)
colnames(Weights) <- c("pcr.RMSE","lm.RMSE")
round(Weights, digits = 4)
r2.pcr<- 1-rse(test$DB_score,model.pcr.pred)
r2.lm <- 1-rse(test$DB_score,model.lm.pred)
adj.r2.pcr <- 1-(1-r2.pcr)*((89/74))
adj.r2.pcr
adj.r2.lm <- 1-(1-r2.lm)*((89/74))
adj.r2.lm 
#4.5
predictions <- read_csv("C:/Users/tamir/Downloads/predictions.csv")
pred.final <- predict(model.pcr, predictions, ncomp = 2)
```
