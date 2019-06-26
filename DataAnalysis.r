# Script for analyzing the Analytical Atheist SDM output data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



library(dplyr)
library(ggpubr)



AASDM_data = read.csv("C:\\Users\\jelane\\Google Drive\\MODRN Stuff\\Models\\analyticSDM\\analyticSDM3 - Capped3\\analyticSDM3 - Capped3\\AnalyticAtheist_20190509_w_edu.csv")



#group_by(AASDM_data, education) %>%
  #summarise(
    #count = n(),
    #mean = mean(education, na.rm = TRUE),
    #sd = sd(education, na.rm = TRUE)
  #)

shapiro.test(AASDM_data$godBelief)
shapiro.test(AASDM_data$supernaturalBelief) # Normal if above .05





#Compute correlation 
# code largely drawn from http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
trimmed_data <- AASDM_data[, c(2:27)]
res <- cor(trimmed_data)
round(res, 2)

important_data <- AASDM_data[,c(16:27)]

library(xtable)
library(Hmisc)
icor<-round(cor(important_data),2)
upper<-icor
upper[upper.tri(icor)]<-""
upper<-as.data.frame(upper)
print(xtable(upper), type="html")
# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 



corstars(icor, result="html")

icorBIG<-round(cor(trimmed_data),2)
corstars(icorBIG, result="html")

res2 <- rcorr(as.matrix(important_data))
res2


library(corrplot)
#corrplot(res, type = "upper", order = "hclust", sig.level = 0.05, tl.col = "black", tl.srt = 45)

# Insignificant correlation are blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank",tl.col = "black", tl.srt = 45)





library(PerformanceAnalytics)
#my_data <- mtcars[, c(1,3,4,5,6,7)]
chart.Correlation(trimmed_data, histogram=TRUE, pch=19)



# now run some basic regressions
linReg_GB1 <- lm(godBelief ~ PRESENCE_OF_A + FIRST_TRAUMA +	FREQ_OF_TRAUMA +
                   INTELLIGENCE +	INIT_COG_INHIBITION +	COG_INHIB_DEPLETION +
                   INIT_RELIGIOUS_INFO +	FAMILY_RELIGIOUS_IMPORTANCE +	INIT_FACT_INFO +
                   EVENT_INTENSITY +	START_UG_AGE +
                   START_PG_AGE +	END_UG_AGE +	STUDYTOPIC +	TRADITION_SELF_CONT +
                   INTUITIVE_THINKING_STYLE +	COGNITIVE_REFLECTION +	NEED_FOR_COGNITION +
                   ENV_RELIGIOUS_IMPORTANCE +	fact_resistance +	cognitive_inhibition +
                   factual_information +	ontological_confusion +	analytical_thinking_style + supernaturalBelief, data=trimmed_data)
summary(linReg_GB1)

linReg_GB1_1 <- lm(godBelief ~ PRESENCE_OF_A + FIRST_TRAUMA +	FREQ_OF_TRAUMA +
                   INTELLIGENCE +	INIT_COG_INHIBITION +	COG_INHIB_DEPLETION +
                   INIT_RELIGIOUS_INFO +	FAMILY_RELIGIOUS_IMPORTANCE +	INIT_FACT_INFO +
                   EVENT_INTENSITY +	START_UG_AGE +
                   START_PG_AGE +	END_UG_AGE +	STUDYTOPIC +	TRADITION_SELF_CONT +
                   INTUITIVE_THINKING_STYLE +	COGNITIVE_REFLECTION +	NEED_FOR_COGNITION +
                   ENV_RELIGIOUS_IMPORTANCE +	fact_resistance +	cognitive_inhibition +
                   factual_information +	ontological_confusion +	supernaturalBelief, data=trimmed_data)
summary(linReg_GB1_1)

linReg_GB1_t <- lm(godBelief ~ INTELLIGENCE +	INIT_COG_INHIBITION +	COG_INHIB_DEPLETION +
                     INIT_RELIGIOUS_INFO +	FAMILY_RELIGIOUS_IMPORTANCE +	INIT_FACT_INFO +
                     TRADITION_SELF_CONT + INTUITIVE_THINKING_STYLE +	COGNITIVE_REFLECTION +	NEED_FOR_COGNITION +
                     ENV_RELIGIOUS_IMPORTANCE +	fact_resistance +	cognitive_inhibition +
                     ontological_confusion, data=trimmed_data)
summary(linReg_GB1_t)

linReg_GB1_st <- lm(godBelief ~ INTELLIGENCE +	TRADITION_SELF_CONT + INTUITIVE_THINKING_STYLE +	COGNITIVE_REFLECTION +	NEED_FOR_COGNITION +
                     fact_resistance +	cognitive_inhibition +
                     ontological_confusion, data=trimmed_data)
summary(linReg_GB1_st)
linReg_SB1_st <- lm(supernaturalBelief ~ INTELLIGENCE +	TRADITION_SELF_CONT + INTUITIVE_THINKING_STYLE +	COGNITIVE_REFLECTION +	NEED_FOR_COGNITION +
                      fact_resistance +	cognitive_inhibition +
                      ontological_confusion, data=trimmed_data)
summary(linReg_SB1_st)
linReg_SB1_st_sc <- lm(scale(supernaturalBelief) ~ scale(INTELLIGENCE) + 
                        scale(TRADITION_SELF_CONT) + 
                        scale(INTUITIVE_THINKING_STYLE) +	
                        scale(COGNITIVE_REFLECTION) +	
                        scale(NEED_FOR_COGNITION) +
                        scale(fact_resistance) +	
                        scale(cognitive_inhibition) +
                        scale(ontological_confusion), data=trimmed_data)
summary(linReg_SB1_st_sc)
linReg_GB1_st_sc <- lm(scale(godBelief) ~ scale(INTELLIGENCE) + 
                         scale(TRADITION_SELF_CONT) + 
                         scale(INTUITIVE_THINKING_STYLE) +	
                         scale(COGNITIVE_REFLECTION) +	
                         scale(NEED_FOR_COGNITION) +
                         scale(fact_resistance) +	
                         scale(cognitive_inhibition) +
                         scale(ontological_confusion), data=trimmed_data)
summary(linReg_GB1_st_sc)
linReg_cog_ont <- lm(cognitive_inhibition ~ ontological_confusion, data=trimmed_data)
summary(linReg_cog_ont)

#get standardized betas
library("QuantPsyc")
betas <- lm.beta(linReg_GB1_st)





# check for mediation
library(lavaan)

modelm <- '
# direct effect
godBelief ~ c*cognitive_inhibition

# mediators
ontological_confusion ~ a*cognitive_inhibition
godBelief ~ b*ontological_confusion

# indirect effects
ab := a*b

# total effect
total := c+(a*b)
'
fit1 <- sem(modelm, se = "bootstrap", estimator = "GLS", data= trimmed_data)
summary(fit1)
# now visualize the data
library("semPlot")
semPaths(fit1,"std",edge.label.cex=0.5, curvePivot = TRUE)

linReg_GB2 <- lm(godBelief ~ PRESENCE_OF_A + FIRST_TRAUMA +	FREQ_OF_TRAUMA +
                   INTELLIGENCE +	INIT_COG_INHIBITION +	COG_INHIB_DEPLETION +
                   INIT_RELIGIOUS_INFO +	FAMILY_RELIGIOUS_IMPORTANCE +	INIT_FACT_INFO +
                   EVENT_INTENSITY +	START_UG_AGE +
                   START_PG_AGE +	END_UG_AGE +	STUDYTOPIC +	TRADITION_SELF_CONT +
                   INTUITIVE_THINKING_STYLE +	COGNITIVE_REFLECTION +	NEED_FOR_COGNITION +
                   ENV_RELIGIOUS_IMPORTANCE +	fact_resistance +	cognitive_inhibition +
                   factual_information +	ontological_confusion +	analytical_thinking_style, data=trimmed_data)
summary(linReg_GB2)


linReg_SB1 <- lm(supernaturalBelief ~ PRESENCE_OF_A + FIRST_TRAUMA +	FREQ_OF_TRAUMA +
                   INTELLIGENCE +	INIT_COG_INHIBITION +	COG_INHIB_DEPLETION +
                   INIT_RELIGIOUS_INFO +	FAMILY_RELIGIOUS_IMPORTANCE +	INIT_FACT_INFO +
                   EVENT_INTENSITY +	START_UG_AGE +
                   START_PG_AGE +	END_UG_AGE +	STUDYTOPIC +	TRADITION_SELF_CONT +
                   INTUITIVE_THINKING_STYLE +	COGNITIVE_REFLECTION +	NEED_FOR_COGNITION +
                   ENV_RELIGIOUS_IMPORTANCE +	fact_resistance +	cognitive_inhibition +
                   factual_information +	ontological_confusion +	analytical_thinking_style + godBelief, data=trimmed_data)
summary(linReg_SB1)

linReg_SB2 <- lm(supernaturalBelief ~ PRESENCE_OF_A + FIRST_TRAUMA +	FREQ_OF_TRAUMA +
                   INTELLIGENCE +	INIT_COG_INHIBITION +	COG_INHIB_DEPLETION +
                   INIT_RELIGIOUS_INFO +	FAMILY_RELIGIOUS_IMPORTANCE +	INIT_FACT_INFO +
                   EVENT_INTENSITY +	START_UG_AGE +
                   START_PG_AGE +	END_UG_AGE +	STUDYTOPIC +	TRADITION_SELF_CONT +
                   INTUITIVE_THINKING_STYLE +	COGNITIVE_REFLECTION +	NEED_FOR_COGNITION +
                   ENV_RELIGIOUS_IMPORTANCE +	fact_resistance +	cognitive_inhibition +
                   factual_information +	ontological_confusion +	analytical_thinking_style, data=trimmed_data)
summary(linReg_SB2)



trimmed_data_gbelievers <- trimmed_data
trimmed_data_gbelievers$godCat <- factor(ifelse(trimmed_data$godBelief > 0, "believer", "nonbeliever"))



# Compute the analysis of variance
res.aov <- aov(INTELLIGENCE ~ godCat, data = trimmed_data_gbelievers)
# Summary of the analysis
summary(res.aov)

kruskal.test(INTELLIGENCE ~ godCat, data = trimmed_data_gbelievers)





trimmed_data_gbelievers <- trimmed_data
trimmed_data_gbelievers$godCat <- factor(ifelse(trimmed_data$godBelief > 0, "believer", "nonbeliever"))



train <- trimmed_data_gbelievers[1:15000,]
test <- trimmed_data_gbelievers[15001:20000,]
model <- glm(godCat ~.,family=quasibinomial,data=train)
summary(model)






# Now use xgboost to do some data discovery
# https://xgboost.readthedocs.io/en/latest/R-package/discoverYourData.html


library(xgboost)
library(Matrix)
library(data.table)
trimmed_data2 <- trimmed_data[, c(1:25)]
df <- data.table(trimmed_data2)
head(df[,godCat:= as.factor(ifelse(godBelief > 0, "believer", "nonbeliever"))])

train_xg <- df[1:15000,]
test_xg <- df[15001:20000,]


#trimmed_data2 <- trimmed_data[, c(1:25)]
df <- data.table(trimmed_data)
# now without cognitive inhibition
# and ontological and god belief
trimmed_data2 <- trimmed_data[, c(1:20,22,24,26)]
trimmed_data2 <- trimmed_data[, c(1:25)]
df<- data.table(trimmed_data2)
#head(df[,godCat:= as.factor(ifelse(godBelief > 0, "believer", "nonbeliever"))])
#DF3<-df
train_xg <- df[1:15000,]
test_xg <- df[15001:20000,]
dftrain<-train_xg
dftest<-test_xg
dftest2<-dftest
output_vector_train<-dftrain$godBelief
output_vector_test<-dftest$godBelief
TrainData <- sparse.model.matrix(godBelief~.-1,data = dftrain)
TestData <- sparse.model.matrix(godBelief~.-1,data = dftest)
dtrain <- xgb.DMatrix(data = TrainData, label=output_vector_train)
dtest <- xgb.DMatrix(data = TestData, label=output_vector_test)
watchlist <- list(train=dtrain, test=dtest)
Model2 <- xgb.train(data=dtrain, max.depth=10, eta=1, nthread = 2, nrounds=8, watchlist=watchlist, objective = "reg:linear")
pred <- predict(Model2, TestData)
plot(pred,output_vector_test)
cor.test(pred,output_vector_test)$estimate^2
importance <- xgb.importance(feature_names = TrainData@Dimnames[[2]], model = Model2)
#write.csv(as.data.frame(importance[,1:2]),file = "Table_Pred_sick_leave_Health.issue_size.establishment_year.csv",row.names = F)
xgb.plot.importance(importance_matrix = importance)







#trimmed_data2 <- trimmed_data[, c(1:25)]
df <- data.table(trimmed_data)
# now without cognitive inhibition
# and ontological and god belief
trimmed_data2 <- trimmed_data[, c(1:20,22,24,26)]
trimmed_data2 <- trimmed_data[, c(1:24,26)]
df<- data.table(trimmed_data2)
#head(df[,superCat:= as.factor(ifelse(supernaturalBelief > 0, "believer", "nonbeliever"))])
DF3<-df
train_xg <- df[1:15000,]
test_xg <- df[15001:20000,]
dftrain<-train_xg
dftest<-test_xg
dftest2<-dftest
output_vector_train<-dftrain$supernaturalBelief
output_vector_test<-dftest$supernaturalBelief
TrainData <- sparse.model.matrix(supernaturalBelief~.-1,data = dftrain)
TestData <- sparse.model.matrix(supernaturalBelief~.-1,data = dftest)
dtrain <- xgb.DMatrix(data = TrainData, label=output_vector_train)
dtest <- xgb.DMatrix(data = TestData, label=output_vector_test)
watchlist <- list(train=dtrain, test=dtest)
Model2 <- xgb.train(data=dtrain, max.depth=10, eta=1, nthread = 2, nrounds=8, watchlist=watchlist, objective = "reg:linear")
pred <- predict(Model2, TestData)
plot(pred,output_vector_test)
cor.test(pred,output_vector_test)$estimate^2
importance <- xgb.importance(feature_names = TrainData@Dimnames[[2]], model = Model2)
#write.csv(as.data.frame(importance[,1:2]),file = "Table_Pred_sick_leave_Health.issue_size.establishment_year.csv",row.names = F)
xgb.plot.importance(importance_matrix = importance)
head(importance)





trimmed_data3 <- trimmed_data[, c(1:25)]
df<- data.table(trimmed_data3)
head(df[,godCat:= as.factor(ifelse(godBelief > 0, "believer", "nonbeliever"))])
DF3<-df
train_xg <- df[1:15000,]
test_xg <- df[15001:20000,]
#sparse_matrix <- sparse.model.matrix(godBelief ~ ., data = df)[,-1]
dftrain<-train_xg
dftest<-test_xg
dftest2<-dftest
#output_vector = df[,godBelief] == "Marked"
sparse_matrix <- sparse.model.matrix(godCat~.-1, data = df)
output_vector = df[,godCat] == "Marked"
bst <- xgboost(data = sparse_matrix, label = output_vector, max.depth = 4,eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")
importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
head(importance)

# now the same analysis without cognitive inhibition:
trimmed_data3 <- trimmed_data[, c(1:20,22:25)]
df<- data.table(trimmed_data3)
head(df[,godCat:= as.factor(ifelse(godBelief > 0, "believer", "nonbeliever"))])
DF3<-df
train_xg <- df[1:15000,]
test_xg <- df[15001:20000,]
#sparse_matrix <- sparse.model.matrix(godBelief ~ ., data = df)[,-1]
dftrain<-train_xg
dftest<-test_xg
dftest2<-dftest
#output_vector = df[,godBelief] == "Marked"
sparse_matrix <- sparse.model.matrix(godCat~.-1, data = df)
output_vector = df[,godCat] == "Marked"
bst <- xgboost(data = sparse_matrix, label = output_vector, max.depth = 4,eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")
importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
head(importance)





# For whatever reason this throws an error
trimmed_data4 <- trimmed_data[, c(1:24,26)]
df<- data.table(trimmed_data4)
head(df[,superCat:= as.factor(ifelse(supernaturalBelief > 0, "believer", "nonbeliever"))])
DF3<-df
train_xg <- df[1:15000,]
test_xg <- df[15001:20000,]
#sparse_matrix <- sparse.model.matrix(godBelief ~ ., data = df)[,-1]
dftrain<-train_xg
dftest<-test_xg
dftest2<-dftest
#output_vector = df[,godBelief] == "Marked"
sparse_matrix <- sparse.model.matrix(superCat~.-1, data = df)
output_vector = df[,superCat] == "Marked"
bst <- xgboost(data = sparse_matrix, label = output_vector, max.depth = 4,eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")
importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
head(importance)
