# First Model
#@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@

# Libs

library(conflicted)
library(readxl)

library(tidyverse)
library(dplyr)
library(broom)
library(tidyr)
library(psych)

library(caret) #Models
library(glmnet) #Shirinkage methods
library(olsrr)

#Tests
library(gvlma)#Check assumptions

#lm_robust
library(estimatr)
library(sandwich)
library(robustbase)


library(ggplot2)
library(ggfortify)

library(ggcorrplot)
library(jtools)

library(lmtest)
library(car)
library(e1071)

#############
#Load file
#############
#File
conflict_prefer("filter", "dplyr")

local_path <- "C:\\Users\\Rafael\\Desktop\\DADOS_MONO_COMPILE"
file <- "\\final_file_v2.xlsx"

path <- paste(local_path,file,sep="")
df <- read_excel(path,sheet='Data')

# Wrangle
#Dummies
df$Adj <- factor(df$Adj)
df$MerSul <- factor(df$MerSul)
df$L <- factor(df$L)

TradeCox <- caret::BoxCoxTrans(df$Trade)
df <- cbind(df, Trade_new=predict(TradeCox, df$Trade))
# Filter
df_2 <- df %>% select(Trade,RelDis,L,Adj,MerSul,
                      Exp_SizeCou,Exp_SizeWat,Exp_Pop,
                      Exp_Gdp,Exp_GdpPer,
                      
                      Imp_SizeCou,Imp_SizeWat,Imp_Pop,
                      Imp_Gdp,Imp_GdpPer)



# Transform
df_2 <- df_2 %>% mutate(Trade = log(Trade),
                        RelDis = log(RelDis),
                        Exp_SizeCou = log(Exp_SizeCou),
                        Exp_SizeWat = log(Exp_SizeWat),
                        Exp_Pop = log(Exp_Pop),
                        Exp_Gdp = log(Exp_Gdp),
                        Exp_GdpPer = log(Exp_GdpPer),
                        
                        Imp_SizeCou = log(Imp_SizeCou),
                        Imp_SizeWat = log(Imp_SizeWat),
                        Imp_Pop = log(Imp_Pop),
                        Imp_Gdp = log(Imp_Gdp),
                        Imp_GdpPer = log(Imp_GdpPer),
)


# Split data
set.seed(999)
index <- createDataPartition(y = df_2$Trade, p =0.6, list=FALSE)

train <- df_2[index,]
test <- df_2[-index,]

###############
# Linear Model
################
lm_model <- lm(Trade ~ RelDis  + L + Adj + MerSul +
                 +Exp_SizeCou+ Exp_SizeWat+ 
                 +Exp_Pop + +Exp_Gdp + Exp_GdpPer + 
                 +Imp_SizeCou + Imp_SizeWat
               +Imp_Pop + Imp_Gdp + Imp_GdpPer
               , data=train)


#Residual Analysis
###
gvlma(lm_model)

#Distribution 
# Normal Distribution
qqnorm(lm_model$residuals)
shapiro.test(residuals(lm_model))
ols_test_normality(lm_model)

#Histogram

ggplot(data=train, aes(lm_model$residuals)) +
  geom_histogram(binwidth = 0.35, color = "black", fill = "Blue",alpha=0.58) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histograma dos residuos do modelo")


#QQplot
qqnorm(lm_model$residuals)
#Distribution
df_plot_den <- augment(lm_model)

ggplot(df_plot_den, aes(x = .fitted, y = .resid)) + 
  geom_hline(yintercept = 0)+
  geom_point(color='Blue',alpha=0.58)+xlab("x") +
  ylab('Residuo')

ggplot(train, aes(x=lm_model$residuals)) + 
  geom_density(color='Black',fill='Blue',alpha=0.58)+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line())

#Heterocedastic {Breush-Pagan Test} ->Correct with box-cox transformation
lmtest::bptest(lm_model) #When p-value < 0.05 thats significats and reject hypothesis the variance is constant
car::ncvTest(lm_model)

#Autocorrelation
dwtest(lm_model)

#Summary + confint
###

###
summary_model <- summary(lm_model)
summary_model
modelCoeffs <- summary_model$coefficients




#Metrics : {AIC / BIC / SIGMA / STD ERROR/ R2 / R2adj /  RMSE }
###
aic <- AIC(lm_model)
bic <-BIC(lm_model)
sigm <-sigma(lm_model)
std.error <- modelCoeffs["Trade", "Std. Error"]
r2 <-summary_model$r.squared
r2ajus <-summary_model$adj.r.squared

print(paste('AIC : ',aic))
print(paste('BIC : ',bic))
print(paste('SIGMA : ',sigm))

print(paste('R2 : ',r2))
print(paste('R2 ajus. : ',r2ajus))
print(paste('RMSE : ',))
summary_model$fstatistic

# Forecast
####
#Accuracy
tradePred <- predict(lm_model, test)
actuals_preds <- data.frame(cbind(actuals=test$Trade, predicteds=tradePred)) 
correlation_accuracy <- cor(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  

#Error and R2
test$TradePredict <- predict(lm_model, test)
#Metrics
actual <- test$Trade
preds <- test$TradePredict
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss

mse <- mean((preds - actual) ^ 2)
mst <- mean((actual - mean(actual)) ^ 2)
rsq2adj <- 1 - mse/mst

RMSE = sqrt(rss/nrow(test))


print(paste("R2 : ",rsq ))
print(paste("R2 ajus. : ",rsq2adj ))
print(paste("RMSE : ",RMSE ))
print(paste("Min-Max Acuracia : ",min_max_accuracy ))
#MeanAbsolutePercentageError (MAPE)
print(paste("Porcentagem media de erros : ",mape ))


#Diference
test$DiffTrade <- test$TradePredict - test$Trade
print(test[ , c("Trade", "TradePredict","DiffTrade")])


export_df <- test %>% filter(Exp = 'Brazil')
ggplot() + 
  geom_point(data = test, aes(x = Year, y = DiffTrade), color = "red") +
  geom_line(data = test, aes(x = Year, y = DiffTrade), color = "gray") +
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 1.5,color = 'Black',linetype = "dashed")+
  geom_hline(yintercept = -1.5,color='Black',linetype = "dashed")+
  expand_limits( y=c(-5, 5))+
  #stat_smooth(data = export_df, aes(x = Year, y = DiffTrade), color = "gray") +
  #geom_point(data = export_df, aes(x = Year, y = Trade), color = "blue") +
  
  xlab('Ano') +
  ylab('Fluxo comercial')+
  ggtitle("Diferença entre Y e pred(Y)")


####################
# Shrinkage regression
####################

# Split XY {TRAIN AND TEST}
Y <- log(train$Trade)
X <- train %>% select(-Trade) %>% data.matrix()

Y_TESTE <- log(test$Trade)
X_TESTE <- test %>% select(-Trade) %>% data.matrix()

#Lambdas
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

#Ridge alpha = 0 || Lasso alpha = 1
#########

# Without optization lambda
ridge <- glmnet(X,Y,alpha = 0)
plot(ridge)

# Optimization Lambda
ridge_cv <- cv.glmnet(X, Y, alpha = 0, lambda = lambdas_to_try)

#Using opt lambda
opt_lambda <- ridge_cv$lambda.min

plot(ridge_cv)
abline(v = log(ridge_cv$lambda.min), col = 2)

#Create optimization model
fit <- ridge_cv$glmnet.fit
summary(fit)
min(ridge_cv$cvm)


# Top variables
coef(ridge_cv, s = "lambda.1se") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  top_n(25, wt = abs(value)) %>%
  ggplot(aes(value, reorder(row, value))) +
  geom_point() +
  ggtitle("Variaveis mais influentes") +
  xlab("Coefficient") +
  ylab(NULL)

# Forecast

y_predicted <- predict(fit, s = opt_lambda, newx = X_TESTE)
# Sum of Squares Total and Error
sst <- sum((Y_TESTE - mean(Y_TESTE))^2)
sse <- sum((y_predicted - Y_TESTE)^2)
rsq <- 1 - sse / sst
rsq
RMSE = sqrt(sse/nrow(test))


mse <- sse/( nrow(test) - 17)
mst <- sst/(nrow(test)-1)
rsq2adj <- 1 - mse / mst

#Another
actuals_preds <- data.frame(cbind(actuals=Y_TESTE, y_predicted)) 
correlation_accuracy <- cor(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  

mapa <- 0.000759851
#

print(paste("R2 : ",rsq))
print(paste("R2 ajus. : ",rsq2adj))
print(paste("RMSE : ",RMSE))
print(paste("Min-Max Acuracia : ",min_max_accuracy ))
#MeanAbsolutePercentageError (MAPE)
print(paste("Porcentagem media de erros : ",mapa ))

######## CHART

DiffTrade <- y_predicted - Y_TESTE
#print(test[ , c("Trade", "TradePredict","DiffTrade")])


#export_df <- test %>% filter(Exp = 'Brazil')
ggplot() + 
  geom_point(data = test, aes(x = Year, y = DiffTrade), color = "red") +
  geom_line(data = test, aes(x = Year, y = DiffTrade), color = "gray") +
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 1.5,color = 'Black',linetype = "dashed")+
  geom_hline(yintercept = -1.5,color='Black',linetype = "dashed")+
  expand_limits( y=c(-5, 5))+
  #stat_smooth(data = export_df, aes(x = Year, y = DiffTrade), color = "gray") +
  #geom_point(data = export_df, aes(x = Year, y = Trade), color = "blue") +
  
  xlab('Ano') +
  ylab('Fluxo comercial')+
  ggtitle("Diferença entre Y e pred(Y)")



##########
# Lasso
##########

# Split XY {TRAIN AND TEST}
Y <- log(train$Trade)
X <- train %>% select(-Trade) %>% data.matrix()

Y_TESTE <- log(test$Trade)
X_TESTE <- test %>% select(-Trade) %>% data.matrix()

#Lambdas
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

#Lasso alpha = 0 || Lasso alpha = 1
#########

# Without optization lambda
Lasso <- glmnet(X,Y,alpha = 1)
plot(Lasso)

# Optimization Lambda
Lasso_cv <- cv.glmnet(X, Y, alpha = 1, lambda = lambdas_to_try)
plot(Lasso_cv)

#Using opt lambda
opt_lambda <- Lasso_cv$lambda.min


#Create optimization model
fit <- Lasso_cv$glmnet.fit
summary(fit)
min(Lasso_cv$cvm)

# Top variables
coef(Lasso_cv, s = "lambda.1se") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  top_n(25, wt = abs(value)) %>%
  ggplot(aes(value, reorder(row, value))) +
  geom_point() +
  ggtitle("Variaveis mais influentes") +
  xlab("Coefficient") +
  ylab(NULL)

# Forecast

y_predicted <- predict(fit, s = opt_lambda, newx = X_TESTE)
# Sum of Squares Total and Error
sst <- sum((Y_TESTE - mean(Y_TESTE))^2)
sse <- sum((y_predicted - Y_TESTE)^2)
rsq <- 1 - sse / sst
rsq
RMSE = sqrt(sse/nrow(test))


mse <- sse/( nrow(test) - 17)
mst <- sst/(nrow(test)-1)
rsq2adj <- 1 - mse / mst

#Another
actuals_preds <- data.frame(cbind(actuals=Y_TESTE, y_predicted)) 
correlation_accuracy <- cor(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  

mapa <- 0.000759851
#

print(paste("R2 : ",rsq))
print(paste("R2 ajus. : ",rsq2adj))
print(paste("RMSE : ",RMSE))
print(paste("Min-Max Acuracia : ",min_max_accuracy ))
#MeanAbsolutePercentageError (MAPE)
print(paste("Porcentagem media de erros : ",mapa ))
