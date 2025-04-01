setwd("C:/Rstudio")
cytokine=read.csv("log10raw.csv",header=TRUE)
save(cytokine, file="cytokine.rda")
View(cytokine)
ctk <- cytokine[complete.cases(cytokine),]
ctk <-ctk[,-1]
for (i in 1:6){ctk[,i]<-as.numeric(as.character(ctk[,i]))}
class <- ctk[c(7)]
class2 <- ifelse(class=="HCC", 1, 0)
ctk$Class <- class2
ctk$Class <- factor(ctk$Class, levels = c(0, 1))
ctk

# logistic regression model
library(rms)
mod100 <- lrm(Class ~ MIP_1b+Perforin+TNF_a+IL_27+sCD137+sFas, data = ctk, x = TRUE, y = TRUE)
mod100$coef
pred1<-predict(mod1, newdata=ctk, type="fitted")
pred10
sFas <-pred1

y_pred_num9 <- ifelse(pred9 > 0.5, 1, 0)
y_pred9 <- factor (y_pred_num9, levels = c(0,1))
y_act9<- ctk$Class
mean (y_pred9== y_act9)

# ROC curve
par(mfrow = c(1,2))
test_prob307 = predict(m_rms7, newdata = ctk, type = "fitted")
test_roc307 = roc(ctk$Class ~ test_prob307, plot = TRUE, print.auc = TRUE)
test_roc30
sFas_MIP1b_Perforin <- pred7
ROC(form = ctk$Class ~sFas, plot="ROC")
ROC(form= ctk$Class ~ sFas_sCD137_IL27, plot = "ROC")

# multiple ROC curve
test_roc1 = roc(form = ctk$Class ~ pred1, plot = TRUE, col="blue")
test_roc5 = roc(ctk$Class ~ pred5, plot = TRUE, add = TRUE)
test_roc9 = roc(ctk$Class ~ pred9, plot = TRUE, add = TRUE, col="red")
test_roc9
test_roc27 = roc(ctk$Class ~ pred27, plot = TRUE, add = TRUE, col="blue")
test_roc30 = roc(ctk$Class ~ pred30, plot = TRUE, add = TRUE, col="red")

#Confussion matrix
t9 = table(predicted = y_pred9, actual = y_act9)
cm9 = confusionMatrix(t9, positive = "1"); cm9

# Calibration plot
pctk30 <- with(ctk, data.frame(y = ifelse(class=="HCC", 1, 0), prob30 = predict(mod30, type = "fitted")))

ggplot(pctk30, aes(prob30, Class)) +
  geom_point(shape = 21, size = 2) +
  geom_abline(slope = 1, intercept = 0) +
  geom_smooth(method = stats::loess, se = FALSE) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  xlab("Estimated Prob.") +
  ylab("Data w/ Empirical Prob.") +
  ggtitle("Logistic Regression Calibration Plot")

# logistic regression plot
library(rms)
mod100
plot(calibrate(mod, B = 5000))
IL27_MIP1b_Perforin_sFas_sCD137_TNFa <-pred100
fit100 <- glm(Class ~ IL_27+MIP_1b+Perforin+sFas+sCD137+TNF_a,  family=binomial, data=ctk)
plotting_dfm100<- expand.grid(IL_27 = seq(from=0, to = 10, by=0.1), MIP_1b = seq(from=0, to = 10, by=0.1), Perforin = seq(from=0, to = 10, by=0.1), sFas = seq(from=0, to = 10, by=0.1), sCD137 = seq(from=0, to = 10, by=0.1), TNF_a = seq(from=0, to = 10, by=0.1))
plotting_dfm100$preds <- plogis( predict(fit100 , newdata=plotting_dfm100))
library(ggplot2)
ggplot(plotting_dfm1, aes(x=sFas, y =preds))  +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial),lty=2) 

# Boostrapping - Internal validation
CalculateAucFromDxy <- function(validate) {
  stopifnot(class(validate) == "validate")
  aucs <- (validate["Dxy", c("index.orig","training","test","optimism","index.corrected")])/2 + 0.5
  n =5000
  res <- rbind(validate, AUC = c(aucs, n))
  res["AUC","optimism"] <- res["AUC","optimism"] - 0.5
  res
}
mod.valid <- validate(mod, method = "boot", B=5000)
CalculateAucFromDxy(mod.valid)

# Identify optimal cut-off point for the best performing model
pred9
ROCR_pred_test9 <- prediction(pred9,ctk$Class)
cost_perf9 = performance(ROCR_pred_test9, "cost") 
ROCR_pred_test9@cutoffs[[1]][which.min(cost_perf9@y.values[[1]])]
table(Actualvalue=ctk$Class,Predictedvalue=pred9>0.634)

# Nomograms for individual's cancer disgnosis
ddist <- datadist(ctk)
options(datadist='ddist')

plot((Nom9),xfrac=.12)
Predict(mod9, IL_27=350, sFas = 25000, sCD137=140,
        fun=plogis)

#Nom9 <- nomogram(mod9, fun =plogis, funlabel="Risk of Death")
#Nom9 <- nomogram(mod9, fun =plogis, 
                 fun.at=c(.00001,.0001,.001, seq(.1,.9,by=.0001),.95,.99,.999),
                 funlabel="Risk of Death")
Nom9<- nomogram(mod9,fun= plogis,
                fun.at=c(.01, 0.1,seq(.3,.9,by=.2), .99),
                funlabel="Risk of HCC")	




