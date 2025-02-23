# NC2024
R code
library(XLConnect)
library(ggplot2)
library(survival)
library(survminer)
library(pROC)
library(ggsci)
library(OptimalCutpoints)
# Survival analysis
book <- loadWorkbook("survivalNAC.xlsx")
breast <- readWorksheet(book, sheet=10) 
str(breast)
fit <- survfit(Surv(DFS,Relapse) ~ ctDNA.burden,  # 创建生存对象 
               data = breast) 
fit <- survfit(Surv(DRFS,Metastasis) ~ ctDNA.burden,  # 创建生存对象 
               data = breast) 
fit2 <- survfit(Surv(DFS,Relapse) ~ pcr,  # 创建生存对象 
                data = breast) 
fit3 <- survfit(Surv(DFS,Relapse) ~ combined, 
                data = breast)
ggsurvplot(fit, data = breast,pval = TRUE, risk.table = TRUE, palette = "lancet",legend="bottom")         

# ROC analysis
book2 <- loadWorkbook("ROC.xlsx")
d <- readWorksheet(book2,sheet = 1)
roc_1st <- roc(d$Relapse, d$VAF, plot = TRUE,print.thres=TRUE)
auc(roc_1st)
ci.auc(roc_1st)
g_roc <-ggroc(roc_1st, color= "red")+geom_abline(aes(x=1, y=1), intercept = 1, alpha=0.5, lty=2)+theme_classic()

# OptimalCutpoints
cutpoint <- optimal.cutpoints(X = "VAF", status = "Relapse", tag.healthy = 0, methods = "MaxDOR", data = d, 
                              pop.prev = NULL, control = control.cutpoints(), ci.fit = TRUE)
cutpoint <- optimal.cutpoints(X = "VAF", status = "Relapse", tag.healthy = 0, methods = "MaxProdSpSe", data = d, 
                              pop.prev = NULL, control = control.cutpoints(CFN=3), ci.fit = TRUE)
cutpoint <- optimal.cutpoints(X = "VAF", status = "Relapse", tag.healthy = 0, methods = "Youden", data = d, 
                              pop.prev = NULL, control = control.cutpoints(CFN=3), ci.fit = TRUE)
cutpoint <- optimal.cutpoints(X = "VAF", status = "Relapse", tag.healthy = 0, methods = "MinPvalue", data = d, 
                              pop.prev = NULL, control = control.cutpoints(CFN=3), ci.fit = TRUE)      
cutpoint <- optimal.cutpoints(X = "VAF", status = "Relapse", tag.healthy = 0, methods = "MinPvalue", data = d, 
                              pop.prev = NULL, control = control.cutpoints(CFN=3), ci.fit = TRUE)    
summary(cutpoint)

