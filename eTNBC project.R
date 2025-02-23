
library(XLConnect)
library(ggplot2)
library(survival)
library(survminer)
library(pROC)
library(ggsci)

#survival analysis
book <- loadWorkbook("survivalNAC.xlsx")
breast <- readWorksheet(book, sheet=10)
# survival project

fit <- survfit(Surv(DFS,Relapse) ~ ctDNA.burden,  
               data = breast) 
fit <- survfit(Surv(DRFS,Metastasis) ~ ctDNA.burden,  # 创建生存对象 
               data = breast) 
fit3 <- survfit(Surv(DFS,Relapse) ~ combined, 
                data = breast)# 创建生存对象 
fit3 <- survfit(Surv(DRFS,Metastasis) ~ combined, 
                data = breast)# 创建生存对象 

ggsurvplot(fit, data = breast,pval = TRUE, risk.table = TRUE, palette = "lancet",legend="bottom")
ggsurvplot(fit3, data = breast,pval = TRUE, risk.table = TRUE, legend="bottom", palette = c("#42B540","#00468B","#0099B4","#ED0000"))

#ROC analysis
#---新辅助阈值的ROC曲线
library(pROC)
book2 <- loadWorkbook("ROC.xlsx")
d <- readWorksheet(book2,sheet = 1)
roc_1st <- roc(d$Relapse, d$VAF, plot = TRUE,print.thres=TRUE)
auc(roc_1st)
ci.auc(roc_1st)
g_roc <-ggroc(roc_1st, color= "red")+geom_abline(aes(x=1, y=1), intercept = 1, alpha=0.5, lty=2)+theme_classic()

#OptimalCutpoints package
library(OptimalCutpoints)
cutpoint <- optimal.cutpoints(X = "VAF", status = "Relapse", tag.healthy = 0, methods = "MaxDOR", data = d, 
                              pop.prev = NULL, control = control.cutpoints(), ci.fit = TRUE)
cutpoint <- optimal.cutpoints(X = "VAF", status = "Relapse", tag.healthy = 0, methods = "MaxProdSpSe", data = d, 
                              pop.prev = NULL, control = control.cutpoints(CFN=3), ci.fit = TRUE)
cutpoint <- optimal.cutpoints(X = "VAF", status = "Relapse", tag.healthy = 0, methods = "MinPvalue", data = d, 
                              pop.prev = NULL, control = control.cutpoints(CFN=3), ci.fit = TRUE)
summary(cutpoint)

