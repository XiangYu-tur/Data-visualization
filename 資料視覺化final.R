#資料處理----
setwd("C:/R/資料視覺化")
dat <- read.csv("final.csv",header = T)
final <- subset(dat,select = c("a1","a5","a15a","b1a","b1b","b1c","b1d","b1e",
                               "b1f","b1g","b1h","b1i","b1j","b1k"))
library(car)
table(final$a1,useNA = "ifany")
final$a1 <- factor(final$a1,labels = c("男","女"))
ggplot(final, aes(x=a1,fill = a1)) +
  geom_bar() + stat_count() + 
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.2, color=I("red"), size = 5) +
  xlab("") + ylab("人數") + ggtitle("性別") +
  png("/R/資料視覺化/final/性別.png",width = 1920, height = 1080, res = 200)
dev.off()

table(final$a5,useNA = "ifany")
final$a5 <- recode(final$a5, "4:5 = 4")
final$a5 <- factor(final$a5,labels = c("大都市","大都市旁的郊區","小都市或小鎮",
                                       "農村地區",NA))
ggplot(final, aes(x=a5,fill = a5)) +
  geom_bar() + stat_count() + 
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.2, color=I("red"), size = 5) +
  xlab("") + ylab("人數") + ggtitle("居住地區") +
  png("/R/資料視覺化/final/居住地區.png",width = 1920, height = 1080, res = 200)
dev.off()


table(final$a15a)
final$a15a <- recode(final$a15a, "1:9 = 1")
final$a15a <- recode(final$a15a, "10:12 = 2")
final$a15a <- recode(final$a15a, "13:27 = 3")
final$a15a <- recode(final$a15a, "96:98 = 4")
final$a15a <- factor(final$a15a,labels = c("國中以下","高中以下","大學(含)以上",NA))
table(final$a15a)
ggplot(final, aes(x=a15a,fill = a15a)) +
  geom_bar() + stat_count() + 
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.2, color=I("red"), size = 5) +
  xlab("") + ylab("人數") + ggtitle("教育程度") +
  png("/R/資料視覺化/final/教育程度.png",width = 1920, height = 1080, res = 200)
dev.off()


for (i in 4:14){
  final[,i] = recode(final[,i], "1=5;2=4;3=3;4=2;5=1")
}
library(ggplot2)
final$b1a <- recode(final$b1a, "93:98 = NA")
table(final$b1a,useNA = "ifany")
ggplot(final, aes(x=b1a,fill = b1a)) +
  geom_bar() + stat_count() + 
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.2, color=I("red"), size = 5) +
  xlab("") + ylab("人數") + ggtitle("出身自有錢人家庭重不重要？") +
  png("/R/資料視覺化/final/出身自有錢人家庭重不重要？.png",width = 1920, height = 1080, res = 200)
dev.off()

final$b1b <- recode(final$b1b, "93:98 = NA")
table(final$b1b,useNA = "ifany")
ggplot(final, aes(x=b1b,fill = b1b)) +
  geom_bar() + stat_count() + 
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.2, color=I("red"), size = 5) +
  xlab("") + ylab("人數") + ggtitle("父母教育程度高重不重要？") +
  png("/R/資料視覺化/final/父母教育程度高重不重要？.png",width = 1920, height = 1080, res = 200)
dev.off()

final$b1c <- recode(final$b1c, "93:98 = NA")
table(final$b1c,useNA = "ifany")
ggplot(final, aes(x=b1c,fill = b1c)) +
  geom_bar() + stat_count() + 
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.2, color=I("red"), size = 5) +
  xlab("") + ylab("人數") + ggtitle("自己的教育程度重不重要？") +
  png("/R/資料視覺化/final/自己的教育程度重不重要？.png",width = 1920, height = 1080, res = 200)
dev.off()

final$b1d <- recode(final$b1d, "93:98 = NA")
table(final$b1d,useNA = "ifany")
ggplot(final, aes(x=b1d,fill = b1d)) +
  geom_bar() + stat_count() + 
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.2, color=I("red"), size = 5) +
  xlab("") + ylab("人數") + ggtitle("有很想要成功的企圖心重不重要？") +
  png("/R/資料視覺化/final/有很想要成功的企圖心重不重要？.png",width = 1920, height = 1080, res = 200)
dev.off()

final$b1e <- recode(final$b1e, "93:98 = NA")
table(final$b1e,useNA = "ifany")
ggplot(final, aes(x=b1e,fill = b1e)) +
  geom_bar() + stat_count() + 
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.2, color=I("red"), size = 5) +
  xlab("") + ylab("人數") + ggtitle("努力工作重不重要？") +
  png("/R/資料視覺化/final/努力工作重不重要？.png",width = 1920, height = 1080, res = 200)
dev.off()

final$b1f <- recode(final$b1f, "93:98 = NA")
table(final$b1f,useNA = "ifany")
ggplot(final, aes(x=b1f,fill = b1f)) +
  geom_bar() + stat_count() + 
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.2, color=I("red"), size = 5) +
  xlab("") + ylab("人數") + ggtitle("好的人脈（認識有關係的人）重不重要？") +
  png("/R/資料視覺化/final/好的人脈（認識有關係的人）重不重要？.png",width = 1920, height = 1080, res = 200)
dev.off()

final$b1g <- recode(final$b1g, "93:98 = NA")
table(final$b1g,useNA = "ifany")
ggplot(final, aes(x=b1g,fill = b1g)) +
  geom_bar() + stat_count() + 
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.2, color=I("red"), size = 5) +
  xlab("") + ylab("人數") + ggtitle("認識有政治關係的人重不重要？") +
  png("/R/資料視覺化/final/認識有政治關係的人重不重要？.png",width = 1920, height = 1080, res = 200)
dev.off()

final$b1h <- recode(final$b1h, "93:98 = NA")
table(final$b1h,useNA = "ifany")
ggplot(final, aes(x=b1h,fill = b1h)) +
  geom_bar() + stat_count() + 
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.2, color=I("red"), size = 5) +
  xlab("") + ylab("人數") + ggtitle("送紅包重不重要（賄賂有沒有用）?") +
  png("/R/資料視覺化/final/b1h.png",width = 1920, height = 1080, res = 200)
dev.off()

final$b1i <- recode(final$b1i, "93:98 = NA")
table(final$b1i,useNA = "ifany")
ggplot(final, aes(x=b1i,fill = b1i)) +
  geom_bar() + stat_count() + 
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.2, color=I("red"), size = 5) +
  xlab("") + ylab("人數") + ggtitle("一個人的族群背景重不重要？") +
  png("/R/資料視覺化/final/一個人的族群背景重不重要？.png",width = 1920, height = 1080, res = 200)
dev.off()

final$b1j <- recode(final$b1j, "93:98 = NA")
table(final$b1j,useNA = "ifany")
ggplot(final, aes(x=b1j,fill = b1j)) +
  geom_bar() + stat_count() + 
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.2, color=I("red"), size = 5) +
  xlab("") + ylab("人數") + ggtitle("一個人的宗教信仰重不重要？") +
  png("/R/資料視覺化/final/一個人的宗教信仰重不重要？.png",width = 1920, height = 1080, res = 200)
dev.off()

final$b1k <- recode(final$b1k, "93:98 = NA")
table(final$b1k,useNA = "ifany")
ggplot(final, aes(x=b1k,fill = b1k)) +
  geom_bar() + stat_count() + 
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.2, color=I("red"), size = 5) +
  xlab("") + ylab("人數") + ggtitle("一個人是男性或女性重不重要？") +
  png("/R/資料視覺化/final/一個人是男性或女性重不重要？.png",width = 1920, height = 1080, res = 200)
dev.off()

#efa----
final.efa <- subset(final,select = c("b1a","b1b","b1c","b1d","b1e","b1f","b1g",
                                  "b1h","b1i","b1j","b1k"))

library(psych);library(GPArotation)
png("/R/資料視覺化/final/efa plot.png",width = 1920, height = 1080, res = 200)
fa.parallel(final.efa, fa = "fa", fm = "pa")
dev.off()
f.efa <- fa(final.efa, nfactor = 4, rotate = "promax",
            residuals = T, SMC = T, fm = "pa")

f.efa

png("/R/資料視覺化/final/efa.png",width = 1920, height = 1080, res = 200)
fa.diagram(f.efa)
dev.off()
#cfa----
library(lavaan);library(tidySEM)

#原模型

#f.model <- '政經地位 =~ b1a + b1g + b1h
#            個人努力 =~ b1d + b1e + b1f
#            教育背景 =~ b1b + b1c
#            社會背景 =~ b1i + b1j + b1k
#' 
#f.cfa <- cfa(f.model, data = final, estimator = "WLSMV")
#summary(f.cfa, fit.measures = T , standardized = T)
##b1e對模型貢獻度0.229因此將它去除

#新模型
f.model <- '政經地位 =~ b1a + b1g + b1h
            個人努力 =~ b1d + b1e + b1f
            教育背景 =~ b1b + b1c
            社會背景 =~ b1i + b1j + b1k
'
f.cfa <- cfa(f.model, data = final, estimator = "WLSMV")
fitMeasures(f.cfa,fit.measures =c("chisq","df","pvalue","cfi","tli","rmsea",
                                    "rmsea.pvalue","srmr"))
summary(f.cfa, fit.measures = T , standardized = T)

lay <- get_layout("","政經地位", "","教育背景","","","社會背景","","","個人努力","",
                  "b1g", "b1h", "b1a", "b1b", "b1c", "b1j","b1i","b1k","b1e","b1d","b1f",
                  rows = 2)

png("/R/資料視覺化/final/cfa.png",width = 2048, height = 1080, res = 200)
graph_sem(f.cfa,layout = lay)
dev.off()
#####恆等姓(教育程度)----

#構造恆等性
e.ci <- cfa(f.model,data = final,
            estimator = "WLSMV",
            group = "a15a")
summary(e.ci, fit.measures = T, standardized = T)
graph_sem(e.ci)
#弱恆等性
e.wi <- cfa(f.model,data = final,
            estimator = "WLSMV",
            group = "a15a",
            group.equal = "loadings")

s1 <- summary(e.wi, fit.measures = T, standardized = T)
a1 <- s1$PE$lhs[33:43]
b1 <- c()
for (i in 1:11){
  b1[i] = abs(max(s1$PE$std.all[32+i],
                  s1$PE$std.all[76+i],
                  s1$PE$std.all[120+i])-
                min(s1$PE$std.all[32+i],
                    s1$PE$std.all[76+i],
                    s1$PE$std.all[120+i]))
}
diff1 <- data.frame(a1,b1)
diff1
anova(e.wi,e.ci)
#強恆等性
e.si <- cfa(f.model,data = final,
            estimator = "WLSMV",
            group = "a15a",
            group.equal = c("loadings","intercepts"))
summary(e.si, fit.measures = T, standardized = T)

#模型比較
anova(e.wi,e.si) #強恆等性不成立

#部分強恆等性
#1
e.si.p1 <- cfa(f.model,data = final,
             estimator = "WLSMV",
             group = "a15a",
             group.equal = c("loadings","intercepts"),
             group.partial = "b1d~1")

fitMeasures(e.si.p1,fit.measures =c("chisq","df","pvalue","cfi","tli","rmsea",
                                    "rmsea.pvalue","srmr"))
summary(e.si.p1, fit.measures = T, standardized = T)

anova(e.si,e.si.p1)
anova(e.wi,e.si.p1)

#2
e.si.p2 <- cfa(f.model,data = final,
               estimator = "WLSMV",
               group = "a15a",
               group.equal = c("loadings","intercepts"),
               group.partial = c("b1d~1","b1f~1"))

fitMeasures(e.si.p2,fit.measures =c("chisq","df","pvalue","cfi","tli","rmsea",
                                   "rmsea.pvalue","srmr"))
summary(e.si.p2, fit.measures = T, standardized = T)

anova(e.si.p1,e.si.p2)
anova(e.wi,e.si.p2)


#3
e.si.p3 <- cfa(f.model,data = final,
               estimator = "WLSMV",
               group = "a15a",
               group.equal = c("loadings","intercepts"),
               group.partial = c("b1d~1","b1f~1","b1c~1"))

fitMeasures(e.si.p3,fit.measures =c("chisq","df","pvalue","cfi","tli","rmsea",
                                    "rmsea.pvalue","srmr"))
summary(e.si.p3, fit.measures = T, standardized = T)
anova(e.si.p2,e.si.p3)
anova(e.wi,e.si.p3)

#4
e.si.p4 <- cfa(f.model,data = final,
               estimator = "WLSMV",
               group = "a15a",
               group.equal = c("loadings","intercepts"),
               group.partial = c("b1d~1","b1e~1","b1c~1","b1j~1"))

fitMeasures(e.si.p4,fit.measures =c("chisq","df","pvalue","cfi","tli","rmsea",
                                    "rmsea.pvalue","srmr"))
summary(e.si.p4, fit.measures = T, standardized = T)
anova(e.si.p3,e.si.p4)
anova(e.wi,e.si.p4)

png("/R/資料視覺化/final/edu.png",width = 2048, height = 1080, res = 100)
graph_sem(e.si.p4)
dev.off()
#部分完全恆等性
e.sfi <- cfa(f.model,data = final,
             estimator = "WLSMV",
             group = "a15a",
             group.equal = c("loadings","intercepts","residuals",
                             "lv.variances","lv.covariances"),
             group.partial = c("b1d~1","b1e~1","b1c~1","b1j~1"))

fitMeasures(e.sfi,fit.measures = c("chisq","df","pvalue","cfi","tli","rmsea",
                                    "rmsea.pvalue","srmr"))
anova(e.sfi,e.si.p4)

#####恆等性(性別)----
#結構恆等性
s.ci <- cfa(f.model,data =final,
            estimator ="WLSMV",
            group ="a1")
summary(s.ci,fit.measures =TRUE,standardized =T)
fitMeasures(s.ci,fit.measures = c("chisq","df","pvalue","cfi","tli","rmsea",
                                  "rmsea.pvalue","srmr"))
#弱恆等性
s.wi <- cfa(f.model,data =final,
            estimator ="WLSMV",
            group ="a1",
            group.equal ="loadings")

fitMeasures(s.wi,fit.measures = c("chisq","df","pvalue","cfi","tli","rmsea",
                                  "rmsea.pvalue","srmr"))
anova(s.wi,s.ci)

s2 <- summary(s.wi,fit.measures =TRUE,standardized =T)
a2 <- s2$PE$lhs[33:43]
b2 <- abs(s2$PE$std.all[33:43] - s2$PE$std.all[80:90])
diff2 <- data.frame(a2,b2)
diff2

#強恆等性
s.si <- cfa(f.model,data =final,
            estimator ="WLSMV",
            group ="a1",
            group.equal =c("loadings","intercepts"))

fitMeasures(s.si,fit.measures =c("chisq","df","pvalue","cfi","tli","rmsea",
                                 "rmsea.pvalue","srmr"))

summary(s.si,fit.measures =TRUE,standardized =T)

anova(s.wi,s.si)

#部分強恆等性
#1
s.si.p1 <- cfa(f.model,data =final,
            estimator ="WLSMV",
            group ="a1",
            group.equal =c("loadings","intercepts"),
            group.partial = "b1e~1")

fitMeasures(s.si.p1,fit.measures = c("chisq","df","pvalue","cfi","tli","rmsea",
                                     "rmsea.pvalue","srmr"))

summary(s.si.p1,fit.measures =TRUE,standardized =T)
anova(s.si.p1,s.si) 
anova(s.si.p1,s.wi) 

#2
s.si.p2 <- cfa(f.model,data =final,
               estimator ="WLSMV",
               group ="a1",
               group.equal =c("loadings","intercepts"),
               group.partial = c("b1c~1","b1d~1"))

fitMeasures(s.si.p2,fit.measures = c("chisq","df","pvalue","cfi","tli","rmsea",
                                     "rmsea.pvalue","srmr"))

summary(s.si.p2,fit.measures =TRUE,standardized =T)
anova(s.si.p1,s.si.p2)
anova(s.si.p2,s.wi) 


#部分完全恆等性
s.sfi <- cfa(f.model,data =final,
             estimator ="WLSMV",
             group ="a1",
             group.equal =c("loadings","intercepts","residuals",
                            "lv.variances","lv.covariances"),
             group.partial = c("b1c~1","b1d~1"))

fitMeasures(s.sfi,fit.measures = c("chisq","df","pvalue","cfi","tli","rmsea",
                                    "rmsea.pvalue","srmr"))
summary(s.sfi,fit.measures =TRUE,standardized =T)
anova(s.sfi,s.si.p3)

lay <- get_layout("","政經地位", "","教育背景","","","社會背景","","","個人努力","",
                  "b1g", "b1h", "b1a", "b1b", "b1c", "b1j","b1i","b1k","b1e","b1d","b1f",
                  rows = 2)
png("/R/資料視覺化/final/sex.png",width = 2048, height = 1080, res = 150)
graph_sem(s.sfi,lay_out = lay)
dev.off()

#####恆等姓(居住地區)----
#結構恆等性
l.ci <- cfa(f.model,data =final,
            estimator ="WLSMV",
            group ="a5")
summary(l.ci,fit.measures =TRUE,standardized =T)
fitMeasures(l.ci,fit.measures = c("chisq","df","pvalue","cfi","tli","rmsea",
                                  "rmsea.pvalue","srmr"))
#弱恆等性
l.wi <- cfa(f.model,data =final,
            estimator ="WLSMV",
            group ="a5",
            group.equal ="loadings")

fitMeasures(l.wi,fit.measures = c("chisq","df","pvalue","cfi","tli","rmsea",
                                  "rmsea.pvalue","srmr"))
summary(l.wi,fit.measures =TRUE,standardized =T)
anova(l.ci,l.wi)
#強恆等性
l.si <- cfa(f.model,data =final,
            estimator ="WLSMV",
            group ="a5",
            group.equal = c("loadings","intercepts"))
fitMeasures(l.si,fit.measures = c("chisq","df","pvalue","cfi","tli","rmsea",
                                  "rmsea.pvalue","srmr"))
summary(l.si,fit.measures =TRUE,standardized =T)
#模型比較
anova(l.si,l.wi)

#完全恆等性
l.sfi <- cfa(f.model,data =final,
            estimator ="WLSMV",
            group ="a5",
            group.equal = c("loadings","intercepts","residuals",
                            "lv.variances","lv.covariances"))
fitMeasures(l.sfi,fit.measures = c("chisq","df","pvalue","cfi","tli","rmsea",
                                   "rmsea.pvalue","srmr"))
summary(l.sfi,fit.measures =TRUE,standardized =T)
anova(l.sfi,l.si)

png("/R/資料視覺化/final/location.png",width = 2048, height = 1080, res = 150)
graph_sem(l.sfi,lay_out = lay0)
dev.off()

#潛在變項平均數的同質性
l.means <- cfa(f.model,data =final,
               estimator ="WLSMV",
               group ="a5",
               group.equal = c("loadings","intercepts","residuals",
                               "lv.variances","lv.covariances","means"))
fitMeasures(l.means,fit.measures = c("chisq","df","pvalue","cfi","tli","rmsea",
                                   "rmsea.pvalue","srmr"))
summary(l.means,fit.measures =TRUE,standardized =T)
anova(l.sfi,l.means)

