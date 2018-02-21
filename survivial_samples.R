library(data.table)
data("larynx", package="KMsurv")
l1 <- ten(Surv(time, delta) ~ stage, data=larynx)
comp(l1)
attributes(l1)
wigga <- data.frame(attr(l1, "lrt"))
rownames(wigga)<- c("Log-Rank","Fleming(1)","Peto","Modified Peto","Tarone","Wilcoxon")

str(wigga)
wigga <- wigga[,c(2:4)]

## Two covariate groups
data("leukemia", package="survival")
f1 <- survfit(Surv(time, status) ~ x, data=leukemia)
stoopid <- tidy(comp(ten(f1)))
str(stoopid)                           
attr(f1, "tft")

tidy(f1)
## K&M 2nd ed. Example 7.2, Table 7.2, pp 209--210.
data("kidney", package="KMsurv")
t1 <- ten(Surv(time=time, event=delta) ~ type, data=kidney)
stigga <- pander(comp(t1, p=c(0, 1, 1, 0.5, 0.5), q=c(1, 0, 1, 0.5, 2)))
## see the weights used
attributes(t1)$lrw
## supremum (Renyi) test; two-sided; two covariate groups
## K&M 2nd ed. Example 7.9, pp 223--226.
data("gastric", package="survMisc")
g1 <- ten(Surv(time, event) ~ group, data=gastric)
comp(g1)
## Three covariate groups
## K&M 2nd ed. Example 7.4, pp 212-214.
data("bmt", package="KMsurv")
b1 <- ten(Surv(time=t2, event=d3) ~ group, data=bmt)
attributes(b1)
comp(b1, p=c(1, 0, 1), q=c(0, 1, 1))[[1]]
## Tests for trend
## K&M 2nd ed. Example 7.6, pp 217-218.
data("larynx", package="KMsurv")
l1 <- ten(Surv(time, delta) ~ stage, data=larynx)
comp(l1)
attr(l1, "tft")
### see effect of F-H test
data("alloauto", package="KMsurv")
a1 <- ten(Surv(time, delta) ~ type, data=alloauto)
comp(a1, p=c(0, 1), q=c(1, 1))


data("kidney", package="KMsurv")
t1 <- ten(survfit(Surv(time, delta) ~ type, data=kidney))
autoplot(t1)
autoplot(t1, type="fill", survLineSize=2, jitter="all")
autoplot(t1, timeTicks="months",
         type="CI", jitter="all",
         legLabs=c("surgical", "percutaneous"),
         title="Time to infection following catheter placement \n
         by type of catheter, for dialysis patients",
         titleSize=10, censSize=2)$plot
t2 <- ten(survfit(Surv(time=time, event=delta) ~ 1, data=kidney))
autoplot(t2, legLabs="")$plot
autoplot(t2, legend=FALSE)
data("rectum.dat", package="km.ci")
t3 <- ten(survfit(Surv(time, status) ~ 1, data=rectum.dat))
## change confidence intervals to log Equal-Precision confidence bands
ci(t3, how="nair", tL=1, tU=40)
autoplot(t3, type="fill", legend=FALSE)$plot
## manually changing the output
t4 <- ten(survfit(Surv(time, delta) ~ type, data=kidney))
(a4 <- autoplot(t4, type="CI", alpha=0.8, survLineSize=2)$plot)
## change default colors
a4 + list(ggplot2::scale_color_manual(values=c("red", "blue")),
          ggplot2::scale_fill_manual(values=c("red", "blue")))
## change limits of y-axis
suppressMessages(a4 + ggplot2::scale_y_continuous(limits=c(0, 1)))


data("bmt", package="KMsurv")
b1 <- ten(Surv(time=t2, event=d3) ~ group, data=bmt)
comp(b1, p=c(1, 0, 1), q=c(0, 1, 1))

