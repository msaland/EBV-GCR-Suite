### R code from vignette source 'timedep.Rnw'

###################################################
### code chunk number 1: preamble
###################################################
options(width=60, continue=" ")
makefig <- function(file, top=1, right=1, left=4) {
    pdf(file, width=9.5, height=7, pointsize=18)
    par(mar=c(4, left, top, right) +.1)
    }
library(survival)


###################################################
### code chunk number 2: testdata
###################################################
tdata <- data.frame(subject=c(5,5,5), time1=c(0,90, 120),
                    time2 = c(90, 120, 185), death=c(0,0,1),
                    creatinine=c(0.9, 1.5, 1.2))
tdata


###################################################
### code chunk number 3: fake
###################################################
getOption("SweaveHooks")[["fig"]]()
set.seed(1953)  # a good year
nvisit <- floor(pmin(lung$time/30.5, 12))
response <- rbinom(nrow(lung), nvisit, .05) > 0
badfit <- survfit(Surv(time/365.25, status) ~ response, data=lung)
plot(badfit, mark.time=FALSE, lty=1:2, 
     xlab="Years post diagnosis", ylab="Survival")
legend(1.5, .85, c("Responders", "Non-responders"), 
       lty=2:1, bty='n')


###################################################
### code chunk number 4: timedep.Rnw:201-203 (eval = FALSE)
###################################################
## fit <- coxph(Surv(time1, time2, status) ~ age + creatinine, 
##              data=mydata)


###################################################
### code chunk number 5: timedep.Rnw:274-275 (eval = FALSE)
###################################################
## newdata <- tmerge(data1, data2, id, newvar=tdc(time, value), ...)


###################################################
### code chunk number 6: timedep.Rnw:320-321
###################################################
cgd0[1:4,]


###################################################
### code chunk number 7: cgd1
###################################################
dim(cgd0)
newcgd <- tmerge(data1=cgd0[, 1:13], data2=cgd0, id=id, tstop=futime)
newcgd <- tmerge(newcgd, cgd0, id=id, infect = event(etime1)) 
newcgd <- tmerge(newcgd, cgd0, id=id, infect = event(etime2)) 
newcgd <- tmerge(newcgd, cgd0, id=id, infect = event(etime3)) 
newcgd <- tmerge(newcgd, cgd0, id=id, infect = event(etime4)) 
newcgd <- tmerge(newcgd, cgd0, id=id, infect = event(etime5)) 
newcgd <- tmerge(newcgd, cgd0, id=id, infect = event(etime6)) 
newcgd <- tmerge(newcgd, cgd0, id=id, infect = event(etime7)) 
newcgd <- tmerge(newcgd, newcgd, id, enum=cumtdc(tstart))
dim(newcgd)
newcgd[1:5,c(1, 4:6, 13:17)]
summary(newcgd)
coxph(Surv(tstart, tstop, infect) ~ treat + inherit + steroids,
      data =newcgd, cluster = id)


###################################################
### code chunk number 8: cgd1b
###################################################
test  <- tmerge(cgd0[, 1:13], cgd0, id=id, tstop=futime,
                 infect = event(etime1), infect= event(etime2),
                 infect = event(etime3), infect= event(etime4),
                 infect = event(etime5), infect= event(etime6),
                 infect = event(etime7))
test <- tmerge(test, test,  id= id, enum = cumtdc(tstart))
all.equal(newcgd, test)


###################################################
### code chunk number 9: cgd1c
###################################################
# create a long data set with the recurrences
temp <- reshape(cgd0[c(1, 14:20)], varying= 2:8, v.names="etime",
                 idvar="id", direction="long")
cgdrecur <- subset(temp, !is.na(etime))  # toss missings (not essential)

newcgd <- tmerge(data1=cgd0[, 1:13], data2=cgd0, id=id, tstop=futime)
newcgd <- tmerge(newcgd, cgdrecur, id=id, infect= event(etime))


###################################################
### code chunk number 10: stanford
###################################################
jasa$subject <- 1:nrow(jasa)  #we need an identifier variable
tdata <- with(jasa, data.frame(subject = subject,
                               futime= pmax(.5, fu.date - accept.dt),
                               txtime= ifelse(tx.date== fu.date,
                                              (tx.date -accept.dt) -.5,
                                              (tx.date - accept.dt)),
                               fustat = fustat
                             ))
xdata <- tmerge(jasa, tdata, id=subject,
                  death = event(futime, fustat),
                  transplant   =  tdc(txtime), 
                  options= list(idname="subject"))

sdata <- tmerge(jasa, tdata, id=subject,
                  death = event(futime, fustat),
                  trt   =  tdc(txtime), 
                  options= list(idname="subject"))
attr(sdata, "tcount")
sdata$age <- sdata$age -48
sdata$year <- as.numeric(sdata$accept.dt - as.Date("1967-10-01"))/365.25

# model 6 of the table in K&P
coxph(Surv(tstart, tstop, death) ~ age*trt + surgery + year, 
      data= sdata, ties="breslow")


###################################################
### code chunk number 11: pbc
###################################################
temp <- subset(pbc, id <= 312, select=c(id:sex, stage)) # baseline
pbc2 <- tmerge(temp, temp, id=id, death = event(time, status)) #set range
pbc2 <- tmerge(pbc2, pbcseq, id=id, ascites = tdc(day, ascites),
               bili = tdc(day, bili), albumin = tdc(day, albumin),
               protime = tdc(day, protime), alk.phos = tdc(day, alk.phos))
fit1 <- coxph(Surv(time, status==2) ~ log(bili) + log(protime), pbc)
fit2 <- coxph(Surv(tstart, tstop, death==2) ~ log(bili) + log(protime), pbc2)
rbind('baseline fit' = coef(fit1),
      'time dependent' = coef(fit2))


###################################################
### code chunk number 12: timedep.Rnw:619-620
###################################################
attr(pbc2, "tcount")


###################################################
### code chunk number 13: timedep.Rnw:622-624
###################################################
#grab a couple of numbers for the paragraph below
atemp <- attr(pbc2, "tcount")[2:3,]


###################################################
### code chunk number 14: timedep.Rnw:705-711 (eval = FALSE)
###################################################
## temp <- subset(pbc, id <= 312, select=c(id:sex, stage))
## pbc2 <- tmerge(temp, temp, id=id, death = event(time, status))
## pbc2a <- tmerge(pbc2, pbcseq, id=id, ascites = tdc(day, ascites),
##                bili = tdc(day, bili), options= list(delay=14))
## pbc2b <- tmerge(pbc2, pbcseq, id=id, ascites = tdc(day+14, ascites),
##                bili = tdc(day+14, bili))


###################################################
### code chunk number 15: rep (eval = FALSE)
###################################################
## newd <- tmerge(data1=base, data2=timeline, id=repid, tstart=age1, 
##                tstop=age2, options(id="repid"))
## newd <- tmerge(newd, outcome, id=repid, mcount = cumtdc(age))
## newd <- tmerge(newd, subset(outcome, event='diabetes'), 
##                diabetes= tdc(age))
## newd <- tmerge(newd, subset(outcome, event='arthritis'), 
##                arthritis= tdc(age))


###################################################
### code chunk number 16: veteran1
###################################################
options(show.signif.stars = FALSE)  # display statistical intelligence
vfit <- coxph(Surv(time, status) ~ trt + prior + karno, veteran)
vfit
quantile(veteran$karno)

zp <- cox.zph(vfit, transform= function(time) log(time +20))
zp


###################################################
### code chunk number 17: veteran1b
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(zp[3], resid=FALSE)    # a plot for the 3rd variable in the fit
abline(0,0, lty=3)
abline(h= vfit$coef[3], lwd=2, lty=3)


###################################################
### code chunk number 18: split
###################################################
vet2 <- survSplit(Surv(time, status) ~ ., data= veteran, cut=c(90, 180), 
                  episode= "tgroup", id="id")
vet2[1:7, c("id", "tstart", "time", "status", "tgroup", "age", "karno")]


###################################################
### code chunk number 19: split2
###################################################
vfit2 <- coxph(Surv(tstart, time, status) ~ trt + prior +
                  karno:strata(tgroup), data=vet2)
vfit2
cox.zph(vfit2)


###################################################
### code chunk number 20: split3
###################################################
vfit2$means


###################################################
### code chunk number 21: split4
###################################################
getOption("SweaveHooks")[["fig"]]()
quantile(veteran$karno)
cdata <- data.frame(tstart= rep(c(0,90,180), 2),
                    time =  rep(c(90,180, 365), 2),
                    status= rep(0,6),   #necessary, but ignored
                    tgroup= rep(1:3, 2),
                    trt  =  rep(1,6),
                    prior=  rep(0,6),
                    karno=  rep(c(40, 75), each=3),
                    curve=  rep(1:2, each=3))
cdata
sfit <- survfit(vfit2, newdata=cdata, id=curve)
km <- survfit(Surv(time, status) ~ I(karno>60), veteran)
plot(km, xmax= 365, col=1:2, lwd=2, 
     xlab="Days from enrollment", ylab="Survival")
lines(sfit, col=1:2, lty=2, lwd=2)


###################################################
### code chunk number 22: vfit3 (eval = FALSE)
###################################################
## vfit3 <- coxph(Surv(time, status) ~ trt + prior + karno +
##                 I(karno * log(time + 20)), data=veteran)


###################################################
### code chunk number 23: vet3
###################################################
vfit3 <-  coxph(Surv(time, status) ~ trt + prior + karno + tt(karno),
                data=veteran,
                tt = function(x, t, ...) x * log(t+20))
vfit3


###################################################
### code chunk number 24: vet3b
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(zp[3])
abline(coef(vfit3)[3:4], lwd=2, lty=3, col=2)


###################################################
### code chunk number 25: vet4
###################################################
vfit4 <-  coxph(Surv(time, status) ~ trt + prior + karno + tt(karno),
                data=veteran,
                tt = function(x, t, ...) x* nsk(t, knots=c(5, 100, 200, 400),
                                                Boundary.knots = FALSE))
vfit4


###################################################
### code chunk number 26: ties1
###################################################
data1 <- read.table(col.names=c("id", "diabetes", "lfu", "status"),
                          header=FALSE, text="
1   5 30  1
2  10 15  1
3  NA 60  0
4  NA 80  1
5  10 80  0
6  NA 90  1
7  30 95  1
")

data1$d2 <- pmin(data1$diabetes, 300, na.rm=TRUE) #replace NA with 300
fit1 <- coxph(Surv(lfu, status) ~ tt(d2), data=data1,
              tt = function(d2, t, ...) ifelse(t > d2, 1, 0))
fit2 <- coxph(Surv(lfu, status) ~ tt(d2), data=data1,
              tt = function(d2, t, ...) ifelse(t < d2, 0, 1))
c(coef(fit1), coef(fit2))


###################################################
### code chunk number 27: ties2
###################################################
data2 <- tmerge(data1, data1, id=id, dstat=event(lfu, status),
                diab = tdc(diabetes))
subset(data2, id %in% c(1,7), c(id, tstart:diab))               
fit3 <- coxph(Surv(tstart, tstop, dstat) ~ diab, data2)
c(coef(fit1), coef(fit2), coef(fit3))


###################################################
### code chunk number 28: pbctime
###################################################
pfit1 <- coxph(Surv(time, status==2) ~ log(bili) + ascites + age, pbc)
pfit2 <- coxph(Surv(time, status==2) ~ log(bili) + ascites + tt(age),
                data=pbc,
                tt=function(x, t, ...) {
                    age <- x + t/365.25 
                    cbind(cage=age, cage2= (age-50)^2, cage3= (age-50)^3)
                })
pfit2
anova(pfit2)
# anova(pfit1, pfit2)  #this fails
2*(pfit2$loglik - pfit1$loglik)[2]


###################################################
### code chunk number 29: expand
###################################################
dtimes <- sort(unique(with(pbc, time[status==2])))
tdata <- survSplit(Surv(time, status==2) ~., pbc, cut=dtimes)
tdata$c.age <- tdata$age + tdata$time/365.25 -50  #current age, centered at 50
pfit3 <- coxph(Surv(tstart, time, event) ~ log(bili) + ascites + c.age +
    I(c.age^2) + I(c.age^3), data=tdata)
rbind(coef(pfit2), coef(pfit3))


###################################################
### code chunk number 30: expand2
###################################################
dtime2 <- 1:11 * 365.25
tdata2 <-survSplit(Surv(time, status==2) ~., pbc, cut=dtime2)
tdata2$c.age <- tdata2$age + tdata2$time/365.25 -50  #current age, centered at 50
pfit4 <- coxph(Surv(tstart, time, event) ~ log(bili) + ascites + c.age + 
    I(c.age^2) + I(c.age^3), data=tdata2)
rbind('1 day grid'= coef(pfit3), '1 year grid'= coef(pfit4))
#
c(tdata=nrow(tdata), tdata2=nrow(tdata2)) 


###################################################
### code chunk number 31: veteran3
###################################################
getOption("SweaveHooks")[["fig"]]()
dtime <- round(1:13 * 30.5)
vdata2 <- survSplit(Surv(time, status) ~ ., veteran, cut=dtime,
                    episode= "month")
vfit1 <- coxph(Surv(tstart, time, status) ~ trt + prior + karno, vdata2)
vfit5 <- coxph(Surv(tstart, time, status) ~ trt + prior + karno + 
                   karno:nsk(month, df=3), vdata2)
anova(vfit1, vfit5)

tdata <- expand.grid(trt=0, prior=0, karno=30, month=seq(1,13, length=50))
yhat <- predict(vfit5, newdata=tdata, se.fit=TRUE, reference="zero")
yy <- yhat$fit+ outer(yhat$se.fit, c(0, -1.96, 1.96), '*')
matplot(seq(1,13, length=50), yy, type='l', lty=c(1,2,2), col=1, lwd=c(1,2,2),
        xlab="Month of fu", ylab="Effect, Karnofsky 60 vs 90")


###################################################
### code chunk number 32: timedep.Rnw:1291-1298
###################################################
function(x, t, riskset, weights){ 
    obrien <- function(x) {
        r <- rank(x)
        (r-.5)/(.5+length(r)-r)
    }
    unlist(tapply(x, riskset, obrien))
}


###################################################
### code chunk number 33: timedep.Rnw:1308-1310
###################################################
function(x, t, riskset, weights) 
    unlist(tapply(x, riskset, rank))


