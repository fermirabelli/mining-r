library(MASS) 

#nutrition import from sas 

sasdata <- read.xport("DEMO_H.XPT")

head (sasdata)

tbl = table(survey$Smoke, survey$Exer) 
tbl


tabl_cc=table(presfall$X.43CausaMuert,presfall$X.21Delito)

tabl_cc
chisq.test(tabl_cc) 

as.data.frame(tabl_cc)

plot(presfall)

p<-presfall
head(p)


library(plyr)



out<-(head(Dat[, combos[,1][2]]))
out
t<-chisq.test(Dat[, combos[,1][1]], Dat[, combos[,1][2]])
colnames(Dat)[combos[,1][2]]
colnames(Dat[combos[,1][2]])
attributes(Dat)

Dat<-data.frame()


Dat<-as.data.frame(cbind(p$"año",p$"Mes",p$"X.4LugMuerte", p$"X.6Cárcel"))
colnames(Dat) <- c("año","Mes","LugarMuerte", "Cárcel"   )




Dat<-as.data.frame(cbind.data.frame(p$"año",p$"Mes",p$"X.4LugMuerte", p$"X.6Cárcel",p$"X.7Modulo" ,       p$"X.9EstructPab",    p$"X.15Juzgado",      p$"Juzgado_2",
                         p$"X.16Jurisdicción", p$"Jusrids_2",        p$"X.17TipoDefensa",  p$"TipoDefensa_2",
                         p$"X.19SituaProce",   p$"SitProc_2"  ,      p$"X.20MontoCond" ,   p$"MontoCond_2" ,
                         p$"X.21Delito"     ,  p$"Delito_2"   ,      p$"RangoEtar"  ,      p$"X.25Sexo",        					
                         
                         p$"X.26Género" ,      p$"X.27Nacionalid",   p$"X.28NacExtr"  ,    p$"X.36ModalidConoc")) 	                   

colnames(Dat) <- c("año","Mes","LugarMuerte", "Cárcel" ,"Modulo", "EstructPabellon",    "Juzgado", "Juzgado_2" ,"Jurisdicción", "Jusrids_2",        "TipoDefensa",  "TipoDefensa_2",
                   "SituaProcesal" ,  "SitProc_2"     ,   "MontoCond",    "MontoCond_2","Delito",       "Delito_2" ,        "RangoEtario",        "Sexo",
                   "Género" ,      "Nacionalid" ,  "NacExtr",      "ModalidConocimiento")
combos <- combn(ncol(Dat),2)

res<-as.data.frame(adply(combos, 2, function(x) {

  test <- chisq.test(Dat[, x[1]], Dat[, x[2]])
  #out<-(head(Dat[, x[1]]))
  out <- data.frame("Row" = colnames(Dat)[x[1]]
                    , "Column" = colnames(Dat[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    ,  "df"= test$parameter
                    ,  "p.value" = round(test$p.value, 3)
  )
  return(out)
  
})  )
res

#Devuelvo combinacion de atributos con mas dependencia 
arrange(res,desc(p.value))

mytable<-table(Dat$Delito     ,Dat$TipoDefensa)
margin.table(mytable, 1) # A frequencies (summed over B)
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages
prop.table(mytable, 2) # column percentages 

summary(mytable)

mytable<-table(Dat$Delito     ,Dat$Nacionalid)
mytable<-table(Dat$Delito     ,Dat$NacExtr)



chisq.test(Dat$Jurisdicción     ,Dat$TipoDefensa)

plot(Dat$Jurisdicción     ,Dat$TipoDefensa)

test <-chisq.test(Dat[, 1], Dat[, 2])
test <- chisq.test(Dat[, x[1]], Dat[, x[2]])

out <- data.frame("Row" = colnames(Dat)[1]
                  , "Column" = colnames(Dat[2])
                  , "Chi.Square" = round(test$statistic,3)
                  ,  "df"= test$parameter
                  ,  "p.value" = round(test$p.value, 3)
                  
)
out

?rnorm

x<-data.frame(
a=rnorm(570, mean = 12, sd = 40),
b=rnorm(570, mean = -1, sd = 25))

chisq.test(x$a,x$b)

plot(x$a,x$b)
cor(x$a,x$b)

temp <- rpart(x$a ~ x$b)
plot(temp)
text(temp)

x<-runif(100,0,100)
v<-c(-Inf,-10,0,10,15,40,Inf)
length(v)
u <- cut(x$a, breaks = v, labels = c(1:(length(v)-1)))
y <- cut(x$b, breaks = v, labels = c(1:(length(v)-1)))
u
y
cbind(x, findInterval(x, u))
plot(u,y)
chisq.test(u,y)

library(Hmisc) # cut2
split(x, cut2(x$a, g=3))
x$wt <- as.numeric(cut2(x$a, g=4))
x$wt2 <- as.numeric(cut2(x$b, g=4))

chisq.test(x$wt,x$wt2)
plot(x$wt,x$wt2)
x
