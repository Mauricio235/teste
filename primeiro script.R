
library(survival)
library(readxl)

dados <- read_excel("C:/Users/LENOVO/Desktop/Projeto monica respiração/DRIVING PRESSURE 13SET24.xlsx", 
                    sheet = "Planilha1")
str(dados)

colnames(dados)
dados$SEXO = as.factor(dados$SEXO)
dados$`LESAO PULMONAR` = as.factor(dados$`LESAO PULMONAR`)
dados$RESP = as.factor(dados$RESP)
dados$DIRETA = as.factor(dados$DIRETA)
dados$COMORBIDADE = as.factor(dados$COMORBIDADE)
dados$CURARE = as.factor(dados$CURARE)
dados$CUFF  = as.factor(dados$CUFF)

modelo_cox <- coxph(Surv(SOBREVIDA, OBITO) ~ IDADE + NUTRICAO + RESP + 
                      dados$`LESAO PULMONAR` + dados$PIM +dados$IO1+
                      dados$PAO21 + dados$paO2FIO21 + dados$`DP A1`+ 
                      dados$`DP B1`+ dados$`DP C1`+dados$`MP A1`+
                      dados$`MP B1`+ dados$`MP C1`+dados$`MP D1`+
                      dados$`MP E1`+dados$PPLATO1+dados$PEEP1+
                      dados$VCMLKG1+ dados$TEMPOVM+ dados$COMORBIDADE, data = dados)


colnames(dados)
str(dados)
plot(modelo_cox)

# Realizar o teste de Grambsch e Therneau
cox.zph(modelo_cox)
str(dados)

# Geração da curva de sobrevivência predita
curva_cox <- survfit(modelo_cox)

# Geração da curva de sobrevivência Kaplan-Meier
curva_km <- survfit(Surv(dados$SOBREVIDA, dados$OBITO) ~ 1, data = dados, conf.int = F)


# Plotando a curva de sobrevivência
plot(curva_cox, xlab = "Tempo", ylab = "Probabilidade de Sobrevivência", 
     main = "Curva de Sobrevivência do Modelo de Cox", col = "blue")
lines(curva_km, col = "red")
# Adicionando a legenda
legend("bottomleft", legend = "Modelo de Cox", col = "#FA8072", lwd = 2)




#Analisar essas variaveis 
#DIRETA +
#`TIPO LESAO`           

cor(dados$CDIN1, dados$CEST1)

plot(dados$CDIN1, dados$CEST1)

library(car)
plot(dados)
vif(modelo_cox)


################################################################################################


library(readr)
library(dplyr)
library(flexsurv)


# 1-Modelo Exponencial
ajust1<-survreg(Surv(SOBREVIDA, OBITO) ~ IDADE + NUTRICAO + RESP + 
                  dados$`LESAO PULMONAR` + dados$PIM +dados$IO1+
                  dados$PAO21 + dados$paO2FIO21 + dados$`DP A1`+ 
                  dados$`DP B1`+ dados$`DP C1`+dados$`MP A1`+
                  dados$`MP B1`+ dados$`MP C1`+dados$`MP D1`+
                  dados$`MP E1`+dados$PPLATO1+dados$PEEP1+
                  dados$VCMLKG1+ dados$TEMPOVM, data = dados, dist = "exponential")
summary(ajust1)
xb1<-exp(ajust1$linear.predictors);xb1
ste<-exp(-(dados$SOBREVIDA/xb1));ste  # Funcao de sobrevivencia
ajust1$loglik
AIC(ajust1)
BIC(ajust1)
#
#
#Calculo do AIC (-2*loglik+2*(p+k) em que p é o num de param da dist e k é o num. de parametros (sem o intercepto)
ajust1_AIC<- -2*ajust1$loglik[2]+2*(1+7);ajust1_AIC   
#Calculo do BIC (-2*loglik+(p+k)*log(r) #r é o tamanho da amostra
r<-length(dados$OBITO);r   
ajust1_BIC<- -2*ajust1$loglik[2]+(1+7)*log(r);ajust1_BIC   
#
#
# 2-Modelo Weibull
ajust2<-survreg(Surv(SOBREVIDA, OBITO) ~ IDADE + NUTRICAO + RESP + 
                  dados$`LESAO PULMONAR` + dados$PIM +dados$IO1+
                  dados$PAO21 + dados$paO2FIO21 + dados$`DP A1`+ 
                  dados$`DP B1`+ dados$`DP C1`+dados$`MP A1`+
                  dados$`MP B1`+ dados$`MP C1`+dados$`MP D1`+
                  dados$`MP E1`+dados$PPLATO1+dados$PEEP1+
                  dados$VCMLKG1+ dados$TEMPOVM, data = dados, dist = "weibull")
summary(ajust2)
xb2<-exp(ajust2$linear.predictors);xb2
gamma2<-1/ajust2$scale;gamma2
stw<-exp(-(dados$SOBREVIDA/xb2)^gamma2);stw
ajust2$loglik
AIC(ajust2)
BIC(ajust2)
#
#Calculo do AIC (-2*loglik+2*(p+k) em que p é o num de param da dist e k é o num de covariaveis
ajust2_AIC<- -2*ajust2$loglik[2]+2*(2+7);ajust2_AIC   
#Calculo do BIC (-2*loglik+(p+k)*log(r) #r é o tamanho da amostra
r<-length(dados$OBITO);r   
ajust2_BIC<- -2*ajust2$loglik[2]+(2+7)*log(r);ajust2_BIC   

#
#Calcular o Std. Error do exp(Log(scale))
vb <- vcov(ajust2);vb #ou ajust2$var
grad <- ajust2$scale;grad #ja em exponencial
vb2 <- vcov(ajust2)[9,9];vb2
vG <- grad %*% vb2 %*% grad;vG
sqrt(vG) # Std. Error do scale
#
#
# 3-Modelo Log-normal (intercepto é o mi e o Scale é o sigma 
ajust3<-survreg(Surv(SOBREVIDA, OBITO) ~ IDADE + NUTRICAO + RESP + 
                  dados$`LESAO PULMONAR` + dados$PIM +dados$IO1+
                  dados$PAO21 + dados$paO2FIO21 + dados$`DP A1`+ 
                  dados$`DP B1`+ dados$`DP C1`+dados$`MP A1`+
                  dados$`MP B1`+ dados$`MP C1`+dados$`MP D1`+
                  dados$`MP E1`+dados$PPLATO1+dados$PEEP1+
                  dados$VCMLKG1+ dados$TEMPOVM, data = dados, dist = "lognorm")
summary(ajust3)
xb3<-ajust3$linear.predictors;xb3
sigma3<-ajust3$scale;sigma3
stln<-pnorm((-log(dados$SOBREVIDA)+ xb3)/sigma3);stln
ajust3$loglik
AIC(ajust3)
BIC(ajust3)
#
#Calculo do AIC (-2*loglik+2*(p+k) em que p é o num de param da dist e k é o num de covariaveis
ajust3_AIC<- -2*ajust3$loglik[2]+2*(2+8);ajust3_AIC   
#Calculo do BIC (-2*loglik+(p+k)*log(r) #r é o tamanho da amostra
r<-length(dados$OBITO);r   
ajust3_BIC<- -2*ajust3$loglik[2]+(2+9)*log(r);ajust3_BIC 
#
#Calcular o Std. Error do exp(Log(scale))
vb <- vcov(ajust3);vb #ou ajust3$var
grad <- ajust3$scale;grad #ja em exponencial
vb2 <- vcov(ajust3)[3,3];vb2
vG <- grad %*% vb2 %*% grad;vG
sqrt(vG) # Std. Error do scale
#
# 4-Modelo Log-logistico  
ajust4<-survreg(Surv(SOBREVIDA, OBITO) ~ IDADE + NUTRICAO + RESP + 
                  dados$`LESAO PULMONAR` + dados$PIM +dados$IO1+
                  dados$PAO21 + dados$paO2FIO21 + dados$`DP A1`+ 
                  dados$`DP B1`+ dados$`DP C1`+dados$`MP A1`+
                  dados$`MP B1`+ dados$`MP C1`+dados$`MP D1`+
                  dados$`MP E1`+dados$PPLATO1+dados$PEEP1+
                  dados$VCMLKG1+ dados$TEMPOVM, data = dados, dist = "loglogistic")
summary(ajust4) 
xb4<-exp(ajust4$linear.predictors);xb4       
gamma4<-1/ajust4$scale;gamma4
stllog<- 1/(1+(dados$SOBREVIDA/xb4)^gamma4);stllog
ajust4$loglik
AIC(ajust4)
BIC(ajust4)


# 
#Calculo do AIC (-2*loglik+2*(p+k) em que p é o num de param da dist e k é o num de covariaveis
ajust4_AIC<- -2*ajust4$loglik[2]+2*(2+9);ajust4_AIC   
#Calculo do BIC (-2*loglik+(p+k)*log(r) #r é o tamanho da amostra
r<-length(dados$OBITO);r   
ajust4_BIC<- -2*ajust4$loglik[2]+(2+9)*log(r);ajust4_BIC 
#
#Calcular o Std. Error do exp(Log(scale))
vb <- vcov(ajust4);vb #ou ajust4$var
grad <- ajust4$scale;grad #ja em exponencial
vb2 <- vcov(ajust4)[3,3];vb2
vG <- grad %*% vb2 %*% grad;vG
sqrt(vG) # Std. Error do scale
#
# 5-Modelo Logistico  
ajust5<-survreg(Surv(SOBREVIDA, OBITO) ~ IDADE + NUTRICAO + RESP + 
                  dados$`LESAO PULMONAR` + dados$PIM +dados$IO1+
                  dados$PAO21 + dados$paO2FIO21 + dados$`DP A1`+ 
                  dados$`DP B1`+ dados$`DP C1`+dados$`MP A1`+
                  dados$`MP B1`+ dados$`MP C1`+dados$`MP D1`+
                  dados$`MP E1`+dados$PPLATO1+dados$PEEP1+
                  dados$VCMLKG1+ dados$TEMPOVM, data = dados, dist = "logistic")
summary(ajust5)
xb5<-ajust5$linear.predictors;xb5
sigma5<-ajust5$scale;sigma5
stlog <- 1/(1+exp((dados$SOBREVIDA-xb5)/sigma5));stlog
ajust5$loglik
AIC(ajust5)
BIC(ajust5)
#
#Calculo do AIC (-2*loglik+2*(p+k) em que p é o num de param da dist e k é o num de covariaveis
ajust5_AIC<- -2*ajust5$loglik[2]+2*(2+7);ajust5_AIC  #Logistica
#Calculo do BIC (-2*loglik+(p+k)*log(r) em que p é o num de param da dist e r é o tamanho da amostra
r<-length(dados$OBITO);r 
ajust5_BIC<- -2*ajust5$loglik[2]+(2+7)*log(r);ajust5_BIC  #Logistica
#
#Calcular o Std. Error do exp(Log(scale))
vb <- vcov(ajust5);vb #ou ajust5$var
grad <- ajust5$scale;grad #ja em exponencial
vb2 <- vcov(ajust5)[3,3];vb2
vG <- grad %*% vb2 %*% grad;vG
sqrt(vG) # Std. Error do scale
#
# 6-Modelo Gaussiano (Normal)  
ajust6<-survreg(Surv(SOBREVIDA, OBITO) ~ IDADE + NUTRICAO + RESP + 
                  dados$`LESAO PULMONAR` + dados$PIM +dados$IO1+
                  dados$PAO21 + dados$paO2FIO21 + dados$`DP A1`+ 
                  dados$`DP B1`+ dados$`DP C1`+dados$`MP A1`+
                  dados$`MP B1`+ dados$`MP C1`+dados$`MP D1`+
                  dados$`MP E1`+dados$PPLATO1+dados$PEEP1+
                  dados$VCMLKG1+ dados$TEMPOVM, data = dados, dist = "gaussian")
summary(ajust6)
xb6<-ajust6$linear.predictors;xb6
sigma6<-ajust6$scale;sigma6
stgau <- 1-pnorm((dados$SOBREVIDA-xb6)/sigma6);stgau
ajust6$loglik
AIC(ajust6)
BIC(ajust6)
#
# Calculo do AIC
ajust6_AIC<- -2*ajust6$loglik[2]+2*(2+7); ajust6_AIC #Normal
#Calculo do BIC (-2*loglik+(p+k)*log(r) em que p é o num de param da dist e r é o número total de falhas
r<-length(dados$OBITO);r
ajust6_BIC<- -2*ajust6$loglik[2]+(2+7)*log(r);ajust6_BIC   
#
#Calcular o Std. Error do exp(Log(scale))
vb <- vcov(ajust6);vb #ou ajust4$var
grad <- ajust6$scale;grad #ja em exponencial
vb2 <- vcov(ajust6)[3,3];vb2
vG <- grad %*% vb2 %*% grad;vG
sqrt(vG) # Std. Error do scale
#
# Tabela de resultados
model <- c("Exponencial", "Weibull", "LogNormal",
           "Log-Logistico","Logistico","Gaussiano")
AIC <- as.numeric(c(AIC(ajust1),AIC(ajust2),AIC(ajust3),
                    AIC(ajust4),AIC(ajust5),AIC(ajust6)))
BIC <- as.numeric(c(BIC(ajust1),BIC(ajust2),BIC(ajust3),
                    BIC(ajust4),BIC(ajust5),BIC(ajust6)))
resultados <- cbind(AIC,BIC)
rownames(resultados) <- model
colnames(resultados) <- c("AIC","BIC")
print(resultados, digits = 7)
min(AIC)
min(BIC)
#
# AIC  
AIC_results <-c(ajust1_AIC,ajust2_AIC,ajust3_AIC,
                ajust4_AIC,ajust5_AIC,ajust6_AIC)
AIC_results
#
# BIC  
BIC_results <-c(ajust1_BIC,ajust2_BIC,ajust3_BIC,
                ajust4_BIC,ajust5_BIC,ajust6_BIC)
BIC_results
#
#
# Akaike weights
require(mvMORPH)
aicw(AIC_results)
#
# AICc weights para pequenas amostras
aicw(AIC_results, aicc=TRUE)
#
require(cogmod)
AICw(AIC_results)
BICw(BIC_results)
#
#
#==================================================================================
#CALCULO DOS RESIDUOS
#==================================================================================
# 1-Residuos do Modelo Exponencial
#==================================
#Residuos de Cox-Snell
rcs1<- -log(ste);rcs1  # residuos cox-snell
ekm1cs<-survfit(Surv(rcs1,dados$OBITO)~1,data=dados)
t1cs<-ekm1cs$time;t1cs
ste_km<-ekm1cs$surv;ste_km
Sexp_cs<-exp(-t1cs);Sexp_cs   
#exponencial padrao
par(mfrow=c(1,2))
plot(ste_km,Sexp_cs,xlim=c(0,1),ylim=c(0,1),xlab="S(ei): Kaplan-Meier",ylab="S(ei): Exponencial padrao",pch=16)
lines(c(0,1), c(0,1), type="l", lty=3)
plot(ekm1cs,conf.int=F,mark.time=F, xlab="Cox-Snell residuals", 
     ylab="Survival probability",xlim=c(0,2.0),main="Exponential")
lines(t1cs,Sexp_cs,lty=2)
legend(-0.1,0.3,lty=c(1,2),c("Kaplan-Meier","Standard Exponential"),
       cex=0.6,bty="n")
#
# 2-Residuos do Modelo Weilbull
#================================
#Residuos de Cox-Snell
rcs2<- -log(stw);rcs2  # residuos cox-snell
ekm2cs<-survfit(Surv(rcs2,dados$OBITO)~1,data=dados)
t2cs<-ekm2cs$time;t2cs
stw_km<-ekm2cs$surv;stw_km
Stw_cs<-exp(-t2cs);Stw_cs   
# Weibull padrao
par(mfrow=c(1,2))
plot(stw_km,Stw_cs,xlim=c(0,1),ylim=c(0,1),xlab="S(ei): Kaplan-Meier",ylab="S(ei): Exponencial padrao",pch=16)
lines(c(0,1), c(0,1), type="l", lty=3)
plot(ekm2cs,conf.int=F,mark.time=F, xlab="Cox-Snell residuals", 
     ylab="Survival probability",xlim=c(0,5.5),main="Weibull")
lines(t2cs,Stw_cs,lty=2)
legend(-0.1,0.3,lty=c(1,2),c("Kaplan-Meier","Standard Exponential"),
       cex=0.6,bty="n")
#
# 3-Residuos do Modelo Log-normal 
#=================================
#Residuos de Cox-Snell
rcs3<- -log(stln);rcs3  # residuos cox-snell
ekm3cs<-survfit(Surv(rcs3,dados$OBITO)~1,data=dados)
t3cs<-ekm3cs$time;t3cs
stln_km<-ekm3cs$surv;stln_km
Stln_cs<-exp(-t3cs);Stln_cs   

# Log-normal padrao
par(mfrow=c(1,2))
plot(stln_km,Stln_cs,xlim=c(0,1),ylim=c(0,1),xlab="S(ei): Kaplan-Meier",ylab="S(ei): Exponencial padrao",pch=16)
lines(c(0,1), c(0,1), type="l", lty=3)
plot(ekm3cs,conf.int=F,mark.time=F, xlab="Cox-Snell residuals", 
     ylab="Survival probability",xlim=c(0,4),main="Log-normal")
lines(t3cs,Stln_cs,lty=2)
legend(-0.1,0.20,lty=c(1,2),c("Kaplan-Meier","Standard Exponential"),
       cex=0.6,bty="n")
#
# 4-Residuos do Modelo Log-logistico 
#=====================================
#Residuos de Cox-Snell
rcs4<- -log(stllog);rcs4  # residuos cox-snell
ekm4cs<-survfit(Surv(rcs4,dados$OBITO)~1,data=dados)
t4cs<-ekm4cs$time;t4cs
stllog_km<-ekm4cs$surv;stllog_km
Stllog_cs<-exp(-t4cs);Stllog_cs   
# Log-logistico padrao
par(mfrow=c(1,2))
plot(stllog_km,Stllog_cs,xlim=c(0,1),ylim=c(0,1),xlab="S(ei): Kaplan-Meier",ylab="S(ei): Exponencial padrao",pch=16)
lines(c(0,1), c(0,1), type="l", lty=3)
plot(ekm4cs,conf.int=F,mark.time=F, xlab="Cox-Snell residuals", 
     ylab="Survival probability",xlim=c(0,4),main="Log-logistic")
lines(t4cs,Stllog_cs,lty=2)
legend(-0.1,0.2,lty=c(1,2),c("Kaplan-Meier","Standard Exponential"),
       cex=0.6,bty="n")
#
# 5-Residuos do Modelo Logistico 
#=====================================
#Residuos de Cox-Snell
rcs5<- -log(stlog);rcs5  # residuos cox-snell
ekm5cs<-survfit(Surv(rcs5,dados$OBITO)~1,data=dados)
t5cs<-ekm5cs$time;t5cs
stlog_km<-ekm5cs$surv;stlog_km
Stlog_cs<-exp(-t5cs);Stlog_cs

# Graficos adequação do modelo
par(mfrow=c(1,2))
plot(stlog_km,Stlog_cs,xlim=c(0,1),ylim=c(0,1),xlab="S(ei): Kaplan-Meier",ylab="S(ei): Exponencial padrao",pch=16)
lines(c(0,1), c(0,1), type="l", lty=3)
plot(ekm5cs,conf.int=F,mark.time=F, xlab="Cox-Snell residuals", 
     ylab="Survival probability",xlim=c(0,3),main="Logistic")
lines(t5cs,Stlog_cs,lty=2)
legend(-0.1,0.3,lty=c(1,2),c("Kaplan-Meier","Standard Exponential"),
       cex=0.6,bty="n")
#
# 6-Residuos do Modelo Gaussiano 
#===============================================================================
#Residuos de Cox-Snell
rcs6<- -log(stgau);rcs6  # residuos cox-snell
ekm6cs<-survfit(Surv(rcs6,dados$OBITO)~1,data=dados)
t6cs<-ekm6cs$time;t6cs
stgau_km<-ekm6cs$surv;stgau_km
Stgau_cs<-exp(-t6cs);Stgau_cs   

# Gaussiano
par(mfrow=c(1,2))
plot(stgau_km,Stgau_cs,xlim=c(0,1),ylim=c(0,1),xlab="S(ei): Kaplan-Meier",ylab="S(ei): Exponencial padrao",pch=16)
lines(c(0,1), c(0,1), type="l", lty=3)
plot(ekm6cs,conf.int=F,mark.time=F, xlab="Cox-Snell residuals", 
     ylab="Survival probability",xlim=c(0,3.0),,main="Gaussian")
lines(t6cs,Stgau_cs,lty=2)
legend(-0.1,0.3,lty=c(1,2),c("Kaplan-Meier","Standard Exponential"),
       cex=0.6,bty="n")
#

###################################################################################

intercept_only<-survreg(Surv(dados$t, 
                             dados$e) ~ 1,
                        data = dados, dist = "lognorm")
summary(intercept_only)

#Selecao de variaveis 
forward <- step(ajust3, direction = "backward")
forward$anova
forward$coefficients

summary(ajust3)
table(dados$`Potencialidade agrícola`)
dados = subset(dados, dados$`Potencialidade agrícola` != 0)
# #===========================================================================================
# #COMPARAÇÃO DAS CONCENTRAÇÕES
# #
# #Sobrevivencia considerando o modelo Log-Logistico
# ajust4<-survreg(Surv(tempo,cens)~esp*des*conc,dist='loglogistic', data=dados)
# summary(ajust4)
# sigma<-ajust4$scale;sigma
# tempoE<-1:7;tempoE                # Tempo avaliado no experimento
# #
# #===========================
# #1-Seleção da ESP=1 e Des=1
# #===========================
# #TEM QUE NUMERAR NA FORMA DE FATOR (ESP=1->0 e Des=1->0)
# esp<-0
# des<-0
# #Conc=0
# conc<- 0
# xb1<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb1
# st11c1<- 1/(1+(tempoE/xb1)^(1/sigma));st11c1
# Ft11c1<-1-st11c1;Ft11c1
# #
# #Conc=0.005
# conc<-0.005
# xb2<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb2
# st11c2<- 1/(1+(tempoE/xb2)^(1/sigma));st11c2
# Ft11c2<-1-st11c2;Ft11c2
# #
# #Conc=0.050
# conc<-0.050
# xb3<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb3
# st11c3<- 1/(1+(tempoE/xb3)^(1/sigma));st11c3
# Ft11c3<-1-st11c3;Ft11c3
# #
# #Conc=0.100
# conc<-0.100 
# xb4<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb4
# st11c4<- 1/(1+(tempoE/xb4)^(1/sigma));st11c4
# Ft11c4<-1-st11c4;Ft11c4
# #
# #Conc=0.500
# conc<-0.500
# xb5<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb5
# st11c5<- 1/(1+(tempoE/xb5)^(1/sigma));st11c5
# Ft11c5<-1-st11c5;Ft11c5
# #
# #
# #==========================
# #2-Seleção da ESP=1 e Des=2
# #==========================
# #TEM QUE NUMERAR NA FORMA DE FATOR (ESP=1->0 e Des=2->1)
# esp<-0
# des<-1
# #Conc=0
# conc<- 0
# xb1<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb1
# st12c1<- 1/(1+(tempoE/xb1)^(1/sigma));st12c1
# Ft12c1<-1-st12c1;Ft12c1
# #
# #Conc=0.005
# conc<-0.005
# xb2<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb2
# st12c2<- 1/(1+(tempoE/xb2)^(1/sigma));st12c2
# Ft12c2<-1-st12c2;Ft12c2
# #
# #Conc=0.050
# conc<-0.050
# xb3<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb3
# st12c3<- 1/(1+(tempoE/xb3)^(1/sigma));st12c3
# Ft12c3<-1-st12c3;Ft12c3
# #
# #Conc=0.100
# conc<-0.100 
# xb4<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb4
# st12c4<- 1/(1+(tempoE/xb4)^(1/sigma));st12c4
# Ft12c4<-1-st12c4;Ft12c4
# #
# #Conc=0.500
# conc<-0.500
# xb5<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb5
# st12c5<- 1/(1+(tempoE/xb5)^(1/sigma));st12c5
# Ft12c5<-1-st12c5;Ft12c5
# #
# #
# #==========================
# #1-Seleção da ESP=2 e Des=1
# #==========================
# #TEM QUE NUMERAR NA FORMA DE FATOR (ESP=2->1 e Des=1->0)
# esp<-1
# des<-0
# #Conc=0
# conc<- 0
# xb1<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb1
# st21c1<- 1/(1+(tempoE/xb1)^(1/sigma));st21c1
# Ft21c1<-1-st21c1;Ft21c1
# #
# #Conc=0.005
# conc<-0.005
# xb2<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb2
# st21c2<- 1/(1+(tempoE/xb2)^(1/sigma));st21c2
# Ft21c2<-1-st21c2;Ft21c2
# #
# #Conc=0.050
# conc<-0.050
# xb3<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb3
# st21c3<- 1/(1+(tempoE/xb3)^(1/sigma));st21c3
# Ft21c3<-1-st21c3;Ft21c3
# #
# #Conc=0.100
# conc<-0.100 
# xb4<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb4
# st21c4<- 1/(1+(tempoE/xb4)^(1/sigma));st21c4
# Ft21c4<-1-st21c4;Ft21c4
# #
# #Conc=0.500
# conc<-0.500
# xb5<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb5
# st21c5<- 1/(1+(tempoE/xb5)^(1/sigma));st21c5
# Ft21c5<-1-st21c5;Ft21c5
# #
# #
# #===========================
# #1-Seleção da ESP=2 e Des=2
# #===========================
# #TEM QUE NUMERAR NA FORMA DE FATOR (ESP=2->1 e Des=2->1)
# esp<-1
# des<-1
# #Conc=0
# conc<- 0
# xb1<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb1
# st22c1<- 1/(1+(tempoE/xb1)^(1/sigma));st22c1
# Ft22c1<-1-st22c1;Ft22c1
# #
# #Conc=0.005
# conc<-0.005
# xb2<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb2
# st22c2<- 1/(1+(tempoE/xb2)^(1/sigma));st22c2
# Ft22c2<-1-st22c2;Ft22c2
# #
# #Conc=0.050
# conc<-0.050
# xb3<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb3
# st22c3<- 1/(1+(tempoE/xb3)^(1/sigma));st22c3
# Ft22c3<-1-st22c3;Ft22c3
# #
# #Conc=0.100
# conc<-0.100 
# xb4<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb4
# st22c4<- 1/(1+(tempoE/xb4)^(1/sigma));st22c4
# Ft22c4<-1-st22c4;Ft22c4
# #
# #Conc=0.500
# conc<-0.500
# xb5<-exp(ajust4$coefficients[1]+ajust4$coef[2]*esp+
#            ajust4$coefficients[3]*des+ajust4$coef[4]*conc+
#            ajust4$coefficients[5]*esp*des+ajust4$coef[6]*esp*conc+
#            ajust4$coefficients[7]*des*conc+ajust4$coef[8]*esp*des*conc);xb5
# st22c5<- 1/(1+(tempoE/xb5)^(1/sigma));st22c5
# Ft22c5<-1-st22c5;Ft22c5
# #
# #
# #=============================================================================
# #Estimação dos Quantis (Para objetos do SURVREG)
# #
# require(ciTools)
# #
# #Quantil 0.80 corresponde uma germinação de 80% e sobrevivência de 20%
# dados11<-subset(dados,esp==1 & des==1);dados11
# qq80<-add_quantile(dados11,ajust4, p =0.80, name = c("quant", "lwr", "upr"),alpha = 0.05);qq80
# unique(c(qq80$quant))
# #
# #CONSTRUCAO DOS GRÁFICOS
# # 
# #Para salvar a figura em 300dpi no formato tif
# #tiff("FigDoses-P1.tiff", width =3000, height =2150, units = 'px', res = 300)
# #
# #Grafico-1
# #
# par(mfrow=c(2,2))
# plot(tempoE, tempoE*0, pch=" ",xlim=range(c(1,7)),ylim=range(c(0,1.03)),
#      xlab="Time (days)",ylab="Seed germination probability",xaxt="n", yaxt="n")
# mtext(adj = 0,expression(italic("Vicia fava")))
# mtext(adj=0.40, " - bleach")
# axis(1, at = seq(0, 7, by = 1))
# axis(2, at = seq(0, 1, by = 0.1))
# lines(tempoE,Ft11c1, col=1,lwd =2.0) 
# lines(tempoE,Ft11c2, col=2,lwd =1.5) 
# lines(tempoE,Ft11c3, col=3,lwd =2.0)
# lines(tempoE,Ft11c4,col=4,lwd =2.0)
# lines(tempoE,Ft11c5,lty=3,col=5,lwd =3.0)
# legend(0.70,1.09,  lty=c(1,1,1,1,3),col=c(1,2,3,4,5),c("0.000%", "0.005%","0.050%","0.100%","0.500%"),
#        lwd=2, bty="n", cex=0.77) 
# text(7,1.03,"(a)",cex=0.9) 
# abline(h=c(0.80),lty=3,lwd=1)
# #
# #arrows(x0=7.49,y0=-0.04,x1=7.49,y1=0.75,angle=20,code = 1,length=0.15) 
# segments(x0=3.06,y0=-0.04,x1=3.06,y1=0.80,lty=3)
# text(2.84,0.30, expression(t["0.80"]==3.06),font=4,srt=90,cex =0.95)
# segments(x0=3.2,y0=-0.04,x1=3.2,y1=0.80,,lty=3)
# text(3.358,0.30, expression(t["0.80"]==3.11),font=4,srt=90,cex =0.95)
# segments(x0=3.7,y0=-0.04,x1=3.7,y1=0.80,,lty=3)
# text(3.85,0.30, expression(t["0.80"]=="3.60"),font=4,srt=90,cex =0.95)
# segments(x0=4.24,y0=-0.04,x1=4.24,y1=0.80,,lty=3)
# text(4.44,0.30, expression(t["0.80"]==4.22),font=4,srt=90,cex =0.95)
# #
# #
# #Grafico-2
# #
# #Quantil 0.80 corresponde uma germinação de 80% e sobrevivência de 20%
# dados12<-subset(dados,esp==1 & des==2);dados12
# qq80<-add_quantile(dados12,ajust4, p =0.80, name = c("quant", "lwr", "upr"),alpha = 0.05);qq80
# unique(c(qq80$quant))
# #
# plot(tempoE, tempoE*0, pch=" ",xlim=range(c(1,7)),ylim=range(c(0,1.03)),
#      xlab="Time (days)",ylab=" ",xaxt="n", yaxt="n")
# mtext(adj = 0,expression(italic("Vicia fava")))
# mtext(adj=0.41, " - vinegar")
# axis(1, at = seq(0, 7, by = 1))
# axis(2, at = seq(0, 1, by = 0.1))
# lines(tempoE,Ft12c1, col=1,lwd =2.0) 
# lines(tempoE,Ft12c2, col=2,lwd =2.0) 
# lines(tempoE,Ft12c3, col=3,lwd =2.0)
# lines(tempoE,Ft12c4,col=4,lwd =2.0)
# lines(tempoE,Ft12c5,lty=3,col=5,lwd =3.0)
# #legend(0.7,1.09, lty=c(1,1,1,1,3),col=c(1,2,3,4,5),c("0.000%", "0.005%","0.050%","0.100%","0.500%"),#       lwd=2, bty="n", cex=0.77) 
# text(7,1.03,"(b)",cex=0.9)  abline(h=c(0.80),lty=3,lwd=1)
# #
# # 
# segments(x0=3.06,y0=-0.04,x1=3.06,y1=0.80,lty=3)
# text(2.84,0.30, expression(t["0.80"]==3.06),font=4,srt=90,cex =0.95)
# segments(x0=3.3,y0=-0.04,x1=3.3,y1=0.80,,lty=3)
# text(3.5,0.30, expression(t["0.80"]==3.23),font=4,srt=90,cex =0.95)
# segments(x0=5.17,y0=-0.04,x1=5.17,y1=0.80,,lty=3)
# text(5.4,0.30, expression(t["0.80"]=="5.17"),font=4,srt=90,cex =0.95)
# #
# #
# #Grafico-3
# #
# #Quantil 0.80 corresponde uma germinação de 80% e sobrevivência de 20%
# dados21<-subset(dados,esp==2 & des==1);dados21
# qq80<-add_quantile(dados21,ajust4, p =0.80, name = c("quant", "lwr", "upr"),alpha = 0.05);qq80
# unique(c(qq80$quant))
# #
# plot(tempoE, tempoE*0, pch=" ",xlim=range(c(1,7)),ylim=range(c(0,1.03)),
#      xlab="Time (days)",ylab="Seed germination probability",xaxt="n", yaxt="n")
# mtext(adj = 0,expression(italic("Lens culinaris")))
# mtext(adj=0.54, " - bleach")
# axis(1, at = seq(0, 7, by = 1))
# axis(2, at = seq(0, 1, by = 0.1))
# lines(tempoE,Ft21c1, col=1,lwd =2.0) 
# lines(tempoE,Ft21c2, col=2,lwd =1.5) 
# lines(tempoE,Ft21c3, col=3,lwd =2.0)
# lines(tempoE,Ft21c4,col=4,lwd =2.0)
# lines(tempoE,Ft21c5,lty=3,col=5,lwd =3.0)
# legend(0.7,1.08,  lty=c(1,1,1,1,3),col=c(1,2,3,4,5),c("0.000%", "0.005%","0.050%","0.100%","0.500%"),
#        lwd=2, bty="n", cex=0.77) 
# text(7,1.03,"(c)",cex=0.9) 
# abline(h=c(0.80),lty=3,lwd=1)
# #
# # 
# segments(x0=3.10,y0=-0.04,=3.10,y1=0.80,lty=3)
# text(2.87,0.30, expression(t["0.80"]==3.07),font=4,srt=90,cex =0.95)
# segments(x0=3.2,y0=-0.04,x1=3.2,y1=0.80,,lty=3)
# text(3.39,0.30, expression(t["0.80"]==3.15),font=4,srt=90,cex =0.95)
# segments(x0=3.94,y0=-0.04,x1=3.94,y1=0.80,,lty=3)
# text(4.145,0.30, expression(t["0.80"]=="3.94"),font=4,srt=90,cex =0.95)
# segments(x0=5.04,y0=-0.04,x1=5.04,y1=0.80,,lty=3)
# text(5.25,0.30, expression(t["0.80"]==5.04),font=4,srt=90,cex =0.95)
# #
# #
# #Grafico-4
# #
# #Quantil 0.80 corresponde uma germinação de 80% e sobrevivência de 20%
# dados22<-subset(dados,esp==2 & des==2);dados22
# qq80<-add_quantile(dados22,ajust4, p =0.80, name = c("quant", "lwr", "upr"),alpha = 0.05);qq80
# unique(c(qq80$quant))
# #
# plot(tempoE, tempoE*0, pch=" ",xlim=range(c(1,7)),ylim=range(c(0,1.03)),
#      xlab="Time (days)",ylab="",xaxt="n", yaxt="n")
# mtext(adj = 0,expression(italic("Lens culinaris")))
# mtext(adj=0.55, " - vinegar")
# axis(1, at = seq(0, 7, by = 1))
# axis(2, at = seq(0, 1, by = 0.1))
# lines(tempoE,Ft22c1, col=1,lwd =2.0) 
# lines(tempoE,Ft22c2, col=2,lwd =2.0) 
# lines(tempoE,Ft22c3, col=3,lwd =2.0)
# lines(tempoE,Ft22c4,col=4,lwd =2.0)
# lines(tempoE,Ft22c5,lty=3,col=5,lwd =3.0)
# #legend(0.8,1.05,lty=c(1,1,1,1,3),col=c(1,2,3,4,5),c("0 %", "0.005 %","0.050 %","0.100 %","0.500 %"),
# #       lwd=2, bty="n", cex=0.77) 
# text(7,1.03,"(d)",cex=0.9) 
# abline(h=c(0.80),lty=3,lwd=1)
# #
# # 
# segments(x0=3.05,y0=-0.04,x1=3.05,y1=0.80,lty=3)
# text(2.81,0.30, expression(t["0.80"]==3.04),font=4,srt=90,cex =0.95)
# segments(x0=3.70,y0=-0.04,x1=3.70,y1=0.80,,lty=3)
# text(3.88,0.30, expression(t["0.80"]==3.15),font=4,srt=90,cex =0.95)
# #dev.off()
# #
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 



