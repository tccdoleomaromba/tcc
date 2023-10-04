install.packages("readxl", dependencies = T)
install.packages("knitr", dependencies = T)
install.packages("plm", dependencies = T)
install.packages("corrplot", dependencies = T)
install.packages("stargazer", dependencies = T)
install.packages("gplots", dependencies = T)
install.packages("tseries", dependencies = T)
install.packages("lmtest", dependencies = T)
install.packages("urca", dependencies = T)
install.packages("car", dependencies = T)
install.packages("sandwich", dependencies = T)


##Puxar dados####
library(readxl)
dados_RANDOM <- read_xlsx("dados_TCC.xlsx", 
                           sheet = "Planilha R", range = "A1:N712")
#View(dados_RANDOM)
dados_R <- na.omit(dados_RANDOM)
summary(dados_R)



#View(dados_R)

dados_R <-dados_R[,-1]  #retirando a coluna dos nomes

##Organizando o dataset##
##Estatísticas descritivas originais##
# Descriptive statistics
library(knitr)
kable(summary(dados_R, caption = "Estatísticas descritivas"))



#Retirando NAs, o painel ficará desbalanceado conforme se verá na função pdim
library(plm)

summary(dados_R)



#View(dados_R)

cor(dados_R[,4:13])


library(corrplot)
corel <- cor(dados_R[,5:13]) # retirei a var dependente
corrplot(corel, method = "number") # ok


panel.teste <- pdata.frame(dados_R, index = c("i", "t"))
#View(panel.teste)
summary(panel.teste)



pdim(panel.teste)  # Unbalanced Panel

attach(panel.teste)
##########
#
## Modelo empilhado - pooling #####
#modelo pooling, whithin e between para os modelos empilhado

pooling <- plm(tx.pcarpitacorreta ~ pib2012pcarpita + Saúde + Educação + Administração + 
                 Custeio+ Seguridade.Social, data = panel.teste, index = c("i", "t"), 
               model = "pooling")
summary(pooling)



car::vif(pooling) # ver se Há evidencias para colinearidade!

#aic e bic
source("aicbic_plm.R")
aicbic_plm(pooling, "AIC")
pooling$AIC <- aicbic_plm(pooling, "AIC")
pooling$BIC <- aicbic_plm(pooling, "BIC")
suppressMessages(library(stargazer))
stargazer(pooling, title = "Título: Resultado da Regressão Pooling", align = TRUE, 
          type = "text", style = "all", keep.stat = c("AIC", "BIC", "rsq", "adj.rsq", 
                                                      "n"))

# primeiro gráfico com os 79 municipios
# plot do MQO - pooling
#dados_R$logpibpc2002<-log(dados_R$pibpc2002) # ajustar ae, a pesquisa é sua

ols <- lm(tx.pcarpitacorreta ~  pib2012pcarpita + Saúde + Educação + Administração + 
            Custeio+ Seguridade.Social, data = dados_R)
summary(ols)


yhat <- ols$fitted

plot(dados_R$pib2012pcarpita, dados_R$`tx pcarpitacorreta`, pch = 19, 
     xlab = "pib2012pcarpita", ylab = "tx.pcarpitacorreta")
abline(lm(dados_R$`tx pcarpitacorreta` ~ dados_R$pib2012pcarpita), lwd = 3, col = "red")



library(gplots)

# Primeiro, crie o gráfico suprimindo o eixo X
plotmeans(dados_R$`tx pcarpitacorreta` ~ i, main = "Heterogeneidade entre municipios", data = panel.teste, xaxt = "n")

# Agora, adicione o eixo X com a escala desejada
axis(1, at=seq(1, 80, by=10), labels=seq(10, 80, by=10))


plotmeans(dados_R$`tx pcarpitacorreta` ~ t, main = "Heterogeneidade entre anos", data = panel.teste)


## Modelo between ####

modelo.between <- plm(tx.pcarpitacorreta ~ pib2012pcarpita + Saúde + Educação + Administração + 
                        Custeio+ Seguridade.Social, data = panel.teste, index = c("i", "t"), 
                      model = "between")

summary(modelo.between)



# fazer pela rotina abaixo para gerar aic e bic do between
sp = summary(modelo.between)
u.hat <- residuals(sp) # extract residuals
df <- cbind(as.vector(u.hat), attr(u.hat, "index"))
names(df) <- c("resid")
#c = length(levels(df$Country)) # extract country dimension 
#t = length(levels(df$Time)) # extract time dimension 
np = length(sp$coefficients[,1]) # number of parameters
n.N = nrow(sp$model) # number of data
s.sq  <- log( (sum(u.hat^2)/(n.N))) # log sum of squares

# effect = c("individual", "time", "twoways", "nested"),
# model = c("within", "random", "ht", "between", "pooling", "fd")

aic <- round(       2*np  +  n.N * (  log(2*pi) + s.sq  + 1 ),1)
bic <- round(log(n.N)*np  +  n.N * (  log(2*pi) + s.sq  + 1 ),1)

print(c("AIC=",aic))


print(c("BIC=",bic))


modelo.between$AIC <- aic
modelo.between$BIC <- bic
#suppressMessages(library(stargazer))
stargazer(modelo.between,pooling,
          title = "Título: Resultados das Regressões Between e Pooling", 
          align = TRUE, 
          type = "text", style = "all", 
          keep.stat = c("AIC", "BIC", "rsq", "adj.rsq","n"))


plmtest(pooling)





#Efeitos Fixos
#Modelo Within
#Oneway (individual)
Modelo.Within.ind <- plm(tx.pcarpitacorreta ~ pib2012pcarpita + Saúde + Educação + Administração + 
                           Custeio+ Seguridade.Social, data = panel.teste, index = c("i", "t"), 
                         model = "within",
                         effect = c("individual"))
summary(Modelo.Within.ind)


source("aicbic_plm.R")
Modelo.Within.ind$AIC <- aicbic_plm(Modelo.Within.ind, "AIC")
Modelo.Within.ind$BIC <- aicbic_plm(Modelo.Within.ind, "BIC")

#Oneway (time)
Modelo.Within.time <- plm(tx.pcarpitacorreta ~ pib2012pcarpita + Saúde + Educação + Administração + 
                            Custeio + Seguridade.Social, data = panel.teste, index = c("i", "t"), 
                          model = "within",
                          effect = c("time"))
summary(Modelo.Within.time)


source("aicbic_plm.R")
Modelo.Within.time$AIC <- aicbic_plm(Modelo.Within.time, "AIC")
Modelo.Within.time$BIC <- aicbic_plm(Modelo.Within.time, "BIC")


#Twoways (individual e time)
Modelo.Within.two <- plm(tx.pcarpitacorreta ~ pib2012pcarpita + Saúde + Educação + Administração + 
                           Custeio+ Seguridade.Social, data = panel.teste, index = c("i", "t"), 
                         model = "within",
                         effect = c("twoways"))
summary(Modelo.Within.two)


source("aicbic_plm.R")
Modelo.Within.two$AIC <- aicbic_plm(Modelo.Within.two, "AIC")
Modelo.Within.two$BIC <- aicbic_plm(Modelo.Within.two, "BIC")

#Resumo da seção (EF)
suppressMessages(library(stargazer))
stargazer(Modelo.Within.ind,Modelo.Within.time, 
          Modelo.Within.two,title = "Título: Resultados das Regressões Efeitos Fixos", 
          align = TRUE, 
          type = "text", style = "all", 
          keep.stat = c("AIC", "BIC", "rsq", "adj.rsq","n"))

#Efeitos aleatórios
formula <- tx.pcarpitacorreta ~ pib2012pcarpita + Saúde + Educação + Administração + 
  Custeio+ Seguridade.Social


#Oneway (individual) effect Random Effect Model (Swamy-Arora’s transformation - default)
random.ind.SA <- plm(formula = formula, 
                     data = panel.teste, 
                     effect = c("individual"), 
                     model = "random")
summary(random.ind.SA)


#Oneway (time) effect Random Effect Model (Swamy-Arora’s transformation - default)
random.time.SA <- plm(formula = formula, 
                      data = panel.teste, 
                      effect = c("time"), 
                      model = "random")
summary(random.time.SA)


#Twoways (time and individual) effect Random Effect Model (Swamy-Arora’s transformation - default)
random.two.SA <- plm(formula = formula, 
                     data = panel.teste, 
                     effect = c("twoways"), 
                     model = "random")
summary(random.two.SA)



#Random Effect Model (Amemiya’s transformation)
#Oneway (individual) effect Random Effect Model (Amemiya’s transformation)
random.ind.Am <- plm(formula = formula, 
                     data = panel.teste, 
                     effect = c("individual"), 
                     model = "random", 
                     random.method = "amemiya")
summary(random.ind.Am)




#Oneway (time) effect Random Effect Model (Amemiya’s transformation)
random.time.Am <- plm(formula = formula, 
                      data = panel.teste, 
                      effect = c("time"), 
                      model = "random", 
                      random.method = "amemiya")
summary(random.time.Am)



#Twoways (time and individual) effect Random Effect Model (Amemiya’s transformation)
random.two.Am <- plm(formula = formula, 
                     data = panel.teste, 
                     effect = c("twoways"), 
                     model = "random", 
                     random.method = "amemiya")
summary(random.two.Am)



#Random Effect Model (Wallace-Hussain’s transformation)
#Oneway (individual) effect Random Effect Model (Wallace-Hussain’s transformation)
random.ind.WH <- plm(formula = formula, 
                     data = panel.teste, 
                     effect = c("individual"), 
                     model = "random", 
                     random.method = "walhus")
summary(random.ind.WH)



#Oneway (time) effect Random Effect Model (Wallace-Hussain’s transformation)
random.time.WH <- plm(formula = formula, 
                      data = panel.teste, 
                      effect = c("time"), 
                      model = "random", 
                      random.method = "walhus")
summary(random.time.WH)


#Twoways (time and individual) effect Random Effect Model (Wallace-Hussain’s transformation)
random.two.WH <- plm(formula = formula, 
                     data = panel.teste, 
                     effect = c("twoways"), 
                     model = "random", 
                     random.method = "walhus")
summary(random.two.WH)



#Random Effect Model (Nerlove’s transformation)
#Oneway (individual) effect Random Effect Model (Nerlove’s transformation)
random.ind.Ne <- plm(formula = formula, 
                     data = panel.teste, 
                     effect = c("individual"), 
                     model = "random", 
                     random.method = "nerlove")
summary(random.ind.Ne)



#Oneway (time) effect Random Effect Model (Nerlove’s transformation)
random.time.Ne <- plm(formula = formula, 
                      data = panel.teste, 
                      effect = c("time"), 
                      model = "random", 
                      random.method = "nerlove")
summary(random.time.Ne)


#Twoways (time and individual) effect Random Effect Model (Nerlove’s transformation)
random.two.Ne <- plm(formula = formula, 
                     data = panel.teste, 
                     effect = c("twoways"), 
                     model = "random", 
                     random.method = "nerlove")
summary(random.two.Ne)



#Resumo de Random effects


source("aicbic_plm.R")
(random.time.SA$AIC <- aicbic_plm(random.time.SA, "AIC"))

(random.time.SA$BIC <- aicbic_plm(random.time.SA, "BIC"))

(random.time.Am$AIC <- aicbic_plm(random.time.Am, "AIC"))

(random.time.Am$BIC <- aicbic_plm(random.time.Am, "BIC"))

(random.time.WH$AIC <- aicbic_plm(random.time.WH, "AIC"))

(random.time.WH$BIC <- aicbic_plm(random.time.WH, "BIC"))

(random.time.Ne$AIC <- aicbic_plm(random.time.Ne, "AIC"))

(random.time.Ne$BIC <- aicbic_plm(random.time.Ne, "BIC"))


suppressMessages(library(stargazer))
stargazer(random.time.SA,random.time.Am,random.time.WH,random.time.Ne,
          title = "Título: Resultados das Regressões Efeitos Aleatórios", 
          align = TRUE, 
          column.labels = c("Swamy-Arora", "Amemiya","Wallace-Hussain", "Nerlove"),
          type = "text", style = "all", 
          keep.stat = c("AIC", "BIC", "rsq", "adj.rsq","n"))


#Polled x Efeitos fixos (LM test for fixed effects versus OLS)


pFtest(Modelo.Within.time, pooling)


#Hausmann (Hausman test for fixed versus random effects model
phtest(random.time.Ne, Modelo.Within.time)


#Teste autocorrelação

pbgtest(random.time.Ne)


#Teste JB para verificar a normalidade dos residuos
library(tseries)

library(lmtest)

jarque.bera.test(resid(random.time.Ne))


#Teste de heterocedasticidade

bptest(random.time.Ne)


bptest(random.time.Ne, studentize = F)



#Teste de Autocorrelação dependência da seção transversal em painéis - Breusch-Pagan LM
pcdtest(random.time.Ne)


pcdtest(random.time.Ne, test = c("cd"))

pcdtest(random.time.Ne, test = c("lm"))


#Teste de raiz unitária - estacionariedade

library(tseries)
library(urca)

adf.test(panel.teste$tx.pcarpitacorreta, k=2)


residuos<-residuals(random.time.Ne)
adf.test(residuos, k=2)


#Teste para correlação serial - Durbin Watson

pdwtest(random.time.Ne)


pdwtest(random.time.Ne, type ="HC1")

#Correção de Erros robustos, corrigindo para Driscoll e Kraay

#Teste do método Arellano

library(car)
library(sandwich)


(coeftest(random.time.Ne))


#Modelo final
# analisar os testes para ver qual o melhor modelo para fazer a nalise e editar aqui
# para montar o modelo final

# fazer stargazer com dois modelos
cov <- vcovSCC(------, type ="HC1", maxlag=4)
vif(-------)
robust.se <- sqrt(diag(cov))

stargazer(--------, <-repete o do anterior, se = list(NULL, robust.se), column.labels = c("Nerlove", 
                                                                                        "Nerlove robusto"), title = "Título: Resultados das Regressões de Efeitos Aleatórios", align = TRUE, 
          type = "text", style = "all", keep.stat = c("aic", "bic", "rsq", "adj.rsq", 
                                                      "n"))
coeftest(random.time.Ne, vcov. = function(x) vcovSCC(x, type ="HC1", maxlag=4))


source("aicbic_plm.R")
(random.time.Ne$AIC <- aicbic_plm(random.time.Ne, "AIC"))

(random.time.Ne$BIC <- aicbic_plm(random.time.Ne, "BIC"))

random.time.NeArellano <- plm(formula = formula, 
                              data = panel.teste, 
                              effect = c("time"), 
                              model = "random", 
                              random.method = "nerlove",
                              vcov. = function(x) vcovSCC(x, type ="HC1", maxlag=4))
summary(random.time.NeArellano)

source("aicbic_plm.R")
(random.time.NeArellano$AIC <- aicbic_plm(random.time.NeArellano, "AIC"))

(random.time.NeArellano$BIC <- aicbic_plm(random.time.NeArellano, "BIC"))

suppressMessages(library(stargazer))
stargazer(random.time.Ne,random.time.NeArellano,
          title = "Título: Resultados das Regressões Efeitos Aleatórios", 
          align = TRUE, 
          column.labels = c("Nerlove","Nerlove-Arellano"),
          type = "text", style = "all", 
          keep.stat = c("AIC", "BIC", "rsq", "adj.rsq","n"))

