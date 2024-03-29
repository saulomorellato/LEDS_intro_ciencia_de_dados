## CLEANING THE MEMORY ##

rm(list=ls(all=TRUE))



## REQUIRING PACKAGES ##

library(GGally)  	# correlation matrix graphic
library(MASS)		# stepwise (aic)
library(mixlm)		# stepwise (valor-p)
library(glmulti)	# all regression
library(glmtoolbox)   	# glm graphics
library(tidyverse)	# manipulacao de dados
library(ecostats)   	# envelope
library(hnp)  		# envelope versao 2
library(tictoc)   # medir tempo de execução
library(gtsummary)	# tabelas
library(report)		# reportar resultados



#####  CARREGAR OS DADOS  #####

df<- read.csv("heart_attack.csv",header=T)
glimpse(df)

df<- df %>% separate(Blood.Pressure, c("Systolic","Diastolic"), sep="/")
df$Heart.Attack.Risk<- as.factor(df$Heart.Attack.Risk)
df<- df %>% select(-Patient.ID)
glimpse(df)

df$Systolic<- as.numeric(df$Systolic)
df$Diastolic<- as.numeric(df$Diastolic)
glimpse(df)




## GAPHICS/CORRELATION ##

#ggpairs(df, columns = 3:4, ggplot2::aes(colour=gender))
#ggpairs(df, columns = 3:4, ggplot2::aes(colour=res_inf))



##############
## MODELO 1 ##
##############

model_all<- glm(Heart.Attack.Risk ~ . , family = binomial(link="logit"), data=df)
model_all %>% summary()



##############
## MODELO 2 ##
##############

## VARIABLE SELECTION - STEP ##

tic()
model_step<- stepAIC(model_all, direction = "both")
toc()
# 39.3 sec elapsed

model_step %>% summary()



##############
## MODELO 3 ##
##############

## VARIABLE SELECTION - ALL REGRESSIONS ##

tic()
set.seed(0)
opt_model_all_reg<- glmulti(Heart.Attack.Risk ~ . ,
                            data = df,
                            crit = aic, # aic, aicc, bic, bicc
                            #level = 1, # 1 sem interacoes, 2 com
                            #method = "h", # "d", ou "h", ou "g"
                            level = 1, # 1 sem interacoes, 2 com
                            method = "g", # "d", ou "h", ou "g"
                            family = binomial,
                            fitfunction = glm, # tipo de modelo (lm, glm, etc)
                            confsetsize = 100 # guarde os melhores 100
)
toc()
# 890.36 sec elapsed

formula_bestmodel<- summary(opt_model_all_reg)$bestmodel
model_all_reg<- glm(formula_bestmodel, family = binomial(link="logit"), data=df)

model_all_reg %>% summary()



### PREPARANDO OS GRÁFICOS ###

# escolha o "melhor"
model<- model_step

n<- nrow(df)    		# número de observações
k<- length(model$coef) 		# k=p+1 (número de coeficientes)

corte.hii<- 2*k/n		# corte para elementos da diagonal de H
corte.cook<- qf(0.5,k,n-k)	# corte para Distância de Cook

hii<- hatvalues(model) 		# valores da diagonal da matriz H
dcook<- cooks.distance(model)	# distância de Cook

obs<- 1:n

df.fit<- data.frame(obs,hii,dcook)




# GRÁFICO - ALAVANCAGEM

df.fit %>% ggplot(aes(x=obs,y=hii,ymin=0,ymax=hii)) + 
  geom_point() + 
  geom_linerange() + 
  geom_hline(yintercept = corte.hii, color="red", linetype="dashed") + 
  xlab("Observação") + 
  ylab("Alavancagem") + 
  theme_bw()



# GRÁFICO - DISTÂNCIA DE COOK

df.fit %>% ggplot(aes(x=obs,y=dcook,ymin=0,ymax=dcook)) + 
  geom_point() + 
  geom_linerange() +
  geom_hline(yintercept = corte.cook, color="red", linetype="dashed") + 
  xlab("Observação") + 
  ylab("Distância de Cook") + 
  theme_bw()



# ENVELOPE

env<- plotenvelope(model, which=2,
                   n.sim=1000,
                   conf.level=0.95,
                   plot.it=FALSE) 

env[[2]]$p.value    # H0: modelo correto vs. H1: modelo incorreto

df.env<- data.frame(obs, env[[2]]$x, env[[2]]$y, env[[2]]$lo, env[[2]]$hi)
colnames(df.env)<- c("obs", "x", "y", "lo", "hi")


df.env %>% ggplot(aes(x=x,y=y)) + 
  geom_point() + 
  geom_line(aes(x=x,y=lo), linetype="dashed") +
  geom_line(aes(x=x,y=hi), linetype="dashed") +
  xlab("Resíduos Simulados") + 
  ylab("Resíduos Observados") + 
  theme_bw()



# ENVELOPE 2


env2<- model %>% hnp(sim=1000,
                     halfnormal=FALSE,
                     plot.sim=FALSE)

df.env2<- data.frame(obs, env2$x, env2$residuals, env2$median, env2$lower, env2$upper)
colnames(df.env2)<- c("obs", "x", "y", "md", "lo", "hi")


df.env2 %>% ggplot(aes(x=x,y=y)) + 
  geom_point() + 
  geom_line(aes(x=x,y=md), linetype="dashed") +
  geom_line(aes(x=x,y=lo), linetype="dashed") +
  geom_line(aes(x=x,y=hi), linetype="dashed") +
  xlab("Resíduos Simulados") + 
  ylab("Resíduos Observados") + 
  theme_bw()



###########################
## REPORTANDO RESULTADOS ##
###########################

# TABELAS

model %>%  tbl_regression(exponentiate = TRUE,
                          estimate_fun = function(x) style_ratio(x, digits = 6))

model %>%  tbl_regression(estimate_fun = function(x) style_sigfig(x, digits = 6))


# RELATÓRIO

model %>% report()

model %>% report() %>% summary()
