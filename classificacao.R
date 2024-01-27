#####  LIMPANDO OBJETOS SALVOS  #####

rm(list=ls(all=TRUE))


#####  PACOTES  #####

library(tidyverse)
library(tidymodels)
library(stacks)
library(textrecipes)
library(cutpointr)
library(tictoc)
library(caret)
library(plsmod)


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


##### SPLIT TRAIN/TEST/VALIDATION #####

set.seed(0)
split<- df %>% initial_split(strata=Heart.Attack.Risk)

df.train<- training(split)
df.test<- testing(split)

folds<- vfold_cv(df.train, v=3, strata=Heart.Attack.Risk)



##### PRÉ-PROCESSAMENTO #####

#recipe<- recipe(Heart.Attack.Risk ~ . , data = df.train) %>%
  #step_clean_names(all_predictors()) %>% 
  #step_clean_levels(all_nominal()) %>% 

rec<- recipe(Heart.Attack.Risk ~ . , data = df.train) %>%
  step_filter_missing(all_predictors(),threshold = 0.4) %>% 
  step_zv(all_predictors()) %>% 
  step_impute_knn(all_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())

rec.knn<- rec %>% 
  #step_pls(all_numeric_predictors(),num_comp = tune(),outcome = "Heart.Attack.Risk") 
  #step_pls(all_numeric_predictors(),num_comp = 2,outcome = "Heart.Attack.Risk") 
  step_pca(all_numeric_predictors(),num_comp = tune())



##### MODELOS #####

fit.las<- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

fit.knn<- nearest_neighbor(neighbors = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

fit.dt<- decision_tree(cost_complexity = tune(),
                       tree_depth = tune(),
                       min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")



##### WORKFLOW #####

wf.las<- workflow() %>%
  add_recipe(rec) %>%
  add_model(fit.las)

wf.knn<- workflow() %>%
  add_recipe(rec.knn) %>%
  add_model(fit.knn)

wf.dt<- workflow() %>%
  add_recipe(rec) %>%
  add_model(fit.dt)



##### HIPERPARAMETERS TUNING - BAYESIAN SEARCH #####

tic()
set.seed(0)
tune.las<- tune_bayes(wf.las,
                      resamples = folds,
                      initial = 10,
                      control = control_stack_bayes(),
                      metrics = metric_set(roc_auc),
                      param_info = parameters(penalty(range=c(-10,0)))
)
toc()
# 43.72 sec elapsed


tic()
set.seed(0)
tune.knn<- tune_bayes(wf.knn,
                      resamples = folds,
                      initial = 5,
                      control = control_stack_bayes(),
                      metrics = metric_set(roc_auc),
                      param_info = parameters(num_comp(range=c(1,20)),
                                              neighbors(range=c(1,40)))
)
toc()
# 92.81 sec elapsed


tic()
set.seed(0)
tune.dt<- tune_bayes(wf.dt,
                      resamples = folds,
                      initial = 10,
                      control = control_stack_bayes(),
                      metrics = metric_set(roc_auc),
                      param_info = parameters(cost_complexity(range=c(-10,-1)),
                                              tree_depth(range=c(1,20)),
                                              min_n(range=c(1,40)))
)
toc()
# 92.67 sec elapsed




## ESCOLHENDO O MELHOR (BEST roc_auc)

show_best(tune.las,n=3)
show_best(tune.knn,n=3)
show_best(tune.dt,n=3)



##### TUNED WORKFLOW #####

wf.las<- wf.las %>% 
  finalize_workflow(select_best(tune.las)) %>% 
  fit(df.train)

wf.knn<- wf.knn %>% 
  finalize_workflow(select_best(tune.knn)) %>% 
  fit(df.train)

wf.dt<- wf.dt %>% 
  finalize_workflow(select_best(tune.dt)) %>% 
  fit(df.train)



######################################################
#####  ESCOLHENDO O PONTO DE CORTE - SENS/ESPEC  #####
######################################################

# RESPOSTA VS PROBABILIDADE

prob.test<- wf.dt %>% predict(df.test, type="prob")
df.prob<- cbind.data.frame(df.test$Heart.Attack.Risk, prob.test[,2])
colnames(df.prob)<- c("Heart.Attack.Risk", "prob")

df.prob %>% head()


# CORTE

cut<- df.prob %>% cutpointr(prob, Heart.Attack.Risk,
                            method=maximize_metric,
                            metric=sum_sens_spec,
                            pos_class="1",
                            direction=">=")

cut<- df.prob %>% cutpointr(prob, Heart.Attack.Risk,
                            method=minimize_metric,
                            metric=abs_d_sens_spec,
                            pos_class="1",
                            direction=">=")

#cut<- df.prob %>% cutpointr(prob, Heart.Attack.Risk,
#                            method=maximize_metric,
#                            metric=spec_constrain,
#                            min_constrain = 0.75,
#                            pos_class="1",
#                            direction=">=")

cut %>% summary()
cut %>% plot_roc()

opt_cut<- cut$optimal_cutpoint

pred<- cut %>% predict(newdata=df.prob)

df.prob<- cbind.data.frame(df.prob, pred)

df.prob %>% head()




#####  VERIFICANDO MEDIDAS DE CLASSIFICAÇÃO  #####

df.prob %>% conf_mat(Heart.Attack.Risk, pred)
df.prob %>% conf_mat(Heart.Attack.Risk, pred) %>% autoplot(type="heatmap")
df.prob %>% conf_mat(Heart.Attack.Risk, pred) %>% summary()




############################
### FINALIZANDO O MODELO ###
############################

wf.final<- fit(wf.best, df)



###############################
### SALVANDO O MODELO FINAL ###
###############################

saveRDS(wf.final,"C:/Users/user/Desktop/pasta1/wf_german_credit.rds")
saveRDS(opt_cut,"C:/Users/user/Desktop/pasta1/ponto_corte.rds")

