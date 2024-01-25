#####  LIMPANDO OBJETOS SALVOS  #####

rm(list=ls(all=TRUE))


#####  PACOTES  #####

library(tidyverse)
library(tidymodels)
library(textrecipes)



#####  CARREGAR OS DADOS  #####

df<- read.csv("heart_attack.csv",header=T)
#df2<- read.csv("heart_attack2.csv",header=T,sep=";")

#df<- df %>% separate(Blood.Pressure, c("Systolic","Diastolic"), sep="/")

glimpse(df)



##### SPLIT TRAIN/TEST/VALIDATION #####

set.seed(0)
split<- df %>% initial_split(strata=Heart.Attack.Risk)

df.train<- training(split)
df.test<- testing(split)

folds<- vfold_cv(df.train, v=2, repeats=5, strata=Heart.Attack.Risk)



##### PRÉ-PROCESSAMENTO #####

recipe<- recipe(Heart.Attack.Risk ~ . , data = df.train) %>%
  step_clean_names(all_predictors()) %>% 
  step_clean_levels(all_nominal)

  
  step_filter_missing(all_predictors(),threshold = 0.4) %>% 
  step_zv(all_predictors()) %>% 
  step_impute_knn(all_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>% 
  #  step_string2factor(Class,levels=c("Good","Bad")) %>% 
  step_dummy(all_nominal_predictors())
#step_other(all_nominal_predictors(),threshold = 0.02)

recipe2<- recipe(Class ~ . , data = df.train) %>%
  step_filter_missing(all_predictors(),threshold = 0.4) %>% 
  step_zv(all_predictors()) %>% 
  step_impute_knn(all_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(terms = ~ all_predictors()^2) %>% 
  step_pls(all_predictors(), outcome="Class", num_comp = tune())

recipe3<- recipe(Class ~ . , data = df.train) %>%
  step_filter_missing(all_predictors(),threshold = 0.4) %>% 
  step_zv(all_predictors()) %>% 
  step_impute_knn(all_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_pls(all_predictors(), outcome="Class", num_comp = 2) %>% 
  step_interact(terms = ~ all_predictors()^2) %>% 
  step_spline_b(all_predictors(), deg_free = 10)



##### MODELOS #####

fit.log.las<- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

fit.log.rid<- logistic_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

fit.log.net<- logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("classification")



##### WORKFLOW #####

wf.las<- workflow() %>%
  add_recipe(recipe1) %>%
  add_model(fit.log.las)

wf.rid<- workflow() %>%
  add_recipe(recipe1) %>%
  add_model(fit.log.rid)

wf.net<- workflow() %>%
  add_recipe(recipe1) %>%
  add_model(fit.log.net)

wf.las2<- workflow() %>%
  add_recipe(recipe2) %>%
  add_model(fit.log.las)

wf.rid2<- workflow() %>%
  add_recipe(recipe2) %>%
  add_model(fit.log.rid)

wf.net2<- workflow() %>%
  add_recipe(recipe2) %>%
  add_model(fit.log.net)

wf.las3<- workflow() %>%
  add_recipe(recipe3) %>%
  add_model(fit.log.las)

wf.rid3<- workflow() %>%
  add_recipe(recipe3) %>%
  add_model(fit.log.rid)

wf.net3<- workflow() %>%
  add_recipe(recipe3) %>%
  add_model(fit.log.net)

##### HIPERPARAMETERS TUNING - BAYESIAN SEARCH #####

tic()
set.seed(1)
tune.las<- tune_bayes(wf.las,
                      resamples = folds,
                      initial = 10,
                      control = control_stack_bayes(),
                      metrics = metric_set(roc_auc),
                      param_info = parameters(penalty(range=c(-10,0)))
)
toc()
# 56.54 sec elapsed


tic()
set.seed(1)
tune.rid<- tune_bayes(wf.rid,
                      resamples = folds,
                      initial = 10,
                      control = control_stack_bayes(),
                      metrics = metric_set(roc_auc),
                      param_info = parameters(penalty(range=c(-10,0)))
)
toc()
# 57.77 sec elapsed


tic()
set.seed(1)
tune.net<- tune_bayes(wf.net,
                      resamples = folds,
                      initial = 10,
                      control = control_stack_bayes(),
                      metrics = metric_set(roc_auc),
                      param_info = parameters(penalty(range=c(-10,0)),
                                              mixture(range=c(0,1))
                      )
)
toc()
# 70.92 sec elapsed


tic()
set.seed(1)
tune.las2<- tune_bayes(wf.las2,
                       resamples = folds,
                       initial = 10,
                       control = control_stack_bayes(),
                       metrics = metric_set(roc_auc),
                       param_info = parameters(penalty(range=c(-10,0)),
                                               num_comp(range=c(1,50)))
)
toc()
# 626.81 sec elapsed


tic()
set.seed(1)
tune.rid2<- tune_bayes(wf.rid2,
                       resamples = folds,
                       initial = 10,
                       control = control_stack_bayes(),
                       metrics = metric_set(roc_auc),
                       param_info = parameters(penalty(range=c(-10,0)),
                                               num_comp(range=c(1,50)))
)
toc()
# 504.58 sec elapsed


tic()
set.seed(1)
tune.net2<- tune_bayes(wf.net2,
                       resamples = folds,
                       initial = 10,
                       control = control_stack_bayes(),
                       metrics = metric_set(roc_auc),
                       param_info = parameters(penalty(range=c(-10,0)),
                                               mixture(range=c(0,1)),
                                               num_comp(range=c(1,50))
                       )
)
toc()
# 653.34 sec elapsed


tic()
set.seed(1)
tune.las3<- tune_bayes(wf.las3,
                       resamples = folds,
                       initial = 10,
                       control = control_stack_bayes(),
                       metrics = metric_set(roc_auc),
                       param_info = parameters(penalty(range=c(-10,0)))
)
toc()
# 297.92 sec elapsed


tic()
set.seed(1)
tune.rid3<- tune_bayes(wf.rid3,
                       resamples = folds,
                       initial = 10,
                       control = control_stack_bayes(),
                       metrics = metric_set(roc_auc),
                       param_info = parameters(penalty(range=c(-10,0)))
)
toc()
# 283.2 sec elapsed


tic()
set.seed(1)
tune.net3<- tune_bayes(wf.net3,
                       resamples = folds,
                       initial = 10,
                       control = control_stack_bayes(),
                       metrics = metric_set(roc_auc),
                       param_info = parameters(penalty(range=c(-10,0)),
                                               mixture(range=c(0,1)))
)
toc()
# 400.29 sec elapsed



## ESCOLHENDO O MELHOR (BEST roc_auc)

show_best(tune.las,n=3)
show_best(tune.rid,n=3)
show_best(tune.net,n=3)
show_best(tune.las2,n=3)
show_best(tune.rid2,n=3)
show_best(tune.net2,n=3)
show_best(tune.las3,n=3)
show_best(tune.rid3,n=3)
show_best(tune.net3,n=3)


