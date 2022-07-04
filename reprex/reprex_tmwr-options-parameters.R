library(tidymodels)
data(concrete, package = "modeldata")
glimpse(concrete)
concrete <- 
  concrete %>% 
  group_by(cement, blast_furnace_slag, fly_ash, water, superplasticizer, 
           coarse_aggregate, fine_aggregate, age) %>% 
  summarize(compressive_strength = mean(compressive_strength),
            .groups = "drop")
nrow(concrete)
set.seed(1501)
concrete_split <- initial_split(concrete, strata = compressive_strength)
concrete_train <- training(concrete_split)
concrete_test  <- testing(concrete_split)

set.seed(1502)
concrete_folds <- 
  vfold_cv(concrete_train, strata = compressive_strength, repeats = 5)

normalized_rec <- 
  recipe(compressive_strength ~ ., data = concrete_train) %>% 
  step_normalize(all_predictors()) 

nnet_spec <- 
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>% 
  set_engine("nnet", MaxNWts = 2600) %>% 
  set_mode("regression")

svm_r_spec <- 
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

svm_p_spec <- 
  svm_poly(cost = tune(), degree = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

knn_spec <- 
  nearest_neighbor(neighbors = tune(), dist_power = tune(), weight_func = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

nnet_param <- 
  nnet_spec %>% 
  parameters() %>% 
  update(hidden_units = hidden_units(c(1, 27)))

normalized <- 
  workflowsets::workflow_set(
    preproc = list(normalized = normalized_rec), 
    models = list(SVM_radial = svm_r_spec, SVM_poly = svm_p_spec, 
                  KNN = knn_spec, neural_network = nnet_spec)
  )
normalized %>% 
  workflowsets::option_add(param = nnet_param, id = "normalized_neural_network") %>% 
  parameters.workflow_set()
