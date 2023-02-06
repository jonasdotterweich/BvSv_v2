# 6_modelling 

#### first for BV ####

#### fitting, tuning, ensembling of the model 


library(flexsdm)
require(dplyr)

#loading model dataframe

df_BV <- read.csv("Data/df_BV+bivars.csv")


#splitting the data frame into training and testing set
#indexes of the data.frame (80% of it)

set.seed(187)#### attention seed was not set when models were run and saved. for testing o f the models testing set was loaded from saved

ind <- sample(1:nrow(df_BV), 0.8*nrow(df_BV))

train_bv <- df_BV[ind,]

test_bv <- df_BV[-ind,]


write.csv(train_bv, "Models/training_BV_v2.csv")

write.csv(test_bv, "Models/testing_BV_v2.csv")



names(df_BV)





#### now models with the training set:

#creating the partition, which can be used to train a series of models

part_train_bv <- part_random(
  data = train_bv,
  pr_ab = "pr_ab",
  method = c(method = "rep_kfold", folds = 5, replicates = 10))




#### GBM fit ####

GBM_fit1 <- fit_gbm(
  part_train_bv,
  response = "pr_ab",
  predictors= c("BIO3" ,                       "BIO7",                        "BIO15",                      
                "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                "ER_tri",                      "WC_alt_lonlat",               "merged_LC"   ),
  partition = ".part",
  thr= c("max_sens_spec"))


perf_result_gbm <- GBM_fit1$performance



#### NET fit ###

NET_fit1 <- fit_net(
  part_train_bv,
  response = "pr_ab",
  predictors = c("BIO3" ,                       "BIO7",                        "BIO15",                      
                 "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                 "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                 "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
  partition = ".part",
  thr = c("max_sens_spec")
)

perf_result_net <- NET_fit1$performance




#### RAF Fit ####

RAF_fit1 <- fit_raf(part_train_bv,
                    response = "pr_ab",
                    predictors = c("BIO3" ,                       "BIO7",                        "BIO15",                      
                                   "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                                   "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                                   "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
                    partition = ".part",
                    thr = c("max_sens_spec")
)

perf_result_raf <- RAF_fit1$performance




#### SVM fit ####

SVM_fit1 <- fit_svm(part_train_bv,
                    response = "pr_ab",
                    predictors = c("BIO3" ,                       "BIO7",                        "BIO15",                      
                                   "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                                   "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                                   "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
                    partition = ".part",
                    thr = c("max_sens_spec")
)

perf_result_svm <- SVM_fit1$performance


##### for now stopping here. Maxent not used due to complications later on
## net will be probably dropped due to bad performance




##### Tuning of models #### 

##gbm ###

#grid for  GBM model tuning

tune_grid_gbm <- expand.grid(n.trees = c(20, 50, 100),
                             shrinkage = c(0.1, 0.5, 1),
                             n.minobsinnode = c(1, 3, 5, 7, 9)
)


gbm_tune <-
  tune_gbm(
    data = part_train_bv,
    response = "pr_ab",
    predictors = c("BIO3" ,                       "BIO7",                        "BIO15",                      
                   "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                   "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                   "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
    partition = ".part",
    grid = tune_grid_gbm,
    thr = "max_sens_spec",
    metric = "TSS",
    n_cores = 1
  )


hyper_gbmtune <- gbm_tune$hyper_performance



### random forest

tune_grid_raf <-
  expand.grid(mtry = seq(1, 7, 1))

rf_tune <-
  tune_raf(
    data = part_train_bv,
    response = "pr_ab",
    predictors = c("BIO3" ,                       "BIO7",                        "BIO15",                      
                   "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                   "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                   "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
    partition = ".part",
    grid = tune_grid_raf,
    thr = "max_sens_spec",
    metric = "TSS",
    n_cores = 1
  )


hyper_rftune <- rf_tune$hyper_performance




### svm tune ###

tune_grid_svm <-
  expand.grid(
    C = c(2, 4, 8, 16, 20),
    sigma = c(0.01, 0.1, 0.2, 0.3, 0.4)
  )

svm_tune <-
  tune_svm(
    data = part_train_bv,
    response = "pr_ab",
    predictors = c("BIO3" ,                       "BIO7",                        "BIO15",                      
                   "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                   "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                   "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
    partition = ".part",
    grid = tune_grid_svm,
    thr = "max_sens_spec",
    metric = "TSS",
    n_cores = 1
  )

hyper_svmtune <- svm_tune$hyper_performance





### GBM 2

hyper_gbmtune[which(hyper_gbmtune$TSS_mean == max(hyper_gbmtune$TSS_mean)),]

#tree:50
#shrink:0.5
#minobs: 3

GBM_fit2 <- fit_gbm(
  part_train_bv,
  response = "pr_ab",
  predictors= c("BIO3" ,                       "BIO7",                        "BIO15",                      
                "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
  partition = ".part",
  thr= c("max_sens_spec"),
  n_trees = 50,
  n_minobsinnode = 3,
  shrinkage = 0.5
)

perf_res_gbm2 <- GBM_fit2$performance

perf_res_gbm2$TSS_mean
perf_result_gbm$TSS_mean




### Random forest

hyper_rftune[which(hyper_rftune$TSS_mean == max(hyper_rftune$TSS_mean)),]

#

RAF_fit2 <- fit_raf(part_train_bv,
                    response = "pr_ab",
                    predictors = c("BIO3" ,                       "BIO7",                        "BIO15",                      
                                   "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                                   "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                                   "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
                    partition = ".part",
                    thr = c("max_sens_spec"),
                    mtry = 2
)


perf_res_rf2 <- RAF_fit2$performance

perf_res_rf2$TSS_mean
perf_result_raf$TSS_mean


###SVM 


hyper_svmtune[which(hyper_svmtune$TSS_mean == max(hyper_svmtune$TSS_mean)),]

#c=2
#sigma=0.1

SVM_fit2 <- fit_svm(part_train_bv,
                    response = "pr_ab",
                    predictors = c("BIO3" ,                       "BIO7",                        "BIO15",                      
                                   "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                                   "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                                   "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
                    partition = ".part",
                    thr = c("max_sens_spec"),
                    sigma = 0.1,
                    C = 2
)

perf_res_svm2 <- SVM_fit2$performance

perf_res_svm2$TSS_mean
perf_result_svm$TSS_mean



###### tuning performances #### 

tuned_performance <- rbind(perf_res_gbm2, perf_res_rf2, perf_res_svm2)


####### Ensemble ##### 


BV_ensembled_v2 <- fit_ensemble(
  models = list(GBM_fit2, RAF_fit2, SVM_fit2),
  ens_method = "mean",
  thr = "max_sens_spec",
  thr_model = "max_sens_spec",
  metric = "TSS"
)

perf_ensemble <- BV_ensembled_v2$performance

write.csv(perf_ensemble, "Models/perf_ensemble_BV_v2.csv")

save(BV_ensembled_v2, file = "Models/BV_ensembled_v2.Rda")









