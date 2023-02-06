# 6.3 model testing 


##### creating the testing model ###### 



test_bv <- read.csv("Models/testing_BV_v2.csv")



part_test_bv <- part_random(
  data = test_bv,
  pr_ab = "pr_ab",
  method = c(method = "rep_kfold", folds = 5, replicates = 10))





#### GBM fit ####

GBM_fit1 <- fit_gbm(
  part_test_bv,
  response = "pr_ab",
  predictors= c("BIO3" ,                       "BIO7",                        "BIO15",                      
                "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
  partition = ".part",
  thr = c('max_sens_spec'))


perf_result_gbm <- GBM_fit1$performance



#### RAF Fit ####

RAF_fit1 <- fit_raf(part_test_bv,
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

SVM_fit1 <- fit_svm(part_test_bv,
                    response = "pr_ab",
                    predictors = c("BIO3" ,                       "BIO7",                        "BIO15",                      
                                   "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                                   "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                                   "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
                    partition = ".part",
                    thr = c("max_sens_spec")
)

perf_result_svm <- SVM_fit1$performance


##### Tuning of models #### 

##gbm ###

#grid for  GBM model tuning

tune_grid_gbm <- expand.grid(n.trees = c(20, 50, 100),
                             shrinkage = c(0.1, 0.5, 1),
                             n.minobsinnode = c(1, 3, 5, 7, 9)
)



gbm_tune <-
  tune_gbm(
    data = part_test_bv,
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

tuneperf_gbm <- gbm_tune$performance


hyper_gbmtune <- gbm_tune$hyper_performance


### random forest

tune_grid_raf <-
  expand.grid(mtry = seq(1, 7, 1))

rf_tune <-
  tune_raf(
    data = part_test_bv,
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
    data = part_test_bv,
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

#tree:100
#shrink:0.1
#minobs: 9

GBM_fit2 <- fit_gbm(
  part_test_bv,
  response = "pr_ab",
  predictors= c("BIO3" ,                       "BIO7",                        "BIO15",                      
                "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
  partition = ".part",
  thr= c("max_sens_spec"),
  n_trees = 50,
  n_minobsinnode = 5,
  shrinkage = 0.5
)

perf_res_gbm2 <- GBM_fit2$performance

perf_res_gbm2$TSS_mean
perf_result_gbm$TSS_mean



### Random forest

hyper_rftune[which(hyper_rftune$TSS_mean == max(hyper_rftune$TSS_mean)),]

#

RAF_fit2 <- fit_raf(part_test_bv,
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

#c=8
#sigma=0.1

SVM_fit2 <- fit_svm(part_test_bv,
                    response = "pr_ab",
                    predictors = c("BIO3" ,                       "BIO7",                        "BIO15",                      
                                   "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                                   "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                                   "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
                    partition = ".part",
                    thr = c("max_sens_spec"),
                    sigma = 0.1,
                    C = 8
)

perf_res_svm2 <- SVM_fit2$performance

perf_res_svm2$TSS_mean
perf_result_svm$TSS_mean


###### tuning performances #### 

tuned_performance <- rbind(perf_res_gbm2, perf_res_rf2, perf_res_svm2)

untuned_perf <- rbind(perf_result_gbm, perf_result_raf, perf_result_svm)

tuned_performance$TSS_mean
untuned_perf$TSS_mean

#we can see, that in the sense of tuning, it only worked for gbm
# this can be different however afer assembly


####### Ensemble ##### 


testing_ens_bv <- fit_ensemble(
  models = list(GBM_fit2, RAF_fit2, SVM_fit2),
  ens_method = "mean",
  thr = "max_sens_spec",
  thr_model = "max_sens_spec",
  metric = "TSS"
) 

testing_ens_bv_untuned <- fit_ensemble(
  models = list(GBM_fit1, RAF_fit1, SVM_fit1),
  ens_method = "mean",
  thr = "max_sens_spec",
  thr_model = "max_sens_spec",
  metric = "TSS"
) 


#according to highest TSS means after of before tuning
testing_ens_bv_mixed <- fit_ensemble(
  models = list(GBM_fit2, RAF_fit1, SVM_fit1),
  ens_method = "mean",
  thr = "max_sens_spec",
  thr_model = "max_sens_spec",
  metric = "TSS"
) 



perf_testing_ens_bv <- testing_ens_bv$performance
perf_test_ens_bv_untuned <- testing_ens_bv_untuned$performance
perf_testing_ens_bv_mixed <- testing_ens_bv_mixed$performance


perf_testing_ens_bv$TSS_mean 
perf_test_ens_bv_untuned$TSS_mean
perf_testing_ens_bv_mixed$TSS_mean

### greatest TSS mean in overall tuned model

write.csv(perf_testing_ens_bv, "Models/perf_ensemble_BV_v2_test.csv")

save(testing_ens_bv, file = "Models/BV_ens_testing.Rda")







######################    ---------------------------------------     ####################






## now the testing of the Sv model

test_sv <- read.csv("Models/testing_SV_v2.csv")

part_test_sv <- part_random(
  data = test_sv,
  pr_ab = "pr_ab",
  method = c(method = "rep_kfold", folds = 5, replicates = 10))


#### GBM fit ####

GBM_fit1sv <- fit_gbm(
  part_test_sv,
  response = "pr_ab",
  predictors= c("BIO3" ,                       "BIO7",                        "BIO15",                      
                "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
  partition = ".part",
  thr = c('max_sens_spec'))


perf_result_gbmsv <- GBM_fit1sv$performance



#### RAF Fit ####

RAF_fit1sv <- fit_raf(part_test_sv,
                    response = "pr_ab",
                    predictors = c("BIO3" ,                       "BIO7",                        "BIO15",                      
                                   "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                                   "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                                   "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
                    partition = ".part",
                    thr = c("max_sens_spec")
)

perf_result_rafsv <- RAF_fit1sv$performance




#### SVM fit ####

SVM_fit1sv <- fit_svm(part_test_sv,
                    response = "pr_ab",
                    predictors = c("BIO3" ,                       "BIO7",                        "BIO15",                      
                                   "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                                   "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                                   "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
                    partition = ".part",
                    thr = c("max_sens_spec")
)

perf_result_svmsv <- SVM_fit1sv$performance


##### Tuning of models #### 

##gbm ###

#grid for  GBM model tuning

tune_grid_gbmsv <- expand.grid(n.trees = c(10, 20, 50, 100),
                             shrinkage = c(0.1, 0.5, 1),
                             n.minobsinnode = c(0.1, 0.5, 1, 3, 5, 7, 9)
)



gbm_tunesv <-
  tune_gbm(
    data = part_test_sv,
    response = "pr_ab",
    predictors = c("BIO3" ,                       "BIO7",                        "BIO15",                      
                   "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                   "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                   "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
    partition = ".part",
    grid = tune_grid_gbmsv,
    thr = "max_sens_spec",
    metric = "TSS",
    n_cores = 1
  )

tuneperf_gbmsv <- gbm_tunesv$performance


hyper_gbmtunesv <- gbm_tunesv$hyper_performance


### random forest

tune_grid_rafsv <-
  expand.grid(mtry = seq(1, 7, 1))

rf_tunesv <-
  tune_raf(
    data = part_test_sv,
    response = "pr_ab",
    predictors = c("BIO3" ,                       "BIO7",                        "BIO15",                      
                   "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                   "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                   "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
    partition = ".part",
    grid = tune_grid_rafsv,
    thr = "max_sens_spec",
    metric = "TSS",
    n_cores = 1
  )


hyper_rftunesv <- rf_tunesv$hyper_performance



### svm tune ###

tune_grid_svmsv <-
  expand.grid(
    C = c(2, 4, 8, 16, 20),
    sigma = c(0.01, 0.1, 0.2, 0.3, 0.4)
  )

svm_tunesv <-
  tune_svm(
    data = part_test_sv,
    response = "pr_ab",
    predictors = c("BIO3" ,                       "BIO7",                        "BIO15",                      
                   "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                   "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                   "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
    partition = ".part",
    grid = tune_grid_svmsv,
    thr = "max_sens_spec",
    metric = "TSS",
    n_cores = 1
  )

hyper_svmtunesv <- svm_tunesv$hyper_performance



### GBM 2

hyper_gbmtunesv[which(hyper_gbmtunesv$TSS_mean == max(hyper_gbmtunesv$TSS_mean)),]

#tree:100
#shrink:0.1
#minobs: 9

GBM_fit2sv <- fit_gbm(
  part_test_sv,
  response = "pr_ab",
  predictors= c("BIO3" ,                       "BIO7",                        "BIO15",                      
                "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
  partition = ".part",
  thr= c("max_sens_spec"),
  n_trees = 10,
  n_minobsinnode = 0.1,
  shrinkage = 1
)

perf_res_gbm2sv <- GBM_fit2sv$performance

perf_res_gbm2sv$TSS_mean
perf_result_gbmsv$TSS_mean



### Random forest

hyper_rftunesv[which(hyper_rftunesv$TSS_mean == max(hyper_rftunesv$TSS_mean)),]

#

RAF_fit2sv <- fit_raf(part_test_sv,
                    response = "pr_ab",
                    predictors = c("BIO3" ,                       "BIO7",                        "BIO15",                      
                                   "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                                   "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                                   "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
                    partition = ".part",
                    thr = c("max_sens_spec"),
                    mtry = 1
)


perf_res_rf2sv <- RAF_fit2sv$performance

perf_res_rf2sv$TSS_mean
perf_result_rafsv$TSS_mean


###SVM 


hyper_svmtunesv[which(hyper_svmtunesv$TSS_mean == max(hyper_svmtunesv$TSS_mean)),]

#c=8
#sigma=0.1

SVM_fit2sv <- fit_svm(part_test_sv,
                    response = "pr_ab",
                    predictors = c("BIO3" ,                       "BIO7",                        "BIO15",                      
                                   "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                                   "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                                   "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
                    partition = ".part",
                    thr = c("max_sens_spec"),
                    sigma = 0.3,
                    C = 2
)

perf_res_svm2sv <- SVM_fit2sv$performance

perf_res_svm2sv$TSS_mean
perf_result_svmsv$TSS_mean


###### tuning performances #### 

tuned_performancesv <- rbind(perf_res_gbm2sv, perf_res_rf2sv, perf_res_svm2sv)

untuned_perfsv <- rbind(perf_result_gbmsv, perf_result_rafsv, perf_result_svmsv)

tuned_performancesv$TSS_mean
untuned_perfsv$TSS_mean

#we can see, that in the sense of tuning, it only worked for gbm
# this can be different however afer assembly


####### Ensemble ##### 


testing_ens_sv <- fit_ensemble(
  models = list(GBM_fit2sv, RAF_fit2sv, SVM_fit2sv),
  ens_method = "mean",
  thr = "max_sens_spec",
  thr_model = "max_sens_spec",
  metric = "TSS"
) 

testing_ens_sv_untuned <- fit_ensemble(
  models = list(GBM_fit1sv, RAF_fit1sv, SVM_fit1sv),
  ens_method = "mean",
  thr = "max_sens_spec",
  thr_model = "max_sens_spec",
  metric = "TSS"
) 


#according to highest TSS means after of before tuning
testing_ens_sv_mixed <- fit_ensemble(
  models = list(GBM_fit2sv, RAF_fit1sv, SVM_fit1sv),
  ens_method = "mean",
  thr = "max_sens_spec",
  thr_model = "max_sens_spec",
  metric = "TSS"
) 



perf_testing_ens_sv <- testing_ens_sv$performance
perf_test_ens_sv_untuned <- testing_ens_sv_untuned$performance
perf_testing_ens_sv_mixed <- testing_ens_sv_mixed$performance


perf_testing_ens_sv$TSS_mean 
perf_test_ens_sv_untuned$TSS_mean
perf_testing_ens_sv_mixed$TSS_mean

### greatest TSS mean in overall untuned model

write.csv(perf_testing_ens_sv, "Models/perf_ensemble_SV_v2_test.csv")

save(testing_ens_sv, file = "Models/SV_ens_testing.Rda")
