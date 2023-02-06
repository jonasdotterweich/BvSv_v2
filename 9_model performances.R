# 9_ model performances visualization


library(ggplot2)
library(flexsdm)

#loading the model for BV

load("Models/BV_ensembled_v2.Rda")
load("Models/BV_ens_testing.Rda")

#loading the model for SV

load("Models/SV_ensembled_v2.Rda")

load("Models/SV_ens_testing.Rda")

#trainbv

BVperf <- BV_ensembled_v2$performance

BVmodels <- BV_ensembled_v2$models

#models
bvm1 <- BVmodels$m_1
bvm2 <- BVmodels$m_2
bvm3 <- BVmodels$m_3

#model performances table
bv_summary <- sdm_summarize(models = list(bvm1, bvm2, bvm3))

#testbv

BVperf_test <- testing_ens_bv$performance

BVmodels_test <- testing_ens_bv$models

#models
bvm1_t <- BVmodels_test$m_1
bvm2_t <- BVmodels_test$m_2
bvm3_t <- BVmodels_test$m_3

#model performances table
bv_summary_test <- sdm_summarize(models = list(bvm1_t, bvm2_t, bvm3_t))

bvtest_sum_df <- as.data.frame(bv_summary_test)

#single modle performances
bvm1perf <- bvm1$performance
bvm2perf <- bvm2$performance
bvm3perf <- bvm3$performance

#same as sdm_summarize
#bvmodels_perf <- rbind(bvm1perf, bvm2perf, bvm3perf)

class(bv_summary)

ggplot(data = bv_sum_df, mapping = aes(x= c("TSS_mean", "AUC_mean"), y= c("gbm", "raf", "svm")))+
  geom_point()


ggplot() +
  geom_point(bv_sum_df, mapping = aes(x = TSS_mean, y = AUC_mean, color = model), show.legend = T) +
  
  geom_point(bvtest_sum_df,  mapping = aes(x = TSS_mean, y = AUC_mean, color = model), shape=15, show.legend = T) +
  
  labs(x = "True Skill Statistic - TSS ", y = "Area under the curve - AUC")


### second try
mean_perf_df <- as.data.frame(BVperf)
mean_perftest_df <- as.data.frame(BVperf_test)



ggplot() +
  geom_point(bv_sum_df, mapping = aes(x = TSS_mean, y = AUC_mean, color = model, shape = "Training Set"), show.legend = T) +
  
  geom_point(bvtest_sum_df,  mapping = aes(x = TSS_mean, y = AUC_mean, color = model, shape = "Testing Set"), show.legend = T) +
  
  geom_point(mean_perf_df, mapping = aes(x = TSS_mean, y = AUC_mean, color = model, shape = "Training Set"), show.legend = T) +
  
  geom_point(mean_perftest_df,  mapping = aes(x = TSS_mean, y = AUC_mean, color = model, shape = "Testing Set"), show.legend = T) +
  
  scale_color_manual(values = c("gbm" = "red2", "raf" = "blue", "svm" = "green2", "mean" = "grey15"), name = "Model Type") +
  
  scale_shape_manual(values = c("Training Set" = 19, "Testing Set" = 15 ), name = "Data Set") +
  
  labs(x = "True Skill Statistic - TSS ", y = "Area under the curve - AUC")


### trying to invert the colors and shapes

BANKvole_modelperf <- ggplot() +
  geom_point(bv_sum_df, mapping = aes(x = TSS_mean, y = AUC_mean, color = "Training Set", shape = model) , size = 4, show.legend = T) +
  
  geom_point(bvtest_sum_df,  mapping = aes(x = TSS_mean, y = AUC_mean, color = "Testing Set", shape = model,), size = 4, show.legend = T) +
  
  geom_point(mean_perf_df, mapping = aes(x = TSS_mean, y = AUC_mean, color = "Training Set", shape = model), size = 4, show.legend = T) +
  
  geom_point(mean_perftest_df,  mapping = aes(x = TSS_mean, y = AUC_mean, color = "Testing Set", shape = model), size = 4, show.legend = T) +
  
  scale_color_manual(values = c("Training Set" = "blue", "Testing Set" = "red"), name = "Data Set") +
  
  scale_shape_manual(values = c("gbm" = 15, "raf" = 16, "svm" = 17, "mean" = 7), name = "Model Type") +
  
  labs(x = "True Skill Statistic - TSS ", y = "Area under the curve - AUC", title = "A - Model Performances Bank Vole" )


  

##### now doing the same for SV


#trainsv

SVperf <- SV_ensembled_v2$performance

SVmodels <- SV_ensembled_v2$models

#models
svm1 <- SVmodels$m_1
svm2 <- SVmodels$m_2
svm3 <- SVmodels$m_3

#model performances table
sv_summary <- sdm_summarize(models = list(svm1, svm2, svm3))


#testsv


perf_ensemble_SV_v2_test

SVperf_test <- testing_ens_sv$performance

SVmodels_test <- testing_ens_sv$models

#models
svm1_t <- SVmodels_test$m_1
svm2_t <- SVmodels_test$m_2
svm3_t <- SVmodels_test$m_3

#model performances table
sv_summary_test <- sdm_summarize(models = list(svm1_t, svm2_t, svm3_t))

svtest_sum_df <- as.data.frame(sv_summary_test)



SNOWvole_modeperf <- ggplot() +
  geom_point(sv_summary, mapping = aes(x = TSS_mean, y = AUC_mean, color = "Training Set", shape = model) , size = 4, show.legend = T) +
  
  geom_point(svtest_sum_df,  mapping = aes(x = TSS_mean, y = AUC_mean, color = "Testing Set", shape = model,), size = 4, show.legend = T) +
  
  geom_point(SVperf, mapping = aes(x = TSS_mean, y = AUC_mean, color = "Training Set", shape = model), size = 4, show.legend = T) +
  
  geom_point(SVperf_test,  mapping = aes(x = TSS_mean, y = AUC_mean, color = "Testing Set", shape = model), size = 4, show.legend = T) +
  
  scale_color_manual(values = c("Training Set" = "blue", "Testing Set" = "red"), name = "Data Set") +
  
  scale_shape_manual(values = c("gbm" = 15, "raf" = 16, "svm" = 17, "mean" = 7), name = "Model Type") +
  
  labs(x = "True Skill Statistic - TSS ", y = "Area under the curve - AUC", title = "B - Model Performances Snow Vole")



gridExtra::grid.arrange(BANKvole_modelperf, SNOWvole_modeperf)







##### To do: figure out nice way to display & search common data which is displayed in publiations
       

citation(package = "ggplot2")
