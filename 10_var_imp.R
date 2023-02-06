# 10_variable importance

## this time not between aligned and unaligned models but just between BV & SV

library(vip)
library(rminer)
library(dplyr)

#loading the model for BV

load("Models/BV_ensembled_v2.Rda")

#loading the model for SV

load("Models/SV_ensembled_v2.Rda")


BVperf <- BV_ensembled_v2$performance

BVmodels <- BV_ensembled_v2$models

#models
bvm1 <- BVmodels$m_1
bvm2 <- BVmodels$m_2
bvm3 <- BVmodels$m_3


bvm1 <- bvm1$model
bvm2 <- bvm2$model
bvm3 <- bvm3$model


vip_gbmBV <- vip(bvm1, num_features = 11)

vip_raf_BV <- vip(bvm2, num_features = 11)

vip_gbmBV_df <- vip_gbmBV$data

vip_raf_BV_df <- vip_raf_BV$data


## svm in rminer

#loading the train data

train <- read.csv("Models/training_BV_v2.csv")

train <- train %>% dplyr::select(5:15)

predsvm=function(bvm3, train)
{ return (predict(bvm3, train))}

svm_imp <- Importance(bvm3, train, PRED = predsvm)

###########################
#### others dont work   ###
#predraf=function(bvm2, train)
#{ return (predict(bvm2, train))}

#raf_imp <- Importance(bvm2, train, PRED = predraf)

#raf_imp$imp
##########################

svm_val <- svm_imp$value
  
svm_resp <- svm_imp$sresponses



svm_imp2 <- svm_imp$imp



svm_imp2$predictors <- names(train)

svm_imp2$predictors

svm_imp2 <- reshape2::melt(svm_imp2, id = c("predictors"))

svm_imp2$L1[1:11] <- names(train)

svm_imp_df <- as.data.frame(svm_imp2)

svm_imp_df <- svm_imp_df[-c(12:22),]

svm_imp_df <- svm_imp_df %>% dplyr::rename( Importance = value,
                                            Variable = L1)

imp_df_ord <- svm_imp_df %>%dplyr::arrange(desc(svm_imp_df$importance))


ggplot(data = imp_df_ord, aes(x = predictors, y = importance)) +
  geom_bar(stat = "identity")+
  coord_flip()
  
  

## why is the chart not aligned in a decreasing order?
# the dataframe is sortet



### trying to put the three dfs together to have varims for the whole ensembled bv model


vip_gbmBV_df <- as.data.frame(vip_gbmBV_df)
vip_raf_BV_df <- as.data.frame(vip_raf_BV_df)

merged_vip <- cbind(vip_gbmBV_df, vip_raf_BV_df, svm_imp_df)

write.csv(merged_vip, "varimps_dfs/merged_vipsBV.csv")
###ordering in numbers
ordered_vips <- read.csv("varimps_dfs/vipsBV_orderd.csv", sep = ";" )

ordered_vips <- ordered_vips[-c(1:11),]

ordered_vips <- ordered_vips[-5]
ordered_vips <- ordered_vips[-5]
ordered_vips <- ordered_vips[-5]

ordered_vips <- ordered_vips %>% dplyr::rename( Importance_GBM = Variable,
                                                Importance_RAF = Importance, 
                                                Importance_SVM = Variable.1)
ordered_vips <- ordered_vips[-c(1),]

## the idea now is to have a barchart with two y axis one for the two vip created and one for rminer svm vaimp


ggplot(ordered_vips, aes(x = "X", y = c("Importance_GBM", "Importance_RAF"), fill = "Type")) +
  geom_col(width = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Importance_SVM")) +
  labs(x = "Type", y = c("Importance_GBM", "Importance_RAF"), fill = "Type") +
  theme_minimal() +
  ggtitle("Bar Plot with Two Y Axis")

ggplot()+
  geom_bar(data =  ordered_vips, mapping =  aes(x = X, y = "Importance_GBM"))+
  geom_bar(data =  ordered_vips, mapping = aes(x = X, y = "Importance_RAF"))+
  geom_bar(data =  ordered_vips, mapping = aes(x = X, y = "Importance_GBM"))+
  scale_y_discrete(c("Importance_GBM", "Importance_RAF"), 
    sec.axis = sec_axis(~ . * 1.20, name = "Importance_SVM"))
  
  
  
#### trying to rewrite the data frame


ordered_vips$X

ordered_df <- data.frame(Variables= c("BIO3" ,                       "BIO7",                        "BIO15",                      
                                      "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                                      "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                                      "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
                         GBM_Importances= c(1.89410864094162, 2.87857958045309, 1.87675541474569, 
                                            2.44756672279682, 3.32159291412128, 59.4810801929287, 
                                            12.9278792421034, 3.92180500028765,
                                            3.42477737307226, 3.39273302639705, 4.43312189215249),
                         RAF_Importances= c(7.96170404639827, 5.37847394097912, 4.39917191568456,
                                            3.7674071428734, 22.9828242704203, 29.6543721935916,
                                            19.1049775012238, 13.6601619831945,
                                            8.49172693260168, 15.5156471143546, 5.96330992837513),
                         SVM_Importances = c(0, 0.0909090909090909, 0.181818181818182,
                                             0.0909090909090909, 0.181818181818182, 0.181818181818182,
                                             0.181818181818182, 0,
                                             0, 0, 0.0909090909090909))



#ordered_df_melt <- melt(ordered_df, id.vars="Variables")

# create the bar plot
#ggplot(ordered_df_melt, aes(x=Variables, y=value, fill=variable)) + 
 #geom_bar(stat = "identity", 
 #         position = "dodge",) +
#  xlab("Variables") +
#  ylab("Importance") +
#  ggtitle("Importance of variables in different models") +
#  theme_minimal() +
#  scale_fill_discrete(name="Model") +
 # scale_y_continuous(sec.axis = sec_axis(~ ./1000, name = "SVM Importance")) +
  # filter to only include SVM Importance on second y axis
#  geom_bar(data = ordered_df_melt[ordered_df_melt$variable == "SVM_Importances",], 
 #          aes(x = Variables, y = value), 
 #          stat = "identity", 
  #         position = "dodge", 
   #        show.legend = FALSE)


##### second try


#ggplot(ordered_df_melt, aes(x=Variables, y=value, fill=variable)) + 
#  geom_bar(stat="identity", position="dodge") +
#  xlab("Variables") +
#  ylab("Importance") +
#  ggtitle("Importance of variables in different models") +
#  theme_minimal() +
#  scale_fill_discrete(name="Model") +
#  scale_y_continuous(limits = c(0, max(ordered_df_melt$value)), sec.axis = sec_axis(~./1000, name = "SVM_Importance")) +
#  facet_grid(~variable, scales = "free_y", switch = "y")



### third try 

#ggplot(ordered_df, aes(x=Variables, y=GBM_Importances, fill="GBM_Importance")) +
#  geom_col(width = 0.5) +
#  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "SVM_Importance")) +
#  geom_col(aes(y=SVM_Importances, fill="SVM_Importance"), width = 0.5) +
#  scale_fill_manual(values=c("GBM_Importance" = "blue", "SVM_Importance" = "red")) +
#  ggtitle("Variable Importance Plot") +
#  xlab("Variables") +
#  ylab("GBM_Importance") +
#  theme(legend.title = element_blank(),
 #       legend.position = "bottom")


### okay, this also doesnt work, what if I try to make singel plots and put them together afterwards




ordered_df <- ordered_df %>%
  mutate(Mean_Importances = rowMeans(cbind(GBM_Importances, RAF_Importances, SVM_Importances)))

ordered_dfSV$Variables <- factor(ordered_dfSV$Variables, levels = c("BIO3", "BIO7", "BIO15", 
                                                                    "BIO19", "ER_aridityIndexThornthwaite", "ER_growingDegDays5", 
                                                                    "ER_PETDriestQuarter", "ER_PETWettestQuarter", 
                                                                    "ER_tri", "WC_alt_lonlat", "merged_LC"))


########
###### final plot #### 
########


allBVplot <- ggplot(ordered_df) +
              geom_col(aes(x=Variables, y=GBM_Importances, fill="GBM"), width = 0.175, just = 2)+
              geom_col(aes(x=Variables, y=RAF_Importances, fill="Random Forest"), width = 0.175, just = 1)+
              geom_col(aes(x=Variables, y=(SVM_Importances*10), fill="SVM"), width = 0.175, just = 0) +
              geom_col(aes(x=Variables, y=(Mean_Importances), fill="Mean"), width = 0.175, just = -1) +
              ggtitle("Variable Importances for the Bank Vole Models") +
              xlab("Variables") +
              ylab("Importance") +
              scale_x_discrete(limits = c("BIO3", "BIO7", "BIO15", "BIO19", 
                                          "ER_aridityIndexThornthwaite", "ER_growingDegDays5",
                                          "ER_PETDriestQuarter", "ER_PETWettestQuarter", 
                                          "ER_tri", "WC_alt_lonlat", "merged_LC")) +
              theme(legend.title = element_blank(),
                    legend.position = "bottom",
                    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



meanBVplot <- ggplot(ordered_df) +
                geom_col(aes(x=Variables, y=(Mean_Importances)), width = 0.5) +
                ggtitle("Variable Importance Plot for the Bank Vole Model") +
                xlab("Variables") +
                ylab("Importance") +
                scale_x_discrete(limits = c("BIO3", "BIO7", "BIO15", "BIO19", 
                                            "ER_aridityIndexThornthwaite", "ER_growingDegDays5",
                                            "ER_PETDriestQuarter", "ER_PETWettestQuarter", 
                                            "ER_tri", "WC_alt_lonlat", "merged_LC")) +
                theme(legend.title = element_blank(),
                      legend.position = "bottom",
                      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



##### also need to do the Importances for SV model

#loading the model for SV

load("Models/SV_ensembled_v2.Rda")


SVperf <- SV_ensembled_v2$performance

SVmodels <- SV_ensembled_v2$models

#models
svm1 <- SVmodels$m_1
svm2 <- SVmodels$m_2
svm3 <- SVmodels$m_3


svm1 <- svm1$model
svm2 <- svm2$model
svm3 <- svm3$model



vip_gbmSV <- vip(svm1, num_features = 11)

vip_raf_SV <- vip(svm2, num_features = 11)

vip_gbmSV_df <- vip_gbmSV$data

vip_raf_SV_df <- vip_raf_SV$data


## svm in rminer

#loading the train data

train_sv <- read.csv("Models/training_SV_v2.csv")

train_sv <- train_sv %>% dplyr::select(5:15)

predsvmSV=function(svm3, train_sv)
{ return (predict(svm3, train_sv))}

svm_imp_SV <- Importance(svm3, train, PRED = predsvmSV)


svm_val_SV <- svm_imp_SV$value

svm_resp_SV <- svm_imp_SV$sresponses



svm_imp2_SV <- svm_imp_SV$imp



svm_imp2_SV$predictors <- names(train_sv)

svm_imp2_SV$predictors

svm_imp2_SV <- reshape2::melt(svm_imp2_SV, id = c("predictors"))

svm_imp2_SV$L1[1:11] <- names(train_sv)

svm_impSV_df <- as.data.frame(svm_imp2_SV)

svm_impSV_df <- svm_impSV_df[-c(12:22),]

svm_impSV_df <- svm_impSV_df %>% dplyr::rename( Importance = value,
                                            Variable = L1)

#imp_df_ord <- svm_imp_df %>%dplyr::arrange(desc(svm_imp_df$importance))


ggplot(data = svm_impSV_df, aes(x = Variable, y = Importance)) +
  geom_bar(stat = "identity")+
  coord_flip()




vip_gbmSV_df <- as.data.frame(vip_gbmSV_df)
vip_raf_SV_df <- as.data.frame(vip_raf_SV_df)

merged_vipSV <- cbind(vip_gbmSV_df, vip_raf_SV_df, svm_impSV_df)

write.csv(merged_vipSV, "varimps_dfs/merged_vipsSV.csv")
###ordering in numbers
ordered_vips <- read.csv("varimps_dfs/vipsSV_orderd.csv", sep = ";" )

ordered_dfSV <- data_frame(Variables= c("BIO3" ,                       "BIO7",                        "BIO15",                      
                                        "BIO19",                       "ER_aridityIndexThornthwaite", "ER_growingDegDays5",         
                                        "ER_PETDriestQuarter",         "ER_PETWettestQuarter",       
                                        "ER_tri",                      "WC_alt_lonlat",               "merged_LC"),
                           GBM_Importances= c(10.9270812364979, 9.36807582115561, 0,
                                              8.72664052836017, 3.55765055355151, 3.74922882098909,
                                              0, 3.14346070425635, 
                                              57.9543946511287, 1.9639289173366, 0.609538766724078),
                           RAF_Importances= c(1.48607041461782, 2.81176575046915, 1.25446084355687,
                                              2.44102109340952, 2.03364977037546, 3.80459393792017,
                                              2.17141003004539, 1.83774630438133, 
                                              4.56498136668085, 3.83496927298922, 0.923414548887547),
                           SVM_Importances = c(0.2, 0.3, 0,
                                               0, 0, 0, 
                                               0, 0,
                                               0.3, 0.2, 0))


ordered_dfSV <- ordered_dfSV %>%
  mutate(Mean_Importances = rowMeans(cbind(GBM_Importances, RAF_Importances, SVM_Importances)))


allSVplot <- ggplot(ordered_dfSV) +
              geom_col(aes(x=Variables, y=GBM_Importances, fill="GBM"), width = 0.175, just = 2)+
              geom_col(aes(x=Variables, y=RAF_Importances, fill="Random Forest"), width = 0.175, just = 1)+
              geom_col(aes(x=Variables, y=(SVM_Importances *10), fill="SVM"), width = 0.175, just = 0) +
              geom_col(aes(x=Variables, y=Mean_Importances, fill="Mean"), width = 0.175, just = -1) +
              ggtitle("Variable Importances for the Snow Vole Models") +
              xlab("Variables") +
              ylab("Importance") +
              scale_x_discrete(limits = c("BIO3", "BIO7", "BIO15", "BIO19", 
                              "ER_aridityIndexThornthwaite", "ER_growingDegDays5",
                              "ER_PETDriestQuarter", "ER_PETWettestQuarter", 
                              "ER_tri", "WC_alt_lonlat", "merged_LC")) +
              theme(legend.title = element_blank(),
                    legend.position = "bottom",
                    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))





meanSVplot <- ggplot(ordered_dfSV) +
                geom_col(aes(x=Variables, y=Mean_Importances), width = 0.5) +
                ggtitle("Variable Importance Plot for the Snow Vole Model") +
                xlab("Variables") +
                ylab("Importance") +
                scale_x_discrete(limits = c("BIO3", "BIO7", "BIO15", "BIO19", 
                              "ER_aridityIndexThornthwaite", "ER_growingDegDays5",
                              "ER_PETDriestQuarter", "ER_PETWettestQuarter", 
                              "ER_tri", "WC_alt_lonlat", "merged_LC")) +
                theme(legend.title = element_blank(),
                      legend.position = "bottom",
                      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
