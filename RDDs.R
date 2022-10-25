####Loading####
rm(list = ls())
#install.packages(c("earth", "caret"))
#install.packages(c("vip", "pdp"))
#install.packages("crs")
#install.packages("PLRModels")
#install.packages("labelled")
load("~/new projects/MC resubmission/data_short.RData")
library(sandwich)
library(lmtest)
library(dplyr)     
library(ggplot2) 
library(labelled) 

data.2nd_short <- remove_labels(data.2nd_short)
data.2nd_short <- data.2nd_short[!is.na(data.2nd_short$non_Russian) &  !is.na(data.2nd_short$income_perperson) & !is.na(data.2nd_short$urban) & 
                                   !is.na(data.2nd_short$male) &  !is.na(data.2nd_short$p.health_good_exc) & !is.na(data.2nd_short$alcohol_mother) & 
                                   !is.na(data.2nd_short$n_household_memb) & !is.na(data.2nd_short$age_months) & !is.na(data.2nd_short$p.age), ]
data.2nd_short$productive_time_min <- data.2nd_short$extra_phys_min + data.2nd_short$extracurricular_art_min + data.2nd_short$extracurricular_min 

nrow(data.2nd_short[data.2nd_short$birth_timeline >= -36 & data.2nd_short$birth_timeline <= 36, ])

sapply(data.2nd_short, function(x){sum(is.na(x))/nrow(data.2nd_short)}) 

data.2nd_short$const <- 1
attrition <- data.2nd_short %>%
  select(idind, age)  %>%
  group_by(idind) %>%
  count()

#data.2nd_short[, 16] <- NULL
data.2nd_short.last_wave <- data.2nd_short %>%
  group_by(idind) %>%
  filter(age == max(age, na.rm = T)) 

#library(PLRModels) 
#library(crs) 
# Modeling packages
#library(earth)     # for fitting MARS models
#library(caret)     # for automating the tuning process
#library(vip)       # for variable importance
#library(pdp) 
outcomes_list <- list("health_good_exc", "health_score", "chronic_cnd_count",  "health_prob_30d", 
                      "homework_min", "extracurricular_min", "extracurricular_art_min", "TV_internet_min","extra_phys_min", "productive_time_min",
                       "used_PC", "vacay_w_parent_1yr", "total_exhibitions_excursions", "see_friends", "child_care_7d",  "relatives_care_7d", 
                      "life_satisfaction", "poverty","income_perperson", "p.health_good_exc",
                      "veg_cons", "fruit_cons", "meat_cons", "dairy_cons", "vodka_spirits_cons", 
                      "refined_sugar_cons", "high_sugar_cons", "high_starch_cons", 
                      "clothes", "food_essential",  "utilities", "essential_sevices", "all_basic_exp", "luxury_exp",  "durable",
                      "sh_food_essential", "sh_clothes", "sh_luxury_exp", "sh_savings_30d", 
                      "sh_loan_payments_30d",  "sh_utilities", "sh_essential_sevices", "sh_all_basic_exp",
                      "savings_30d", "savings_will_last_several_months",
                      "loan_payments_30d", "consumer_loan_12m",  "loan_from_private_ind", "loan_to_private_ind", 
                      "home_ownership",   "hot_water_supply",  "central_sewerage", 
                      "central_heating") 

outcomes_list_cont <- list("chronic_cnd_count", "homework_min", "extracurricular_min", "extracurricular_art_min", "TV_internet_min","extra_phys_min", "productive_time_min",
                           "total_exhibitions_excursions", "veg_cons", "fruit_cons", "meat_cons", "dairy_cons", "vodka_spirits_cons",  "income_perperson",
                           "refined_sugar_cons", "high_sugar_cons", "high_starch_cons",  "clothes", "food_essential",  "utilities",
                           "essential_sevices", "all_basic_exp", "luxury_exp", "sh_food_essential", "sh_clothes", "sh_luxury_exp", "sh_savings_30d", 
                           "savings_30d","savings_will_last_several_months", "loan_payments_30d","sh_loan_payments_30d", "sh_utilities",
                           "sh_essential_sevices", "sh_all_basic_exp") 
  
outcomes_list_binary <- list("health_good_exc",  "used_PC", "vacay_w_parent_1yr","see_friends", "relatives_care_7d",
                             "durable", "consumer_loan_12m", "loan_from_private_ind", "loan_to_private_ind", "consumer_loan_12m", 
                             "home_ownership",  "poverty", "p.health_good_exc",
                              "hot_water_supply",  "central_sewerage",  "central_heating")
                            
outcomes_list_multinominal <- list("health_score", "life_satisfaction", "consumer_loan_12m")


                     
#for (vars in 1:length(outcomes_list_cont)){
#  regmodel <- paste(outcomes_list_cont[[vars]], "non_Russian + income_perperson + urban + male + 
#                    p.health_good_exc + alcohol_mother + p.age + n_household_memb + age_months + I(age_months^2) + I(age_months^3) + 
#                    + interview_timeline  + I(interview_timeline^2)  + I(interview_timeline^3)", sep = " ~ " )
#  impute_model <-  lm(regmodel, data = data.2nd_short)
#  predict(impute_model, data.2nd_short)
#}


covariates_list <- list() 
covariates_list[["e"]][[1]] <- c("post", "birth_timeline", "post_birth_timeline")
covariates_list[["e"]][[2]] <- c("post", "birth_timeline", "I(birth_timeline^2)", "post:birth_timeline", "post:I(birth_timeline^2)")
covariates_list[["e"]][[3]] <- c("post", "birth_timeline", "I(birth_timeline^2)", "I(birth_timeline^3)",
                            "post:birth_timeline", "post:I(birth_timeline^2)", "post:I(birth_timeline^3)")
#covariates_list[["e"]][[4]] <- c("post", "birth_timeline", "I(birth_timeline^2)", "I(birth_timeline^3)", "I(birth_timeline^4)",
#                            "post:birth_timeline", "post:I(birth_timeline^2)", "post:I(birth_timeline^3)", "post:I(birth_timeline^4)")

covariates_list[["c"]][[1]] <- c("")
covariates_list[["c"]][[2]] <- c("age_months",  "p.age")
covariates_list[["c"]][[3]] <- c("age_months", "p.age", "non_Russian","urban", "male", "higher_education", 
                                 "n_household_memb", "income_perperson",  "poverty",
                                 "p.health_good_exc",   "alcohol_mother", "single_parent")         # "poverty","income_perperson", "p.health_good_exc","alcohol_mother" 
covariates_list[["c"]][[4]] <- c("age_months", "I(age_months^2)", "I(age_months^3)", 
                            "p.age", "non_Russian",   "higher_education",   "urban", "male",
                            "n_household_memb",  "poverty","income_perperson", "p.health_good_exc", 
                            "alcohol_mother", "single_parent" )                                    # "poverty","income_perperson", "p.health_good_exc","alcohol_mother" 

covariates_list[["t"]][[1]] <- c("")
covariates_list[["t"]][[2]] <- c("factor(p.RLMS_wave)")
covariates_list[["t"]][[3]] <- c("interview_timeline", "I(interview_timeline^2)", "I(interview_timeline^3)")
covariates_list[["t"]][[4]] <- c("factor(p.RLMS_wave)", "interview_timeline", "I(interview_timeline^2)", "I(interview_timeline^3)")



model_list <- list()

for (wind in c(12, 24, 36)){
   for (e.var in 1:length(covariates_list[["e"]])){
    for (c.var in 1:length(covariates_list[["c"]])){
      for (t.var in 1:length(covariates_list[["t"]])){
        
        model_label <- paste0("w", wind, ".", "e", e.var, "c", c.var, "t", t.var)
        model_covariates <- unlist(list(covariates_list[["e"]][e.var], covariates_list[["c"]][c.var], covariates_list[["t"]][t.var]))
        model_covariates <- model_covariates[model_covariates != ""]
        model_list[model_label] <- paste(model_covariates, collapse = " + ")
      }
    }
  }
}

outcomes <- rep(unlist(outcomes_list), each =2)
outcomes[1:length(outcomes) %% 2 == 0] <- ""

nrow(data.2nd_short[data.2nd_short$birth_timeline <= 36 & 
                 data.2nd_short$birth_timeline >= -36, ])


####Loop RDD_2nd####
#source("C://Users//alksp//Desktop//RLMS code//RLMS editing 2020/add_in_RDD.R")

RDD_2nd <- data.frame(outcome = unlist(outcomes))

for (var in names(model_list)){
  RDD_2nd[, var] <- NA
}

for (var_n in 1:length(outcomes_list)){
  for (model in 1:length(model_list)){
    
    reg_model <- paste(outcomes_list[[var_n]], model_list[[model]], sep = " ~ ")
    time_window <- as.numeric(substr(names(model_list[model]),  2, 3))
    
    try({
      temp_data <- data.2nd_short
      temp_model <- lm(reg_model, data = temp_data[temp_data$birth_timeline <= time_window & 
                                                     temp_data$birth_timeline >= -time_window & temp_data$year %in% 2010:2017,  ]) 
      cov_matrix <-  vcovCL(temp_model, cluster = temp_model$region.x) #clustered  estimated covariance matrix
      temp_res <- coeftest(temp_model, cov_matrix)
      
      RDD_2nd[which(RDD_2nd$outcome == outcomes_list[[var_n]]),  names(model_list[model])] <- round(temp_res[[2,1]], 3)
      
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.10, 1, 0) 
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.05, 2, n_stars) 
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.01, 3, n_stars) 
      
      if (n_stars == 0){
        RDD_2nd[which(RDD_2nd$outcome == outcomes_list[[var_n]])+1,  names(model_list[model])]  <- paste0("=\"(", round(temp_res[[2,2]],3), ")\"", paste(rep("*", each = n_stars), collapse = ""))
      }
      else{
        RDD_2nd[which(RDD_2nd$outcome == outcomes_list[[var_n]])+1,  names(model_list[model])]  <- paste0("(", round(temp_res[[2,2]],3), ")", paste(rep("*", each = n_stars), collapse = ""))
      }
      
      RDD_2nd[which(RDD_2nd$outcome == outcomes_list[[var_n]])+2,  names(model_list[model])]  <- paste0("=\"(", round(temp_res[["post",4]],3), ")\"", collapse = "") #p-value
      
      
    })
  }
}



#relative effects
models <-  c("w12.e1c1t2", "w12.e1c2t2", "w12.e1c3t2",
             "w24.e2c2t2", "w24.e2c3t2",
             "w36.e3c2t2", "w36.e3c3t2")

RDD_2nd_rel <- RDD_2nd[, c("outcome", models)]
RDD_2nd_rel$Relative_effect <- ""
temp_data <- data.2nd_short.last_wave

for (var_n in 1:length(outcomes_list)){
  av_12 <- mean(as.numeric(unlist(temp_data[ temp_data$birth_timeline <= 12 & 
                       temp_data$birth_timeline >= -12 & temp_data$MC_eligible == 0,  outcomes_list[[var_n]]])), na.rm = T)
  av_24 <- mean(as.numeric(unlist(temp_data[ temp_data$birth_timeline <= 24 & 
                        temp_data$birth_timeline >= -24 & temp_data$MC_eligible == 0,  outcomes_list[[var_n]]])), na.rm = T)
  av_36 <- mean(as.numeric(unlist(temp_data[ temp_data$birth_timeline <= 36 & 
                        temp_data$birth_timeline >= -36 & temp_data$MC_eligible == 0,  outcomes_list[[var_n]]])), na.rm = T)
  mean_vect <- c(av_12, av_12, av_12) 
  coef_vect <- as.numeric(unlist(RDD_2nd_rel[which(RDD_2nd_rel$outcome == outcomes_list[[var_n]]), 2:4])) #(ncol(RDD_2nd_rel)-1)
  rel_effect <- mean((coef_vect*100/mean_vect))
  
  RDD_2nd_rel[which(RDD_2nd_rel$outcome == outcomes_list[[var_n]]), "Relative_effect"] <- paste0(round(rel_effect, 1), "%")
}

dir <- "C:/Users/Lenovo/OneDrive/Documents/new projects/MC resubmission/resubmission Fall 2022"
setwd(dir)
RDD_2nd_rel <- table_relabel(RDD_2nd_rel)

write.csv(RDD_2nd_rel[, c("outcome", models, "Relative_effect")], "RDD_2nd_rel_all.csv")




nrow(temp_data[ temp_data$birth_timeline <= 24 & 
                  temp_data$birth_timeline >= -24 & temp_data$year %in% 2010:2017,  ])

##### 2nd: by rural/urban ####
RDD_2nd_urb <- data.frame(outcome = unlist(outcomes))
RDD_2nd_rur <- data.frame(outcome = unlist(outcomes))


for (var in names(model_list)){
  RDD_2nd_urb[, var] <- NA
}

for (var in names(model_list)){
  RDD_2nd_rur[, var] <- NA
}
####URBAN

for (var_n in 1:length(outcomes_list)){
  for (model in 1:length(model_list)){
    
    reg_model <- paste(outcomes_list[[var_n]], model_list[[model]], sep = " ~ " )
    time_window <- as.numeric(substr(names(model_list[model]),  2, 3))
    
    try({
      temp_model <- lm(reg_model, data = data.2nd_short[data.2nd_short$urban == 1 & data.2nd_short$birth_timeline <= time_window & 
                                                          data.2nd_short$birth_timeline >= -time_window & data.2nd_short$year %in% 2010:2017,  ]) 
      cov_matrix <-  vcovCL(temp_model, cluster = temp_model$region.x) #clustered  estimated covariance matrix
      temp_res <- coeftest(temp_model, cov_matrix)
      
      RDD_2nd_urb[which(RDD_2nd_urb$outcome == outcomes_list[[var_n]]),  names(model_list[model])] <- round(temp_res[[2,1]], 3)
      
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.10, 1, 0) 
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.05, 2, n_stars) 
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.01, 3, n_stars) 
      
      if (n_stars == 0){
        RDD_2nd_urb[which(RDD_2nd_urb$outcome == outcomes_list[[var_n]])+1,  names(model_list[model])]  <- paste0("=\"(", round(temp_res[[2,2]],3), ")\"", paste(rep("*", each = n_stars), collapse = ""))
      }
      else{
        RDD_2nd_urb[which(RDD_2nd_urb$outcome == outcomes_list[[var_n]])+1,  names(model_list[model])]  <- paste0("(", round(temp_res[[2,2]],3), ")", paste(rep("*", each = n_stars), collapse = ""))
      }
      RDD_2nd_urb[which(RDD_2nd_urb$outcome == outcomes_list[[var_n]])+2,  names(model_list[model])]  <- paste0("=\"(", round(temp_res[["post",4]],3), ")\"", collapse = "") #p-value
      
      
    })
  }
}

####RURAL

for (var_n in 1:length(outcomes_list)){
  for (model in 1:length(model_list)){
    
    reg_model <- paste(outcomes_list[[var_n]], model_list[[model]], sep = " ~ " )
    time_window <- as.numeric(substr(names(model_list[model]),  2, 3))
    
    try({
      temp_model <- lm(reg_model, data = data.2nd_short[data.2nd_short$urban == 0 & data.2nd_short$birth_timeline <= time_window & 
                                                          data.2nd_short$birth_timeline >= -time_window & data.2nd_short$year %in% 2010:2017,  ]) 
      cov_matrix <-  vcovCL(temp_model, cluster = temp_model$region.x) #clustered  estimated covariance matrix
      temp_res <- coeftest(temp_model, cov_matrix)
      
      RDD_2nd_rur[which(RDD_2nd_rur$outcome == outcomes_list[[var_n]]),  names(model_list[model])] <- round(temp_res[[2,1]], 3)
      
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.10, 1, 0) 
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.05, 2, n_stars) 
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.01, 3, n_stars) 
      
      if (n_stars == 0){
        RDD_2nd_rur[which(RDD_2nd_rur$outcome == outcomes_list[[var_n]])+1,  names(model_list[model])]  <- paste0("=\"(", round(temp_res[[2,2]],3), ")\"", paste(rep("*", each = n_stars), collapse = ""))
      }
      else{
        RDD_2nd_rur[which(RDD_2nd_rur$outcome == outcomes_list[[var_n]])+1,  names(model_list[model])]  <- paste0("(", round(temp_res[[2,2]],3), ")", paste(rep("*", each = n_stars), collapse = ""))
      }
      RDD_2nd_rur[which(RDD_2nd_rur$outcome == outcomes_list[[var_n]])+2,  names(model_list[model])]  <- paste0("=\"(", round(temp_res[["post",4]],3), ")\"", collapse = "") #p-value
      
      
    })
  }
}


##### 2nd: by gender ####
RDD_2nd_male <- data.frame(outcome = unlist(outcomes))
RDD_2nd_female <- data.frame(outcome = unlist(outcomes))


for (var in names(model_list)){
  RDD_2nd_male[, var] <- NA
}

for (var in names(model_list)){
  RDD_2nd_male[, var] <- NA
}


####MALE
for (var_n in 1:length(outcomes_list)){
  for (model in 1:length(model_list)){
    
    reg_model <- paste(outcomes_list[[var_n]], model_list[[model]], sep = " ~ " )
    time_window <- as.numeric(substr(names(model_list[model]),  2, 3))
    
    try({
      temp_model <- lm(reg_model, data = data.2nd_short[data.2nd_short$male == 1 & data.2nd_short$birth_timeline <= time_window & 
                                                          data.2nd_short$birth_timeline >= -time_window & data.2nd_short$year %in% 2010:2017,  ]) 
      cov_matrix <-  vcovCL(temp_model, cluster = temp_model$region.x) #clustered  estimated covariance matrix
      temp_res <- coeftest(temp_model, cov_matrix)
      
      RDD_2nd_male[which(RDD_2nd_male$outcome == outcomes_list[[var_n]]),  names(model_list[model])] <- round(temp_res[[2,1]], 3)
      
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.10, 1, 0) 
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.05, 2, n_stars) 
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.01, 3, n_stars) 
      
      if (n_stars == 0){
        RDD_2nd_male[which(RDD_2nd_male$outcome == outcomes_list[[var_n]])+1,  names(model_list[model])]  <- paste0("=\"(", round(temp_res[[2,2]],3), ")\"", paste(rep("*", each = n_stars), collapse = ""))
      }
      else{
        RDD_2nd_male[which(RDD_2nd_male$outcome == outcomes_list[[var_n]])+1,  names(model_list[model])]  <- paste0("(", round(temp_res[[2,2]],3), ")", paste(rep("*", each = n_stars), collapse = ""))
      }
      RDD_2nd_male[which(RDD_2nd_male$outcome == outcomes_list[[var_n]])+2,  names(model_list[model])]  <- paste0("=\"(", round(temp_res[["post",4]],3), ")\"", collapse = "") #p-value
      
      
    })
  }
}


####FEMALE

for (var_n in 1:length(outcomes_list)){
  for (model in 1:length(model_list)){
    
    reg_model <- paste(outcomes_list[[var_n]], model_list[[model]], sep = " ~ " )
    time_window <- as.numeric(substr(names(model_list[model]),  2, 3))
    
    try({
      temp_model <- lm(reg_model, data = data.2nd_short[data.2nd_short$male == 0 & data.2nd_short$birth_timeline <= time_window & 
                                                          data.2nd_short$birth_timeline >= -time_window & data.2nd_short$year %in% 2010:2017,  ]) 
      cov_matrix <-  vcovCL(temp_model, cluster = temp_model$region.x) #clustered  estimated covariance matrix
      temp_res <- coeftest(temp_model, cov_matrix)
      
      RDD_2nd_female[which(RDD_2nd_female$outcome == outcomes_list[[var_n]]),  names(model_list[model])] <- round(temp_res[[2,1]], 3)
      
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.10, 1, 0) 
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.05, 2, n_stars) 
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.01, 3, n_stars) 
      
      if (n_stars == 0){
        RDD_2nd_female[which(RDD_2nd_female$outcome == outcomes_list[[var_n]])+1,  names(model_list[model])]  <- paste0("=\"(", round(temp_res[[2,2]],3), ")\"", paste(rep("*", each = n_stars), collapse = ""))
      }
      else{
        RDD_2nd_female[which(RDD_2nd_female$outcome == outcomes_list[[var_n]])+1,  names(model_list[model])]  <- paste0("(", round(temp_res[[2,2]],3), ")", paste(rep("*", each = n_stars), collapse = ""))
      }
      RDD_2nd_female[which(RDD_2nd_female$outcome == outcomes_list[[var_n]])+2,  names(model_list[model])]  <- paste0("=\"(", round(temp_res[["post",3]],4), ")\"", collapse = "") #p-value
      
      
    })
  }
}



##### 2nd: by poverty ####
RDD_2nd_poor <- data.frame(outcome = unlist(outcomes))
RDD_2nd_notpoor <- data.frame(outcome = unlist(outcomes))


for (var in names(model_list)){
  RDD_2nd_poor[, var] <- NA
}

for (var in names(model_list)){
  RDD_2nd_notpoor[, var] <- NA
}

####POOR
for (var_n in 1:length(outcomes_list)){
  for (model in 1:length(model_list)){
    
    reg_model <- paste(outcomes_list[[var_n]], model_list[[model]], sep = " ~ " )
    time_window <- as.numeric(substr(names(model_list[model]),  2, 3))
    
    try({
      temp_model <- lm(reg_model, data = data.2nd_short[data.2nd_short$poverty == 1 & data.2nd_short$birth_timeline <= time_window & 
                                                          data.2nd_short$birth_timeline >= -time_window & data.2nd_short$year %in% 2010:2017,  ]) 
      cov_matrix <-  vcovCL(temp_model, cluster = temp_model$region.x) #clustered  estimated covariance matrix
      temp_res <- coeftest(temp_model, cov_matrix)
      
      RDD_2nd_poor[which(RDD_2nd_poor$outcome == outcomes_list[[var_n]]),  names(model_list[model])] <- round(temp_res[[2,1]], 3)
      
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.10, 1, 0) 
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.05, 2, n_stars) 
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.01, 3, n_stars) 
      
      if (n_stars == 0){
        RDD_2nd_poor[which(RDD_2nd_poor$outcome == outcomes_list[[var_n]])+1,  names(model_list[model])]  <- paste0("=\"(", round(temp_res[[2,2]],3), ")\"", paste(rep("*", each = n_stars), collapse = ""))
      }
      else{
        RDD_2nd_poor[which(RDD_2nd_poor$outcome == outcomes_list[[var_n]])+1,  names(model_list[model])]  <- paste0("(", round(temp_res[[2,2]],3), ")", paste(rep("*", each = n_stars), collapse = ""))
      }
      RDD_2nd_poor[which(RDD_2nd_poor$outcome == outcomes_list[[var_n]])+2,  names(model_list[model])]  <- paste0("=\"(", round(temp_res[["post",4]],3), ")\"", collapse = "") #p-value
      
      
    })
  }
}

####NOT POOR
for (var_n in 1:length(outcomes_list)){
  for (model in 1:length(model_list)){
    
    reg_model <- paste(outcomes_list[[var_n]], model_list[[model]], sep = " ~ " )
    time_window <- as.numeric(substr(names(model_list[model]),  2, 3))
    
    try({
      temp_model <- lm(reg_model, data = data.2nd_short[data.2nd_short$poverty == 0 & data.2nd_short$birth_timeline <= time_window & 
                                                          data.2nd_short$birth_timeline >= -time_window & data.2nd_short$year %in% 2010:2017,  ]) 
      cov_matrix <-  vcovCL(temp_model, cluster = temp_model$region.x) #clustered  estimated covariance matrix
      temp_res <- coeftest(temp_model, cov_matrix)
      
      RDD_2nd_notpoor[which(RDD_2nd_notpoor$outcome == outcomes_list[[var_n]]),  names(model_list[model])] <- round(temp_res[[2,1]], 3)
      
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.10, 1, 0) 
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.05, 2, n_stars) 
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.01, 3, n_stars) 
      
      if (n_stars == 0){
        RDD_2nd_notpoor[which(RDD_2nd_notpoor$outcome == outcomes_list[[var_n]])+1,  names(model_list[model])]  <- paste0("=\"(", round(temp_res[[2,2]],3), ")\"", paste(rep("*", each = n_stars), collapse = ""))
      }
      else{
        RDD_2nd_notpoor[which(RDD_2nd_notpoor$outcome == outcomes_list[[var_n]])+1,  names(model_list[model])]  <- paste0("(", round(temp_res[[2,2]],3), ")", paste(rep("*", each = n_stars), collapse = ""))
      }
      RDD_2nd_notpoor[which(RDD_2nd_notpoor$outcome == outcomes_list[[var_n]])+2,  names(model_list[model])]  <- paste0("=\"(", round(temp_res[["post",4]],3), ")\"", collapse = "") #p-value
      
      
    })
  }
}



####LOWESS ####

dir <- "C:/Users/Lenovo/OneDrive/Documents/new projects/MC resubmission/plots/non_semi_param"
dir.create(dir, recursive = T)
setwd(dir)
n=1000

RDD_2nd_npsp <- data.frame(outcome = unlist(outcomes_list))
n = 1000

for(window in c(12,24,36)){
  for (model_type in c("np",  "sp")){
    
    RDD_2nd_npsp[, paste("RDD", window, model_type, sep = "_")] <- NA
    
    newdata_l <- data.frame(post = c(rep(0, n)),  birth_timeline= c(seq(from = -window, to = 0, length.out = n)))
    newdata_r <- data.frame(post =  rep(1, n),  birth_timeline= seq(from = 0,  to = window, length.out = n))
    
    for (var_n in 1:length(outcomes_list)){
      
      data_temp <- data.2nd_short[data.2nd_short$birth_timeline <= window & data.2nd_short$birth_timeline >= -window,  ]
      
      reg_model <- paste(outcomes_list[[var_n]], "post+ birth_timeline", sep = " ~ ")
      
      if (model_type == "np"){
        res <- loess(reg_model, span = 0.6, degree = 1, 
                     data =  data_temp)
      } else {
        res <- loess(reg_model, span = 0.6, degree = 1, parametric = "post", 
                     data =  data_temp)
      }
      
      plx_l <- predict(res, newdata = newdata_l,  se=T)
      plx_r <- predict(res, newdata = newdata_r,  se=T)
      
      effect <- round(plx_r[["fit"]][1] - plx_l[["fit"]][n], 3)
      bs_99_r <- plx_r[["fit"]][1] + qt(0.995,plx_r$df)*plx_r$se[1]
      bi_99_r <- plx_r[["fit"]][1] - qt(0.995,plx_r$df)*plx_r$se[1]
      bs_95_r <- plx_r[["fit"]][1] + qt(0.975,plx_r$df)*plx_r$se[1]
      bi_95_r <- plx_r[["fit"]][1] - qt(0.975,plx_r$df)*plx_r$se[1]
      bs_90_r <- plx_r[["fit"]][1] + qt(0.95,plx_r$df)*plx_r$se[1]
      bi_90_r <- plx_r[["fit"]][1] - qt(0.95,plx_r$df)*plx_r$se[1]
      
      bs_99_l <- plx_l[["fit"]][n] + qt(0.995,plx_l$df)*plx_l$se[n]
      bi_99_l <- plx_l[["fit"]][n] - qt(0.995,plx_l$df)*plx_l$se[n]
      bs_95_l <- plx_l[["fit"]][n] + qt(0.975,plx_l$df)*plx_l$se[n]
      bi_95_l <- plx_l[["fit"]][n] - qt(0.975,plx_l$df)*plx_l$se[n]
      bs_90_l <- plx_l[["fit"]][n] + qt(0.95,plx_l$df)*plx_l$se[n]
      bi_90_l <- plx_l[["fit"]][n] - qt(0.95,plx_l$df)*plx_l$se[n]
      
      star <- 0
      
      if (  bs_90_l < bi_90_r |   bi_90_l > bs_90_r) {
        star <- 1
      } 
      if (  bs_95_l < bi_95_r |   bi_95_l > bs_95_r) {
        star <- 2
      }
      if (  bs_99_l < bi_99_r |   bi_99_l > bs_99_r){
        star <- 3
      }
      
      
      RDD_2nd_npsp[which(RDD_2nd_npsp$outcome == outcomes_list[[var_n]]), paste("RDD", window, model_type, sep = "_")] <- paste0(effect, paste(rep("*", star), collapse = ""))
      
      
      data_temp <- data_temp[data_temp[, outcomes_list[[var_n]]]< mean(unlist(data_temp[, outcomes_list[[var_n]]]) , na.rm = T) + 1.5*sd(unlist(data_temp[, outcomes_list[[var_n]]]), na.rm = T) & 
                               data_temp[, outcomes_list[[var_n]]] > mean(unlist(data_temp[, outcomes_list[[var_n]]]), na.rm = T) - 1.5*sd(unlist(data_temp[, outcomes_list[[var_n]]]), na.rm = T), ]
      
      pdf(paste0(paste(outcomes_list[[var_n]], window, model_type,  sep = "_"), ".pdf"))
      
      if (outcomes_list[[var_n]] %in% c("health_score",  "life_satisfaction",  "loan_from_private_ind", "loan_to_private_ind")){
        plot(unlist(data_temp["birth_timeline"]), unlist(data_temp[, outcomes_list[[var_n]]]), type = "p", lty = 1, lwd = 1, col = "white", cex =0.2, 
             xlab = "birth timeline relative to Jan 2007", ylab = "Outcome")
      }  else if (max(data_temp[, outcomes_list[[var_n]]],  na.rm = T) == 1 | outcomes_list[[var_n]] %in% c("child_care_7d")){
        plot(unlist(data_temp["birth_timeline"]), unlist(data_temp[, outcomes_list[[var_n]]]), type = "p", lty = 1, lwd = 1, col = "white", cex =0.2, 
             xlab = "birth timeline relative to Jan 2007", ylab = "Outcome", ylim=c(0,1))
      } else{
        plot(unlist(data_temp["birth_timeline"]), unlist(data_temp[, outcomes_list[[var_n]]]), type = "p", lty = 1, lwd = 1, col = "tomato2", cex =0.2, 
             xlab = "birth timeline relative to Jan 2007", ylab = "Outcome")
      }
      
      
      lines(newdata_l[, 2], plx_l$fit, type = "l", lty = 1, col= 33, lwd= 2.5) 
      lines(newdata_l[, 2],plx_l$fit - qt(0.995,plx_l$df)*plx_l$se, lty=4,  lwd= 2.0,  col="steelblue3")
      lines(newdata_l[, 2],plx_l$fit + qt(0.995,plx_l$df)*plx_l$se, lty=4,  lwd= 2.0,  col="steelblue3")
      #lines(newdata[, 2],plx$fit - qt(0.975,plx$df)*plx$se, lty=2,  lwd= 1.5)
      #lines(newdata[, 2],plx$fit + qt(0.975,plx$df)*plx$se, lty=2,  lwd= 1.5)
      lines(newdata_l[, 2],plx_l$fit - qt(0.95,plx_l$df)*plx_l$se, lty=2,  lwd= 2.5,  col="slategray3")
      lines(newdata_l[, 2],plx_l$fit + qt(0.95,plx_l$df)*plx_l$se, lty=2,  lwd= 2.5,  col="slategray3")
      
      
      lines(newdata_r[, 2], plx_r$fit, type = "l", lty = 1, col= 33, lwd= 2.5) 
      lines(newdata_r[, 2],plx_r$fit - qt(0.995,plx_r$df)*plx_r$se, lty=4,  lwd= 2.0,  col="steelblue3")
      lines(newdata_r[, 2],plx_r$fit + qt(0.995,plx_r$df)*plx_r$se, lty=4,  lwd= 2.0,  col="steelblue3")
      #lines(newdata[, 2],plx$fit - qt(0.975,plx$df)*plx$se, lty=2,  lwd= 1.5)
      #lines(newdata[, 2],plx$fit + qt(0.975,plx$df)*plx$se, lty=2,  lwd= 1.5)
      lines(newdata_r[, 2],plx_r$fit - qt(0.95,plx_r$df)*plx_r$se, lty=2,  lwd= 2.5,  col="slategray3")
      lines(newdata_r[, 2],plx_r$fit + qt(0.95,plx_r$df)*plx_r$se, lty=2,  lwd= 2.5,  col="slategray3")
      
      legend(x = "topright",    legend=c("90% conf.", "99% conf."),
             col=c("steelblue3", "slategray3"), lty=c(4, 2), cex=0.8,  lwd= 2.5)
      
      dev.off()
      
    }  
  }
}

RDD_2nd_npsp$Relative_effect <- ""
temp_data <- data.2nd_short

for (var_n in 1:length(outcomes_list)){
  av_12 <- mean(as.numeric(unlist(temp_data[ temp_data$birth_timeline <= 12 & 
                                               temp_data$birth_timeline >= -12 & temp_data$MC_eligible == 0,  outcomes_list[[var_n]]])), na.rm = T)
  av_24 <- mean(as.numeric(unlist(temp_data[ temp_data$birth_timeline <= 24 & 
                                               temp_data$birth_timeline >= -24 & temp_data$MC_eligible == 0,  outcomes_list[[var_n]]])), na.rm = T)
  av_36 <- mean(as.numeric(unlist(temp_data[ temp_data$birth_timeline <= 36 & 
                                               temp_data$birth_timeline >= -36 & temp_data$MC_eligible == 0,  outcomes_list[[var_n]]])), na.rm = T)
  mean_vect <- c(av_12, av_12) 
  coef_vect <- as.numeric(unlist(RDD_2nd_npsp[which(RDD_2nd_npsp$outcome == outcomes_list[[var_n]]), 2:3])) #(ncol(RDD_2nd_rel)-1)
  rel_effect <- mean((coef_vect*100/mean_vect))
  
  RDD_2nd_npsp[which(RDD_2nd_npsp$outcome == outcomes_list[[var_n]]), "Relative_effect"] <- paste0(round(rel_effect, 1), "%")
}

dir <- "C:/Users/Lenovo/OneDrive/Documents/new projects/MC resubmission/resubmission Fall 2022"
setwd(dir)
RDD_2nd_npsp_rel <- table_relabel(RDD_2nd_npsp)

write.csv(RDD_2nd_npsp_rel, "RDD_2nd_npsp_rel.csv")

####LOWESS by gender ####

RDD_npsp_2nd_fem <- data.frame(outcome = unlist(outcomes_list))
n=1000


####FEMALE
for(window in c(24,36)){
  for (model_type in c("np",  "sp")){
    
    RDD_npsp_2nd_fem[, paste("RDD", window, model_type, sep = "_")] <- NA
    
    newdata_l <- data.frame(post = c(rep(0, n)),  birth_timeline= c(seq(from = -window, to = 0, length.out = n)))
    newdata_r <- data.frame(post =  rep(1, n),  birth_timeline= seq(from = 0,  to = window, length.out = n))
    
    for (var_n in 1:length(outcomes_list)){
      
      data_temp <- data.2nd_short[data.2nd_short$birth_timeline <= window & data.2nd_short$birth_timeline >= -window & 
                                  data.2nd_short$female == 1,  ]
      
      reg_model <- paste(outcomes_list[[var_n]], "post+ birth_timeline", sep = " ~ ")
      
      if (model_type == "np"){
        res <- loess(reg_model, span = 0.6, degree = 1, 
                     data =  data_temp)
      } else {
        res <- loess(reg_model, span = 0.6, degree = 1, parametric = "post", 
                     data =  data_temp)
      }
      
      
      
      plx_l <- predict(res, newdata = newdata_l,  se=T)
      plx_r <- predict(res, newdata = newdata_r,  se=T)
      
      effect <- round(plx_r[["fit"]][1] - plx_l[["fit"]][n], 3)
      bs_99_r <- plx_r[["fit"]][1] + qt(0.995,plx_r$df)*plx_r$se[1]
      bi_99_r <- plx_r[["fit"]][1] - qt(0.995,plx_r$df)*plx_r$se[1]
      bs_95_r <- plx_r[["fit"]][1] + qt(0.975,plx_r$df)*plx_r$se[1]
      bi_95_r <- plx_r[["fit"]][1] - qt(0.975,plx_r$df)*plx_r$se[1]
      bs_90_r <- plx_r[["fit"]][1] + qt(0.95,plx_r$df)*plx_r$se[1]
      bi_90_r <- plx_r[["fit"]][1] - qt(0.95,plx_r$df)*plx_r$se[1]
      
      bs_99_l <- plx_l[["fit"]][n] + qt(0.995,plx_l$df)*plx_l$se[n]
      bi_99_l <- plx_l[["fit"]][n] - qt(0.995,plx_l$df)*plx_l$se[n]
      bs_95_l <- plx_l[["fit"]][n] + qt(0.975,plx_l$df)*plx_l$se[n]
      bi_95_l <- plx_l[["fit"]][n] - qt(0.975,plx_l$df)*plx_l$se[n]
      bs_90_l <- plx_l[["fit"]][n] + qt(0.95,plx_l$df)*plx_l$se[n]
      bi_90_l <- plx_l[["fit"]][n] - qt(0.95,plx_l$df)*plx_l$se[n]
      
      star <- 0
      
      if (  bs_90_l < bi_90_r |   bi_90_l > bs_90_r) {
        star <- 1
      } 
      if (  bs_95_l < bi_95_r |   bi_95_l > bs_95_r) {
        star <- 2
      }
      if (  bs_99_l < bi_99_r |   bi_99_l > bs_99_r){
        star <- 3
      }
      
      RDD_npsp_2nd_fem[which(RDD_npsp_2nd_fem$outcome == outcomes_list[[var_n]]), paste("RDD", window, model_type, sep = "_")] <- paste0(effect, paste(rep("*", star), collapse = ""))
      
      
    }  
  }
}


####MALE

RDD_npsp_2nd_male<- data.frame(outcome = unlist(outcomes_list))
n = 1000

for(window in c(24,36)){
  for (model_type in c("np",  "sp")){
    
    RDD_npsp_2nd_male[, paste("RDD", window, model_type, sep = "_")] <- NA
    
    newdata_l <- data.frame(post = c(rep(0, n)),  birth_timeline= c(seq(from = -window, to = 0, length.out = n)))
    newdata_r <- data.frame(post =  rep(1, n),  birth_timeline= seq(from = 0,  to = window, length.out = n))
    
    for (var_n in 1:length(outcomes_list)){
      
      data_temp <- data.2nd_short[data.2nd_short$birth_timeline <= window & data.2nd_short$birth_timeline >= -window & 
                                    data.2nd_short$female == 0,  ]
      
      reg_model <- paste(outcomes_list[[var_n]], "post+ birth_timeline", sep = " ~ ")
      
      if (model_type == "np"){
        res <- loess(reg_model, span = 0.6, degree = 1, 
                     data =  data_temp)
      } else {
        res <- loess(reg_model, span = 0.6, degree = 1, parametric = "post", 
                     data =  data_temp)
      }
      
      
      
      plx_l <- predict(res, newdata = newdata_l,  se=T)
      plx_r <- predict(res, newdata = newdata_r,  se=T)
      
      effect <- round(plx_r[["fit"]][1] - plx_l[["fit"]][n], 3)
      bs_99_r <- plx_r[["fit"]][1] + qt(0.995,plx_r$df)*plx_r$se[1]
      bi_99_r <- plx_r[["fit"]][1] - qt(0.995,plx_r$df)*plx_r$se[1]
      bs_95_r <- plx_r[["fit"]][1] + qt(0.975,plx_r$df)*plx_r$se[1]
      bi_95_r <- plx_r[["fit"]][1] - qt(0.975,plx_r$df)*plx_r$se[1]
      bs_90_r <- plx_r[["fit"]][1] + qt(0.95,plx_r$df)*plx_r$se[1]
      bi_90_r <- plx_r[["fit"]][1] - qt(0.95,plx_r$df)*plx_r$se[1]
      
      bs_99_l <- plx_l[["fit"]][n] + qt(0.995,plx_l$df)*plx_l$se[n]
      bi_99_l <- plx_l[["fit"]][n] - qt(0.995,plx_l$df)*plx_l$se[n]
      bs_95_l <- plx_l[["fit"]][n] + qt(0.975,plx_l$df)*plx_l$se[n]
      bi_95_l <- plx_l[["fit"]][n] - qt(0.975,plx_l$df)*plx_l$se[n]
      bs_90_l <- plx_l[["fit"]][n] + qt(0.95,plx_l$df)*plx_l$se[n]
      bi_90_l <- plx_l[["fit"]][n] - qt(0.95,plx_l$df)*plx_l$se[n]
      
      star <- 0
      
      if (  bs_90_l < bi_90_r |   bi_90_l > bs_90_r) {
        star <- 1
      } 
      if (  bs_95_l < bi_95_r |   bi_95_l > bs_95_r) {
        star <- 2
      }
      if (  bs_99_l < bi_99_r |   bi_99_l > bs_99_r){
        star <- 3
      }
      
      RDD_npsp_2nd_male[which(RDD_npsp_2nd_male$outcome == outcomes_list[[var_n]]), paste("RDD", window, model_type, sep = "_")] <- paste0(effect, paste(rep("*", star), collapse = ""))
      
      
    }  
  }
}


####LOWESS by residence ####
####RURAL

RDD_npsp_2nd_rur <- data.frame(outcome = unlist(outcomes_list))
n = 1000

for(window in c(24,36)){
  for (model_type in c("np",  "sp")){
    
    RDD_npsp_2nd_rur[, paste("RDD", window, model_type, sep = "_")] <- NA
    
    newdata_l <- data.frame(post = c(rep(0, n)),  birth_timeline= c(seq(from = -window, to = 0, length.out = n)))
    newdata_r <- data.frame(post =  rep(1, n),  birth_timeline= seq(from = 0,  to = window, length.out = n))
    
    for (var_n in 1:length(outcomes_list)){
      
      data_temp <- data.2nd_short[data.2nd_short$birth_timeline <= window & data.2nd_short$birth_timeline >= -window & 
                                    data.2nd_short$urban == 0,  ]
      
      reg_model <- paste(outcomes_list[[var_n]], "post+ birth_timeline", sep = " ~ ")
      
      if (model_type == "np"){
        res <- loess(reg_model, span = 0.6, degree = 1, 
                     data =  data_temp)
      } else {
        res <- loess(reg_model, span = 0.6, degree = 1, parametric = "post", 
                     data =  data_temp)
      }
      
      
      
      plx_l <- predict(res, newdata = newdata_l,  se=T)
      plx_r <- predict(res, newdata = newdata_r,  se=T)
      
      effect <- round(plx_r[["fit"]][1] - plx_l[["fit"]][n], 3)
      bs_99_r <- plx_r[["fit"]][1] + qt(0.995,plx_r$df)*plx_r$se[1]
      bi_99_r <- plx_r[["fit"]][1] - qt(0.995,plx_r$df)*plx_r$se[1]
      bs_95_r <- plx_r[["fit"]][1] + qt(0.975,plx_r$df)*plx_r$se[1]
      bi_95_r <- plx_r[["fit"]][1] - qt(0.975,plx_r$df)*plx_r$se[1]
      bs_90_r <- plx_r[["fit"]][1] + qt(0.95,plx_r$df)*plx_r$se[1]
      bi_90_r <- plx_r[["fit"]][1] - qt(0.95,plx_r$df)*plx_r$se[1]
      
      bs_99_l <- plx_l[["fit"]][n] + qt(0.995,plx_l$df)*plx_l$se[n]
      bi_99_l <- plx_l[["fit"]][n] - qt(0.995,plx_l$df)*plx_l$se[n]
      bs_95_l <- plx_l[["fit"]][n] + qt(0.975,plx_l$df)*plx_l$se[n]
      bi_95_l <- plx_l[["fit"]][n] - qt(0.975,plx_l$df)*plx_l$se[n]
      bs_90_l <- plx_l[["fit"]][n] + qt(0.95,plx_l$df)*plx_l$se[n]
      bi_90_l <- plx_l[["fit"]][n] - qt(0.95,plx_l$df)*plx_l$se[n]
      
      star <- 0
      
      if (  bs_90_l < bi_90_r |   bi_90_l > bs_90_r) {
        star <- 1
      } 
      if (  bs_95_l < bi_95_r |   bi_95_l > bs_95_r) {
        star <- 2
      }
      if (  bs_99_l < bi_99_r |   bi_99_l > bs_99_r){
        star <- 3
      }
      
      RDD_npsp_2nd_rur[which(RDD_npsp_2nd_rur$outcome == outcomes_list[[var_n]]), paste("RDD", window, model_type, sep = "_")] <- paste0(effect, paste(rep("*", star), collapse = ""))
      
      
    }  
  }
}


####URBAN
RDD_npsp_2nd_urb <- data.frame(outcome = unlist(outcomes_list))
n = 1000

for(window in c(24,36)){
  for (model_type in c("np",  "sp")){
    
    RDD_npsp_2nd_urb[, paste("RDD", window, model_type, sep = "_")] <- NA
    
    newdata_l <- data.frame(post = c(rep(0, n)),  birth_timeline= c(seq(from = -window, to = 0, length.out = n)))
    newdata_r <- data.frame(post =  rep(1, n),  birth_timeline= seq(from = 0,  to = window, length.out = n))
    
    for (var_n in 1:length(outcomes_list)){
      
      data_temp <- data.2nd_short[data.2nd_short$birth_timeline <= window & data.2nd_short$birth_timeline >= -window & 
                                    data.2nd_short$urban == 1,  ]
      
      reg_model <- paste(outcomes_list[[var_n]], "post+ birth_timeline", sep = " ~ ")
      
      if (model_type == "np"){
        res <- loess(reg_model, span = 0.6, degree = 1, 
                     data =  data_temp)
      } else {
        res <- loess(reg_model, span = 0.6, degree = 1, parametric = "post", 
                     data =  data_temp)
      }
      
      
      
      plx_l <- predict(res, newdata = newdata_l,  se=T)
      plx_r <- predict(res, newdata = newdata_r,  se=T)
      
      effect <- round(plx_r[["fit"]][1] - plx_l[["fit"]][n], 3)
      bs_99_r <- plx_r[["fit"]][1] + qt(0.995,plx_r$df)*plx_r$se[1]
      bi_99_r <- plx_r[["fit"]][1] - qt(0.995,plx_r$df)*plx_r$se[1]
      bs_95_r <- plx_r[["fit"]][1] + qt(0.975,plx_r$df)*plx_r$se[1]
      bi_95_r <- plx_r[["fit"]][1] - qt(0.975,plx_r$df)*plx_r$se[1]
      bs_90_r <- plx_r[["fit"]][1] + qt(0.95,plx_r$df)*plx_r$se[1]
      bi_90_r <- plx_r[["fit"]][1] - qt(0.95,plx_r$df)*plx_r$se[1]
      
      bs_99_l <- plx_l[["fit"]][n] + qt(0.995,plx_l$df)*plx_l$se[n]
      bi_99_l <- plx_l[["fit"]][n] - qt(0.995,plx_l$df)*plx_l$se[n]
      bs_95_l <- plx_l[["fit"]][n] + qt(0.975,plx_l$df)*plx_l$se[n]
      bi_95_l <- plx_l[["fit"]][n] - qt(0.975,plx_l$df)*plx_l$se[n]
      bs_90_l <- plx_l[["fit"]][n] + qt(0.95,plx_l$df)*plx_l$se[n]
      bi_90_l <- plx_l[["fit"]][n] - qt(0.95,plx_l$df)*plx_l$se[n]
      
      star <- 0
      
      if (  bs_90_l < bi_90_r |   bi_90_l > bs_90_r) {
        star <- 1
      } 
      if (  bs_95_l < bi_95_r |   bi_95_l > bs_95_r) {
        star <- 2
      }
      if (  bs_99_l < bi_99_r |   bi_99_l > bs_99_r){
        star <- 3
      }
      
      RDD_npsp_2nd_urb[which(RDD_npsp_2nd_urb$outcome == outcomes_list[[var_n]]), paste("RDD", window, model_type, sep = "_")] <- paste0(effect, paste(rep("*", star), collapse = ""))

    }  
  }
}


####LOWESS by poverty status ####

RDD_npsp_2nd_poor <- data.frame(outcome = unlist(outcomes_list))
n = 1000

RDD_npsp_2nd_notpoor <- data.frame(outcome = unlist(outcomes_list))
n = 1000


####POOR

for(window in c(24,36)){
  for (model_type in c("np",  "sp")){
    
    RDD_npsp_2nd_poor[, paste("RDD", window, model_type, sep = "_")] <- NA
    
    newdata_l <- data.frame(post = c(rep(0, n)),  birth_timeline= c(seq(from = -window, to = 0, length.out = n)))
    newdata_r <- data.frame(post =  rep(1, n),  birth_timeline= seq(from = 0,  to = window, length.out = n))
    
    for (var_n in 1:length(outcomes_list)){
      
      data_temp <- data.2nd_short[data.2nd_short$birth_timeline <= window & data.2nd_short$birth_timeline >= -window & 
                                    data.2nd_short$poverty == 1,  ]
      
      reg_model <- paste(outcomes_list[[var_n]], "post+ birth_timeline", sep = " ~ ")
      
      if (model_type == "np"){
        res <- loess(reg_model, span = 0.6, degree = 1, 
                     data =  data_temp)
      } else {
        res <- loess(reg_model, span = 0.6, degree = 1, parametric = "post", 
                     data =  data_temp)
      }
      
      
      
      plx_l <- predict(res, newdata = newdata_l,  se=T)
      plx_r <- predict(res, newdata = newdata_r,  se=T)
      
      effect <- round(plx_r[["fit"]][1] - plx_l[["fit"]][n], 3)
      bs_99_r <- plx_r[["fit"]][1] + qt(0.995,plx_r$df)*plx_r$se[1]
      bi_99_r <- plx_r[["fit"]][1] - qt(0.995,plx_r$df)*plx_r$se[1]
      bs_95_r <- plx_r[["fit"]][1] + qt(0.975,plx_r$df)*plx_r$se[1]
      bi_95_r <- plx_r[["fit"]][1] - qt(0.975,plx_r$df)*plx_r$se[1]
      bs_90_r <- plx_r[["fit"]][1] + qt(0.95,plx_r$df)*plx_r$se[1]
      bi_90_r <- plx_r[["fit"]][1] - qt(0.95,plx_r$df)*plx_r$se[1]
      
      bs_99_l <- plx_l[["fit"]][n] + qt(0.995,plx_l$df)*plx_l$se[n]
      bi_99_l <- plx_l[["fit"]][n] - qt(0.995,plx_l$df)*plx_l$se[n]
      bs_95_l <- plx_l[["fit"]][n] + qt(0.975,plx_l$df)*plx_l$se[n]
      bi_95_l <- plx_l[["fit"]][n] - qt(0.975,plx_l$df)*plx_l$se[n]
      bs_90_l <- plx_l[["fit"]][n] + qt(0.95,plx_l$df)*plx_l$se[n]
      bi_90_l <- plx_l[["fit"]][n] - qt(0.95,plx_l$df)*plx_l$se[n]
      
      star <- 0
      
      if (  bs_90_l < bi_90_r |   bi_90_l > bs_90_r) {
        star <- 1
      } 
      if (  bs_95_l < bi_95_r |   bi_95_l > bs_95_r) {
        star <- 2
      }
      if (  bs_99_l < bi_99_r |   bi_99_l > bs_99_r){
        star <- 3
      }
      
      RDD_npsp_2nd_poor[which(RDD_npsp_2nd_poor$outcome == outcomes_list[[var_n]]), paste("RDD", window, model_type, sep = "_")] <- paste0(effect, paste(rep("*", star), collapse = ""))
      
    }  
  }
}


####NOT POOR

RDD_npsp_2nd_notpoor <- data.frame(outcome = unlist(outcomes_list))
n = 1000

for(window in c(24,36)){
  for (model_type in c("np",  "sp")){
    
    RDD_npsp_2nd_notpoor[, paste("RDD", window, model_type, sep = "_")] <- NA
    
    newdata_l <- data.frame(post = c(rep(0, n)),  birth_timeline= c(seq(from = -window, to = 0, length.out = n)))
    newdata_r <- data.frame(post =  rep(1, n),  birth_timeline= seq(from = 0,  to = window, length.out = n))
    
    for (var_n in 1:length(outcomes_list)){
      
      data_temp <- data.2nd_short[data.2nd_short$birth_timeline <= window & data.2nd_short$birth_timeline >= -window & 
                                    data.2nd_short$poverty == 0,  ]
      
      reg_model <- paste(outcomes_list[[var_n]], "post+ birth_timeline", sep = " ~ ")
      
      if (model_type == "np"){
        res <- loess(reg_model, span = 0.6, degree = 1, 
                     data =  data_temp)
      } else {
        res <- loess(reg_model, span = 0.6, degree = 1, parametric = "post", 
                     data =  data_temp)
      }
      
      
      
      plx_l <- predict(res, newdata = newdata_l,  se=T)
      plx_r <- predict(res, newdata = newdata_r,  se=T)
      
      effect <- round(plx_r[["fit"]][1] - plx_l[["fit"]][n], 3)
      bs_99_r <- plx_r[["fit"]][1] + qt(0.995,plx_r$df)*plx_r$se[1]
      bi_99_r <- plx_r[["fit"]][1] - qt(0.995,plx_r$df)*plx_r$se[1]
      bs_95_r <- plx_r[["fit"]][1] + qt(0.975,plx_r$df)*plx_r$se[1]
      bi_95_r <- plx_r[["fit"]][1] - qt(0.975,plx_r$df)*plx_r$se[1]
      bs_90_r <- plx_r[["fit"]][1] + qt(0.95,plx_r$df)*plx_r$se[1]
      bi_90_r <- plx_r[["fit"]][1] - qt(0.95,plx_r$df)*plx_r$se[1]
      
      bs_99_l <- plx_l[["fit"]][n] + qt(0.995,plx_l$df)*plx_l$se[n]
      bi_99_l <- plx_l[["fit"]][n] - qt(0.995,plx_l$df)*plx_l$se[n]
      bs_95_l <- plx_l[["fit"]][n] + qt(0.975,plx_l$df)*plx_l$se[n]
      bi_95_l <- plx_l[["fit"]][n] - qt(0.975,plx_l$df)*plx_l$se[n]
      bs_90_l <- plx_l[["fit"]][n] + qt(0.95,plx_l$df)*plx_l$se[n]
      bi_90_l <- plx_l[["fit"]][n] - qt(0.95,plx_l$df)*plx_l$se[n]
      
      star <- 0
      
      if (  bs_90_l < bi_90_r |   bi_90_l > bs_90_r) {
        star <- 1
      } 
      if (  bs_95_l < bi_95_r |   bi_95_l > bs_95_r) {
        star <- 2
      }
      if (  bs_99_l < bi_99_r |   bi_99_l > bs_99_r){
        star <- 3
      }
      
      RDD_npsp_2nd_notpoor[which(RDD_npsp_2nd_notpoor$outcome == outcomes_list[[var_n]]), paste("RDD", window, model_type, sep = "_")] <- paste0(effect, paste(rep("*", star), collapse = ""))
      
    }  
  }
}


####F-tests####

###URBAN/RURAL
model_list_test <- list()


delete_column <- function(x){
  if(substr(x, nchar(x), nchar(x)) == ":"){
    x = substr(x, 1, nchar(x)-1)
  }
  return(x)
}

for (wind in c(12, 24, 36)){
  for (e.var in 1:length(covariates_list[["e"]])){
    for (c.var in 1:length(covariates_list[["c"]])){
      for (t.var in 1:length(covariates_list[["t"]])){
        
        model_label <- paste0("w", wind, ".", "e", e.var, "c", c.var, "t", t.var)
        model_covariates <- unlist(list(paste0("urban:", unlist(covariates_list[["e"]][e.var])),
                                        covariates_list[["e"]][e.var],
                                        paste0("urban:", unlist(covariates_list[["c"]][c.var])), 
                                        covariates_list[["c"]][c.var],
                                        paste0("urban:", unlist(covariates_list[["t"]][t.var])), 
                                        covariates_list[["t"]][t.var]))
        model_covariates <- sapply(model_covariates, delete_column)
        model_covariates <- model_covariates[model_covariates != ""]
        model_list_test[model_label] <- paste(model_covariates, collapse = " + ")
      }
    }
  }
}

regressors <- model_list_test

outcomes_test <- rep(unlist(outcomes_list), each =3)
outcomes_test[1:length(outcomes_test)%%3 != 1] <- ""

RDD_2nd_test_urbrur <- data.frame(outcome = outcomes_test)

for (var in names(regressors)){
  RDD_2nd_test_urbrur[, var] <- NA
}

for (var_n in 1:length(outcomes_list)){
  for (regr in 1:length(regressors)){
    
    
    reg_model <- paste(outcomes_list[[var_n]], regressors[[regr]], sep = " ~ ")
    time_window <- as.numeric(substr(names(model_list_test[regr]),  2, 3))
    
    try({
      temp_model <- lm(reg_model,
                       data = data.2nd_short[data.2nd_short$birth_timeline <= time_window & data.2nd_short$birth_timeline >= -time_window,  ]) 
      cov_matrix <-  vcovCL(temp_model, cluster = temp_model$region_m) #clustered  estimated covariance matrix
      temp_res <- coeftest(temp_model, cov_matrix)
      
      RDD_2nd_test_urbrur[which(RDD_2nd_test_urbrur$outcome == outcomes_list[[var_n]]),  names(regressors[regr])] <- round(temp_res[["urban:post",1]],3)
      n_stars <- ifelse(round(temp_res[["urban:post",4]], 4)< 0.10, 1, 0) 
      n_stars <- ifelse(round(temp_res[["urban:post",4]], 4)< 0.05, 2, n_stars) 
      n_stars <- ifelse(round(temp_res[["urban:post",4]], 4)< 0.01, 3, n_stars) 
      
      if (n_stars == 0){
        RDD_2nd_test_urbrur[which(RDD_2nd_test_urbrur$outcome == outcomes_list[[var_n]])+1,  names(regressors[regr])]  <- paste0("=\"(", round(temp_res[["urban:post",2]],3), ")\"", paste(rep("*", each = n_stars), collapse = ""))
      }
      else{
        RDD_2nd_test_urbrur[which(RDD_2nd_test_urbrur$outcome == outcomes_list[[var_n]])+1,  names(regressors[regr])]  <- paste0("(", round(temp_res[["urban:post",2]],3), ")", paste(rep("*", each = n_stars), collapse = ""))
      }
      
      RDD_2nd_test_urbrur[which(RDD_2nd_test_urbrur$outcome == outcomes_list[[var_n]])+2,  names(regressors[regr])]  <- paste0("=\"(", round(temp_res[["urban:post",4]],3), ")\"", collapse = "")
      
      
      
    })
  }
}

###BY POVERTY 
model_list_test <- list()

for (wind in c(12, 24, 36)){
  for (e.var in 1:length(covariates_list[["e"]])){
    for (c.var in 1:length(covariates_list[["c"]])){
      for (t.var in 1:length(covariates_list[["t"]])){
        
        model_label <- paste0("w", wind, ".", "e", e.var, "c", c.var, "t", t.var)
        model_covariates <- unlist(list(paste0("poverty:", unlist(covariates_list[["e"]][e.var])),
                                        covariates_list[["e"]][e.var],
                                        paste0("poverty:", unlist(covariates_list[["c"]][c.var])), 
                                        covariates_list[["c"]][c.var],
                                        paste0("poverty:", unlist(covariates_list[["t"]][t.var])), 
                                        covariates_list[["t"]][t.var]))
        model_covariates <- model_covariates[model_covariates != ""]
        model_covariates <- sapply(model_covariates, delete_column)
        model_list_test[model_label] <- paste(model_covariates, collapse = " + ")
      }
    }
  }
}

regressors <- model_list_test

outcomes_test <- rep(unlist(outcomes_list), each =3)
outcomes_test[1:length(outcomes_test)%%3 != 1] <- ""

RDD_2nd_test_poverty <- data.frame(outcome = outcomes_test)

for (var in names(regressors)){
  RDD_2nd_test_poverty[, var] <- NA
}


for (var_n in 1:length(outcomes_list)){
  for (regr in 1:length(regressors)){
    
    
    reg_model <- paste(outcomes_list[[var_n]], regressors[[regr]], sep = " ~ ")
    time_window <- as.numeric(substr(names(model_list_test[regr]),  2, 3))
    
    try({
      temp_model <- lm(reg_model,
                       data = data.2nd_short[data.2nd_short$birth_timeline <= time_window & data.2nd_short$birth_timeline >= -time_window,  ]) 
      cov_matrix <-  vcovCL(temp_model, cluster = temp_model$region_m) #clustered  estimated covariance matrix
      temp_res <- coeftest(temp_model, cov_matrix)
      
      RDD_2nd_test_poverty[which(RDD_2nd_test_poverty$outcome == outcomes_list[[var_n]]),  names(regressors[regr])] <- round(temp_res[["poverty:post",1]],3)
      n_stars <- ifelse(round(temp_res[["poverty:post",4]], 4)< 0.10, 1, 0) 
      n_stars <- ifelse(round(temp_res[["poverty:post",4]], 4)< 0.05, 2, n_stars) 
      n_stars <- ifelse(round(temp_res[["poverty:post",4]], 4)< 0.01, 3, n_stars) 
      
      if (n_stars == 0){
        RDD_2nd_test_poverty[which(RDD_2nd_test_poverty$outcome == outcomes_list[[var_n]])+1,  names(regressors[regr])]  <- paste0("=\"(",  round(temp_res[["poverty:post",2]],3), ")\"", paste(rep("*", each = n_stars), collapse = ""))
      }
      else{
        RDD_2nd_test_poverty[which(RDD_2nd_test_poverty$outcome == outcomes_list[[var_n]])+1,  names(regressors[regr])]  <- paste0("(",  round(temp_res[["poverty:post",2]],3), ")", paste(rep("*", each = n_stars), collapse = ""))
      }
      
      RDD_2nd_test_poverty[which(RDD_2nd_test_poverty$outcome == outcomes_list[[var_n]])+2,  names(regressors[regr])]  <- paste0("=\"(", round(temp_res[["poverty:post",4]],3), ")\"", collapse = "")
      
      
      #RDD_2nd_test_poverty[which(RDD_2nd_test_poverty$outcome == outcomes_list[[var_n]])+1,  names(regressors[regr])]  <- paste0("(", round(temp_res[["poverty:post",2]],3), ")", paste(rep("*", each = n_stars), collapse = ""))
      #RDD_2nd_test_poverty[which(RDD_2nd_test_poverty$outcome == outcomes_list[[var_n]])+2,  names(regressors[regr])]  <- paste0("(", round(temp_res[["poverty:post",4]],3), ")", collapse = "")
      
      
    })
  }
}
View(RDD_2nd_test_gender[c("outcome", "w12.e1c2t2")])

###BY GENDER 
model_list_test <- list()

for (wind in c(12, 24, 36)){
  for (e.var in 1:length(covariates_list[["e"]])){
    for (c.var in 1:length(covariates_list[["c"]])){
      for (t.var in 1:length(covariates_list[["t"]])){
        
        model_label <- paste0("w", wind, ".", "e", e.var, "c", c.var, "t", t.var)
        model_covariates <- unlist(list(paste0("male:", unlist(covariates_list[["e"]][e.var])),
                                        covariates_list[["e"]][e.var],
                                        paste0("male:", unlist(covariates_list[["c"]][c.var])), 
                                        covariates_list[["c"]][c.var],
                                        paste0("male:", unlist(covariates_list[["t"]][t.var])), 
                                        covariates_list[["t"]][t.var]))
        model_covariates <- model_covariates[model_covariates != ""]
        model_covariates <- sapply(model_covariates, delete_column)
        model_list_test[model_label] <- paste(model_covariates, collapse = " + ")
      }
    }
  }
}

regressors <- model_list_test

outcomes_test <- rep(unlist(outcomes_list), each =3)
outcomes_test[1:length(outcomes_test)%%3 != 1] <- ""

RDD_2nd_test_gender <- data.frame(outcome = outcomes_test)

for (var in names(regressors)){
  RDD_2nd_test_gender[, var] <- NA
}


for (var_n in 1:length(outcomes_list)){
  for (regr in 1:length(regressors)){
    
    
    reg_model <- paste(outcomes_list[[var_n]], regressors[[regr]], sep = " ~ ")
    time_window <- as.numeric(substr(names(model_list_test[regr]),  2, 3))
    
    try({
      temp_model <- lm(reg_model,
                       data = data.2nd_short[data.2nd_short$birth_timeline <= time_window & data.2nd_short$birth_timeline >= -time_window,  ]) 
      cov_matrix <-  vcovCL(temp_model, cluster = temp_model$region_m) #clustered  estimated covariance matrix
      temp_res <- coeftest(temp_model, cov_matrix)
      
      RDD_2nd_test_gender[which(RDD_2nd_test_gender$outcome == outcomes_list[[var_n]]),  names(regressors[regr])] <- round(temp_res[["male:post",1]],3)
      n_stars <- ifelse(round(temp_res[["male:post",4]], 4)< 0.10, 1, 0) 
      n_stars <- ifelse(round(temp_res[["male:post",4]], 4)< 0.05, 2, n_stars) 
      n_stars <- ifelse(round(temp_res[["male:post",4]], 4)< 0.01, 3, n_stars) 
      
      
      
      if (n_stars == 0){
        RDD_2nd_test_gender[which(RDD_2nd_test_gender$outcome == outcomes_list[[var_n]])+1,  names(regressors[regr])]  <- paste0("=\"(",  round(temp_res[["male:post",2]],3), ")\"", paste(rep("*", each = n_stars), collapse = ""))
      }
      else{
        RDD_2nd_test_gender[which(RDD_2nd_test_gender$outcome == outcomes_list[[var_n]])+1,  names(regressors[regr])]  <- paste0("(",  round(temp_res[["male:post",2]],3), ")", paste(rep("*", each = n_stars), collapse = ""))
      }
      
      RDD_2nd_test_gender[which(RDD_2nd_test_gender$outcome == outcomes_list[[var_n]])+2,  names(regressors[regr])]  <- paste0("=\"(", round(temp_res[["male:post",4]],3), ")\"", collapse = "")
      
      #RDD_2nd_test_gender[which(RDD_2nd_test_gender$outcome == outcomes_list[[var_n]])+1,  names(regressors[regr])]  <- paste0("(", round(temp_res[["male:post",2]],3), ")", paste(rep("*", each = n_stars), collapse = ""))
      #RDD_2nd_test_gender[which(RDD_2nd_test_gender$outcome == outcomes_list[[var_n]])+2,  names(regressors[regr])]  <- paste0("(", round(temp_res[["male:post",4]],3), ")", collapse = "")
      
      
    })
  }
}


####Poly-3, smooth plots,  outcomes####
dir <- "C:/Users/Lenovo/OneDrive/Documents/new projects/MC resubmission/plots/param"
dir.create(dir, recursive = T)
setwd(dir)
n=1000

for (window in c(12,24,36)){
  
  birth_timeline_l = seq(from = -window, to = 0, length.out = n)
  birth_timeline_r = seq(from = 0 , to = window, length.out = n)
  
  newdata_l <- data.frame(post = c(rep(0, n)),  birth_timeline= c(seq(from = -window, to = 0, length.out = n)),
                          birth_timeline_2 = birth_timeline_l^2,  birth_timeline_3 = birth_timeline_l^3)
  newdata_r <- data.frame(post =  rep(1, n),  birth_timeline= seq(from = 0,  to = window, length.out = n), 
                          birth_timeline_2 = birth_timeline_r^2,  birth_timeline_3 = birth_timeline_r^3)
  
  
  for (var_n in 1:length(outcomes_list)){
    
    data_temp <- data.2nd_short[data.2nd_short$birth_timeline <= window & data.2nd_short$birth_timeline >= -window,  ]
    
    
    
    reg_model_plot <- paste(outcomes_list[[var_n]], "post + birth_timeline   +  I(birth_timeline^2) + I(birth_timeline^3)", sep = " ~ ")
    temp_model <- lm(reg_model_plot,  data = data_temp) 
    
    
    plx_l_90 <- predict(temp_model, newdata = newdata_l, interval = "confidence",  level = 0.9)
    plx_r_90 <- predict(temp_model, newdata = newdata_r, interval = "confidence",  level = 0.9)
    
    plx_l_99 <- predict(temp_model, newdata = newdata_l, interval = "confidence",  level = 0.99)
    plx_r_99 <- predict(temp_model, newdata = newdata_r, interval = "confidence",  level = 0.99)
    
    
    data_temp <- data_temp[data_temp[, outcomes_list[[var_n]]]< mean(unlist(data_temp[, outcomes_list[[var_n]]]) , na.rm = T) + 1.5*sd(unlist(data_temp[, outcomes_list[[var_n]]]), na.rm = T) & 
                             data_temp[, outcomes_list[[var_n]]] > mean(unlist(data_temp[, outcomes_list[[var_n]]]), na.rm = T) - 1.5*sd(unlist(data_temp[, outcomes_list[[var_n]]]), na.rm = T), ]
    
    pdf(paste0(paste(outcomes_list[[var_n]], window,   sep = "_"), ".pdf"))
    
    if (max(data_temp[, outcomes_list[[var_n]]],  na.rm = T) == 1){
        plot(unlist(data_temp["birth_timeline"]), unlist(data_temp[, outcomes_list[[var_n]]]), type = "p", lty = 1, lwd = 1, col = "white", cex =0.2, 
        xlab = "birth timeline relative to Jan 2007", ylab = "Outcome", ylim=c(0,1))
    } 
    else{
      plot(unlist(data_temp["birth_timeline"]), unlist(data_temp[, outcomes_list[[var_n]]]), type = "p", lty = 1, lwd = 1, col = "tomato2", cex =0.2, 
           xlab = "birth timeline relative to Jan 2007", ylab = "Outcome")
    }
    
    lines(newdata_l[, 2], plx_l_90[,1], type = "l", lty = 1, col= 33, lwd= 2.5) 
    lines(newdata_l[, 2], plx_l_90[,2], lty=4,  lwd= 2.0,  col="steelblue3")
    lines(newdata_l[, 2],plx_l_90[,3], lty=4,  lwd= 2.0,  col="steelblue3")
    #lines(newdata[, 2],plx$fit - qt(0.975,plx$df)*plx$se.fit, lty=2,  lwd= 1.5)
    #lines(newdata[, 2],plx$fit + qt(0.975,plx$df)*plx$se.fit, lty=2,  lwd= 1.5)
    lines(newdata_l[, 2], plx_l_99[,2], lty=2,  lwd= 2.5,  col="slategray3")
    lines(newdata_l[, 2], plx_l_99[,3], lty=2,  lwd= 2.5,  col="slategray3")
    
    
    lines(newdata_r[, 2], plx_r_90[,1], type = "l", lty = 1, col= 33, lwd= 2.5) 
    lines(newdata_r[, 2], plx_r_90[,2], lty=4,  lwd= 2.0,  col="steelblue3")
    lines(newdata_r[, 2],plx_r_90[,3], lty=4,  lwd= 2.0,  col="steelblue3")
    #lines(newdata[, 2],plx$fit - qt(0.975,plx$df)*plx$se.fit, lty=2,  lwd= 1.5)
    #lines(newdata[, 2],plx$fit + qt(0.975,plx$df)*plx$se.fit, lty=2,  lwd= 1.5)
    lines(newdata_r[, 2], plx_r_99[,2], lty=2,  lwd= 2.5,  col="slategray3")
    lines(newdata_r[, 2], plx_r_99[,3], lty=2,  lwd= 2.5,  col="slategray3")
    legend(x = "topright",    legend=c("90% conf.", "99% conf."),
           col=c("steelblue3", "slategray3"), lty=c(4, 2), cex=0.8,  lwd= 2.5)
    
    
    dev.off()
    
  }
}



####Poly-3, smooth plots,  covariates####
dir <- "C:/Users/Lenovo/OneDrive/Documents/new projects/MC resubmission/plots/covariates/param"
dir.create(dir, recursive = T)
setwd(dir)
n=1000
covariates <- c("p.age", "non_Russian", "income_perperson", "urban",  "higher_education",  "p.health_good_exc",
                "alcohol_mother",  "n_household_memb", "poverty", "income_30d")
for (window in c(12,24,36)){
  
  birth_timeline_l = seq(from = -window, to = 0, length.out = n)
  birth_timeline_r = seq(from = 0 , to = window, length.out = n)
  
  newdata_l <- data.frame(post = c(rep(0, n)),  birth_timeline= c(seq(from = -window, to = 0, length.out = n)),
                          birth_timeline_2 = birth_timeline_l^2,  birth_timeline_3 = birth_timeline_l^3)
  newdata_r <- data.frame(post =  rep(1, n),  birth_timeline= seq(from = 0,  to = window, length.out = n), 
                          birth_timeline_2 = birth_timeline_r^2,  birth_timeline_3 = birth_timeline_r^3)
  
  
  for (var_n in 1:length(covariates)){
    
    data_temp <- data.2nd_short[data.2nd_short$birth_timeline <= window & data.2nd_short$birth_timeline >= -window,  ]
    
    
    
    reg_model_plot <- paste(covariates[[var_n]], "post + birth_timeline   +  I(birth_timeline^2) + I(birth_timeline^3)", sep = " ~ ")
    temp_model <- lm(reg_model_plot,  data = data_temp) 
    
    
    plx_l_90 <- predict(temp_model, newdata = newdata_l, interval = "confidence",  level = 0.9)
    plx_r_90 <- predict(temp_model, newdata = newdata_r, interval = "confidence",  level = 0.9)
    
    plx_l_99 <- predict(temp_model, newdata = newdata_l, interval = "confidence",  level = 0.99)
    plx_r_99 <- predict(temp_model, newdata = newdata_r, interval = "confidence",  level = 0.99)
    
    
    data_temp <- data_temp[data_temp[, covariates[[var_n]]]< mean(unlist(data_temp[, covariates[[var_n]]]) , na.rm = T) + 1.5*sd(unlist(data_temp[, covariates[[var_n]]]), na.rm = T) & 
                             data_temp[, covariates[[var_n]]] > mean(unlist(data_temp[, covariates[[var_n]]]), na.rm = T) - 1.5*sd(unlist(data_temp[, covariates[[var_n]]]), na.rm = T), ]
    
    pdf(paste0(paste(covariates[[var_n]], window,   sep = "_"), ".pdf"))
    
    if (max(data_temp[, covariates[[var_n]]],  na.rm = T) == 1){
      plot(unlist(data_temp["birth_timeline"]), unlist(data_temp[, covariates[[var_n]]]), type = "p", lty = 1, lwd = 1, col = "white", cex =0.2, 
           xlab = "birth timeline relative to Jan 2007", ylab = "Outcome", ylim=c(0,1))
    } 
    else{
      plot(unlist(data_temp["birth_timeline"]), unlist(data_temp[, covariates[[var_n]]]), type = "p", lty = 1, lwd = 1, col = "tomato2", cex =0.2, 
           xlab = "birth timeline relative to Jan 2007", ylab = "Outcome")
    }
    
    lines(newdata_l[, 2], plx_l_90[,1], type = "l", lty = 1, col= 33, lwd= 2.5) 
    lines(newdata_l[, 2], plx_l_90[,2], lty=4,  lwd= 2.0,  col="steelblue3")
    lines(newdata_l[, 2],plx_l_90[,3], lty=4,  lwd= 2.0,  col="steelblue3")
    #lines(newdata[, 2],plx$fit - qt(0.975,plx$df)*plx$se.fit, lty=2,  lwd= 1.5)
    #lines(newdata[, 2],plx$fit + qt(0.975,plx$df)*plx$se.fit, lty=2,  lwd= 1.5)
    lines(newdata_l[, 2], plx_l_99[,2], lty=2,  lwd= 2.5,  col="slategray3")
    lines(newdata_l[, 2], plx_l_99[,3], lty=2,  lwd= 2.5,  col="slategray3")
    
    
    lines(newdata_r[, 2], plx_r_90[,1], type = "l", lty = 1, col= 33, lwd= 2.5) 
    lines(newdata_r[, 2], plx_r_90[,2], lty=4,  lwd= 2.0,  col="steelblue3")
    lines(newdata_r[, 2],plx_r_90[,3], lty=4,  lwd= 2.0,  col="steelblue3")
    #lines(newdata[, 2],plx$fit - qt(0.975,plx$df)*plx$se.fit, lty=2,  lwd= 1.5)
    #lines(newdata[, 2],plx$fit + qt(0.975,plx$df)*plx$se.fit, lty=2,  lwd= 1.5)
    lines(newdata_r[, 2], plx_r_99[,2], lty=2,  lwd= 2.5,  col="slategray3")
    lines(newdata_r[, 2], plx_r_99[,3], lty=2,  lwd= 2.5,  col="slategray3")
    legend(x = "topright",    legend=c("90% conf.", "99% conf."),
           col=c("steelblue3", "slategray3"), lty=c(4, 2), cex=0.8,  lwd= 2.5)
    
    
    dev.off()
    
  }
}

####Loop RDD_2nd-COVARIATES####

covariates_list <- list() 
covariates_list[["e"]][[1]] <- c("post", "birth_timeline", "post_birth_timeline")
covariates_list[["e"]][[2]] <- c("post", "birth_timeline", "I(birth_timeline^2)", "post:birth_timeline", "post:I(birth_timeline^2)")
covariates_list[["e"]][[3]] <- c("post", "birth_timeline", "I(birth_timeline^2)", "I(birth_timeline^3)",
                                 "post:birth_timeline", "post:I(birth_timeline^2)", "post:I(birth_timeline^3)")

covariates_list[["c"]][[1]] <- c("")
covariates_list[["c"]][[2]] <- c("age_months")
covariates_list[["c"]][[3]] <- c("age_months", "I(age_months^2)", "I(age_months^3)")


covariates_list[["t"]][[1]] <- c("")
covariates_list[["t"]][[2]] <- c("factor(p.RLMS_wave)")
covariates_list[["t"]][[3]] <- c("interview_timeline", "I(interview_timeline^2)", "I(interview_timeline^3)")
covariates_list[["t"]][[4]] <- c("factor(p.RLMS_wave)", "interview_timeline", "I(interview_timeline^2)", "I(interview_timeline^3)")



model_list_covariates <- list()

for (wind in c(12, 24, 36)){
  for (e.var in 1:length(covariates_list[["e"]])){
    for (c.var in 1:length(covariates_list[["c"]])){
      for (t.var in 1:length(covariates_list[["t"]])){
        
        model_label <- paste0("w", wind, ".", "e", e.var, "c", c.var, "t", t.var)
        model_covariates <- unlist(list(covariates_list[["e"]][e.var], covariates_list[["c"]][c.var], covariates_list[["t"]][t.var]))
        model_covariates <- model_covariates[model_covariates != ""]
        model_list_covariates[model_label] <- paste(model_covariates, collapse = " + ")
      }
    }
  }
}

outcomes_covariates <- c("non_Russian","poverty",   "higher_education",  "income_perperson", "urban", "male",
                                 "p.health_good_exc", "alcohol_mother", "p.age", "n_household_memb", "single_parent" )


outcomes_covariates_axis <- rep(unlist(outcomes_covariates), each =2)
outcomes_covariates_axis[1:length(outcomes_covariates_axis) %% 2 == 0] <- ""



RDD_2nd_covariates <- data.frame(outcome = unlist(outcomes_covariates_axis))

for (var in names(model_list_covariates)){
  RDD_2nd_covariates[, var] <- NA
}

for (var_n in 1:length(outcomes_covariates)){
  for (model in 1:length(model_list_covariates)){
    
    reg_model <- paste(outcomes_covariates[[var_n]], model_list_covariates[[model]], sep = " ~ ")
    time_window <- as.numeric(substr(names(model_list_covariates[model]),  2, 3))
    
    try({
      temp_model <- lm(reg_model, data = data.2nd_short[data.2nd_short$birth_timeline <= time_window & 
                                                          data.2nd_short$birth_timeline >= -time_window & data.2nd_short$year %in% 2010:2017,  ]) 
      cov_matrix <-  vcovCL(temp_model, cluster = temp_model$region.x) #clustered  estimated covariance matrix
      temp_res <- coeftest(temp_model, cov_matrix)
      
      RDD_2nd_covariates[which(RDD_2nd_covariates$outcome == outcomes_covariates[[var_n]]),  names(model_list_covariates[model])] <- round(temp_res[[2,1]], 3)
      
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.10, 1, 0) 
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.05, 2, n_stars) 
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.01, 3, n_stars) 
      
      if (n_stars == 0){
        RDD_2nd_covariates[which(RDD_2nd_covariates$outcome == outcomes_covariates[[var_n]])+1,  names(model_list_covariates[model])]  <- paste0("=\"(", round(temp_res[[2,2]],3), ")\"", paste(rep("*", each = n_stars), collapse = ""))
      }
      else{
        RDD_2nd_covariates[which(RDD_2nd_covariates$outcome == outcomes_covariates[[var_n]])+1,  names(model_list_covariates[model])]  <- paste0("(", round(temp_res[[2,2]],3), ")", paste(rep("*", each = n_stars), collapse = ""))
      }
      
      RDD_2nd_covariates[which(RDD_2nd_covariates$outcome == outcomes_covariates[[var_n]])+2,  names(model_list_covariates[model])]  <- paste0("=\"(", round(temp_res[["post",4]],3), ")\"", collapse = "") #p-value
      
      
    })
  }
}


####LOWESS - COVARIATES####

dir <- "C:/Users/Lenovo/OneDrive/Documents/new projects/MC resubmission/plots/covariates/non_semi_param"
dir.create(dir, recursive = T)
setwd(dir)
n=1000
covariates <- c("p.age", "non_Russian", "income_perperson", "urban",  "higher_education",  "p.health_good_exc",
                "alcohol_mother",  "n_household_memb", "poverty", "income_30d", "single_parent")


RDD_2nd_npsp_covariates <- data.frame(outcome = unlist(covariates))
n = 1000

for(window in c(12,24,36)){
  for (model_type in c("np",  "sp")){
    
    RDD_2nd_npsp_covariates[, paste("RDD", window, model_type, sep = "_")] <- NA
    
    newdata_l <- data.frame(post = c(rep(0, n)),  birth_timeline= c(seq(from = -window, to = 0, length.out = n)))
    newdata_r <- data.frame(post =  rep(1, n),  birth_timeline= seq(from = 0,  to = window, length.out = n))
    
    for (var_n in 1:length(covariates)){
      
      data_temp <- data.2nd_short[data.2nd_short$birth_timeline <= window & data.2nd_short$birth_timeline >= -window,  ]
      
      reg_model <- paste(covariates[[var_n]], "post+ birth_timeline", sep = " ~ ")
      
      if (model_type == "np"){
        res <- loess(reg_model, span = 0.6, degree = 1, 
                     data =  data_temp)
      } else {
        res <- loess(reg_model, span = 0.6, degree = 1, parametric = "post", 
                     data =  data_temp)
      }

      plx_l <- predict(res, newdata = newdata_l,  se=T)
      plx_r <- predict(res, newdata = newdata_r,  se=T)
      
      effect <- round(plx_r[["fit"]][1] - plx_l[["fit"]][n], 3)
      bs_99_r <- plx_r[["fit"]][1] + qt(0.995,plx_r$df)*plx_r$se[1]
      bi_99_r <- plx_r[["fit"]][1] - qt(0.995,plx_r$df)*plx_r$se[1]
      bs_95_r <- plx_r[["fit"]][1] + qt(0.975,plx_r$df)*plx_r$se[1]
      bi_95_r <- plx_r[["fit"]][1] - qt(0.975,plx_r$df)*plx_r$se[1]
      bs_90_r <- plx_r[["fit"]][1] + qt(0.95,plx_r$df)*plx_r$se[1]
      bi_90_r <- plx_r[["fit"]][1] - qt(0.95,plx_r$df)*plx_r$se[1]
      
      bs_99_l <- plx_l[["fit"]][n] + qt(0.995,plx_l$df)*plx_l$se[n]
      bi_99_l <- plx_l[["fit"]][n] - qt(0.995,plx_l$df)*plx_l$se[n]
      bs_95_l <- plx_l[["fit"]][n] + qt(0.975,plx_l$df)*plx_l$se[n]
      bi_95_l <- plx_l[["fit"]][n] - qt(0.975,plx_l$df)*plx_l$se[n]
      bs_90_l <- plx_l[["fit"]][n] + qt(0.95,plx_l$df)*plx_l$se[n]
      bi_90_l <- plx_l[["fit"]][n] - qt(0.95,plx_l$df)*plx_l$se[n]
      
      star <- 0
      
      if (  bs_90_l < bi_90_r |   bi_90_l > bs_90_r) {
        star <- 1
      } 
      if (  bs_95_l < bi_95_r |   bi_95_l > bs_95_r) {
        star <- 2
      }
      if (  bs_99_l < bi_99_r |   bi_99_l > bs_99_r){
        star <- 3
      }
      
      
      RDD_2nd_npsp_covariates[which(RDD_2nd_npsp_covariates$outcome == covariates[[var_n]]), paste("RDD", window, model_type, sep = "_")] <- paste0(effect, paste(rep("*", star), collapse = ""))
      
      
      data_temp <- data_temp[data_temp[, covariates[[var_n]]]< mean(unlist(data_temp[, covariates[[var_n]]]) , na.rm = T) + 1.5*sd(unlist(data_temp[, covariates[[var_n]]]), na.rm = T) & 
                               data_temp[, covariates[[var_n]]] > mean(unlist(data_temp[, covariates[[var_n]]]), na.rm = T) - 1.5*sd(unlist(data_temp[, covariates[[var_n]]]), na.rm = T), ]
      
      pdf(paste0(paste(covariates[[var_n]], window, model_type,  sep = "_"), ".pdf"))
      
      if (covariates[[var_n]] %in% c("higher_education",  "p.health_good_exc",  "alcohol_mother")){
       plot(unlist(data_temp["birth_timeline"]), unlist(data_temp[, covariates[[var_n]]]), type = "p", lty = 1, lwd = 1, col = "white", cex =0.2, 
            xlab = "birth timeline relative to Jan 2007", ylab = "Outcome",  ylim=c(0,1)) 
      } else if (max(data_temp[, covariates[[var_n]]],  na.rm = T) == 1){
        plot(unlist(data_temp["birth_timeline"]), unlist(data_temp[, covariates[[var_n]]]), type = "p", lty = 1, lwd = 1, col = "white", cex =0.2, 
             xlab = "birth timeline relative to Jan 2007", ylab = "Outcome", ylim=c(0,1))
      }  else{
        plot(unlist(data_temp["birth_timeline"]), unlist(data_temp[, covariates[[var_n]]]), type = "p", lty = 1, lwd = 1, col = "tomato2", cex =0.2, 
             xlab = "birth timeline relative to Jan 2007", ylab = "Outcome")
      }
      
      
      lines(newdata_l[, 2], plx_l$fit, type = "l", lty = 1, col= 33, lwd= 2.5) 
      lines(newdata_l[, 2],plx_l$fit - qt(0.995,plx_l$df)*plx_l$se, lty=4,  lwd= 2.0,  col="steelblue3")
      lines(newdata_l[, 2],plx_l$fit + qt(0.995,plx_l$df)*plx_l$se, lty=4,  lwd= 2.0,  col="steelblue3")
      #lines(newdata[, 2],plx$fit - qt(0.975,plx$df)*plx$se, lty=2,  lwd= 1.5)
      #lines(newdata[, 2],plx$fit + qt(0.975,plx$df)*plx$se, lty=2,  lwd= 1.5)
      lines(newdata_l[, 2],plx_l$fit - qt(0.95,plx_l$df)*plx_l$se, lty=2,  lwd= 2.5,  col="slategray3")
      lines(newdata_l[, 2],plx_l$fit + qt(0.95,plx_l$df)*plx_l$se, lty=2,  lwd= 2.5,  col="slategray3")
      
      
      lines(newdata_r[, 2], plx_r$fit, type = "l", lty = 1, col= 33, lwd= 2.5) 
      lines(newdata_r[, 2],plx_r$fit - qt(0.995,plx_r$df)*plx_r$se, lty=4,  lwd= 2.0,  col="steelblue3")
      lines(newdata_r[, 2],plx_r$fit + qt(0.995,plx_r$df)*plx_r$se, lty=4,  lwd= 2.0,  col="steelblue3")
      #lines(newdata[, 2],plx$fit - qt(0.975,plx$df)*plx$se, lty=2,  lwd= 1.5)
      #lines(newdata[, 2],plx$fit + qt(0.975,plx$df)*plx$se, lty=2,  lwd= 1.5)
      lines(newdata_r[, 2],plx_r$fit - qt(0.95,plx_r$df)*plx_r$se, lty=2,  lwd= 2.5,  col="slategray3")
      lines(newdata_r[, 2],plx_r$fit + qt(0.95,plx_r$df)*plx_r$se, lty=2,  lwd= 2.5,  col="slategray3")
      
      legend(x = "topright",    legend=c("90% conf.", "99% conf."),
             col=c("steelblue3", "slategray3"), lty=c(4, 2), cex=0.8,  lwd= 2.5)
      
      dev.off()
      
    }  
  }
}

write.csv(RDD_2nd_npsp_covariates, "C:/Users/Lenovo/OneDrive/Documents/new projects/MC resubmission/estimates/RDD_2nd_npsp_covariates.csv")
####Loop RDD_2nd-COVARIATES####


data.2nd_short$birth_timeline_8 <- data.2nd_short$birth_timeline -  8 
data.2nd_short$post_8  <- 0
data.2nd_short$post_8[data.2nd_short$birth_timeline_8>0]  <- 1
data.2nd_short$post_birth_timeline_8  <- data.2nd_short$birth_timeline_8*data.2nd_short$post_8

  
covariates_list <- list() 
covariates_list[["e"]][[1]] <- c("post_8", "birth_timeline_8", "post_birth_timeline_8")
covariates_list[["e"]][[2]] <- c("post_8", "birth_timeline_8", "I(birth_timeline_8^2)", "post_8:birth_timeline_8", "post_8:I(birth_timeline_8^2)")
covariates_list[["e"]][[3]] <- c("post_8", "birth_timeline_8", "I(birth_timeline_8^2)", "I(birth_timeline_8^3)",
                                 "post_8:birth_timeline_8", "post_8:I(birth_timeline_8^2)", "post_8:I(birth_timeline_8^3)")

covariates_list[["c"]][[1]] <- c("")
covariates_list[["c"]][[2]] <- c("age_months")
covariates_list[["c"]][[3]] <- c("age_months", "I(age_months^2)", "I(age_months^3)")


covariates_list[["t"]][[1]] <- c("")
covariates_list[["t"]][[2]] <- c("factor(p.RLMS_wave)")
covariates_list[["t"]][[3]] <- c("interview_timeline", "I(interview_timeline^2)", "I(interview_timeline^3)")
covariates_list[["t"]][[4]] <- c("factor(p.RLMS_wave)", "interview_timeline", "I(interview_timeline^2)", "I(interview_timeline^3)")



model_list_covariates <- list()

for (wind in c(12, 24, 36)){
  for (e.var in 1:length(covariates_list[["e"]])){
    for (c.var in 1:length(covariates_list[["c"]])){
      for (t.var in 1:length(covariates_list[["t"]])){
        
        model_label <- paste0("w", wind, ".", "e", e.var, "c", c.var, "t", t.var)
        model_covariates <- unlist(list(covariates_list[["e"]][e.var], covariates_list[["c"]][c.var], covariates_list[["t"]][t.var]))
        model_covariates <- model_covariates[model_covariates != ""]
        model_list_covariates[model_label] <- paste(model_covariates, collapse = " + ")
      }
    }
  }
}

outcomes_covariates <- c("non_Russian","poverty",   "higher_education",  "income_perperson", "urban", "male",
                         "p.health_good_exc", "alcohol_mother", "p.age", "n_household_memb", "single_parent" )


outcomes_covariates_axis <- rep(unlist(outcomes_covariates), each =2)
outcomes_covariates_axis[1:length(outcomes_covariates_axis) %% 2 == 0] <- ""



RDD_2nd_covariates_8 <- data.frame(outcome = unlist(outcomes_covariates_axis))

for (var in names(model_list_covariates)){
  RDD_2nd_covariates_8[, var] <- NA
}

for (var_n in 1:length(outcomes_covariates)){
  for (model in 1:length(model_list_covariates)){
    
    reg_model <- paste(outcomes_covariates[[var_n]], model_list_covariates[[model]], sep = " ~ ")
    time_window <- as.numeric(substr(names(model_list_covariates[model]),  2, 3))
    
    try({
      temp_model <- lm(reg_model, data = data.2nd_short[data.2nd_short$birth_timeline_8 <= time_window & 
                                                          data.2nd_short$birth_timeline_8 >= -time_window & data.2nd_short$year %in% 2010:2017,  ]) 
      cov_matrix <-  vcovCL(temp_model, cluster = temp_model$region.x) #clustered  estimated covariance matrix
      temp_res <- coeftest(temp_model, cov_matrix)
      
      RDD_2nd_covariates_8[which(RDD_2nd_covariates_8$outcome == outcomes_covariates[[var_n]]),  names(model_list_covariates[model])] <- round(temp_res[[2,1]], 3)
      
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.10, 1, 0) 
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.05, 2, n_stars) 
      n_stars <- ifelse(round(temp_res[[2,4]], 4)< 0.01, 3, n_stars) 
      
      if (n_stars == 0){
        RDD_2nd_covariates_8[which(RDD_2nd_covariates_8$outcome == outcomes_covariates[[var_n]])+1,  names(model_list_covariates[model])]  <- paste0("=\"(", round(temp_res[[2,2]],3), ")\"", paste(rep("*", each = n_stars), collapse = ""))
      }
      else{
        RDD_2nd_covariates_8[which(RDD_2nd_covariates_8$outcome == outcomes_covariates[[var_n]])+1,  names(model_list_covariates[model])]  <- paste0("(", round(temp_res[[2,2]],3), ")", paste(rep("*", each = n_stars), collapse = ""))
      }
      
      RDD_2nd_covariates_8[which(RDD_2nd_covariates_8$outcome == outcomes_covariates[[var_n]])+2,  names(model_list_covariates[model])]  <- paste0("=\"(", round(temp_res[["post",4]],3), ")\"", collapse = "") #p-value
      
      
    })
  }
}


####LOWESS - COVARIATES####

dir <- "C:/Users/Lenovo/OneDrive/Documents/new projects/MC resubmission/plots/covariates/non_semi_param"
dir.create(dir, recursive = T)
setwd(dir)
n=1000
covariates <- c("p.age", "non_Russian", "income_perperson", "urban",  "higher_education",  "p.health_good_exc",
                "alcohol_mother",  "n_household_memb", "poverty", "income_30d", "single_parent")


RDD_2nd_npsp_covariates <- data.frame(outcome = unlist(covariates))
n = 1000

for(window in c(12,24,36)){
  for (model_type in c("np",  "sp")){
    
    RDD_2nd_npsp_covariates[, paste("RDD", window, model_type, sep = "_")] <- NA
    
    newdata_l <- data.frame(post = c(rep(0, n)),  birth_timeline= c(seq(from = -window, to = 0, length.out = n)))
    newdata_r <- data.frame(post =  rep(1, n),  birth_timeline= seq(from = 0,  to = window, length.out = n))
    
    for (var_n in 1:length(covariates)){
      
      data_temp <- data.2nd_short[data.2nd_short$birth_timeline <= window & data.2nd_short$birth_timeline >= -window,  ]
      
      reg_model <- paste(covariates[[var_n]], "post+ birth_timeline", sep = " ~ ")
      
      if (model_type == "np"){
        res <- loess(reg_model, span = 0.6, degree = 1, 
                     data =  data_temp)
      } else {
        res <- loess(reg_model, span = 0.6, degree = 1, parametric = "post", 
                     data =  data_temp)
      }
      
      plx_l <- predict(res, newdata = newdata_l,  se=T)
      plx_r <- predict(res, newdata = newdata_r,  se=T)
      
      effect <- round(plx_r[["fit"]][1] - plx_l[["fit"]][n], 3)
      bs_99_r <- plx_r[["fit"]][1] + qt(0.995,plx_r$df)*plx_r$se[1]
      bi_99_r <- plx_r[["fit"]][1] - qt(0.995,plx_r$df)*plx_r$se[1]
      bs_95_r <- plx_r[["fit"]][1] + qt(0.975,plx_r$df)*plx_r$se[1]
      bi_95_r <- plx_r[["fit"]][1] - qt(0.975,plx_r$df)*plx_r$se[1]
      bs_90_r <- plx_r[["fit"]][1] + qt(0.95,plx_r$df)*plx_r$se[1]
      bi_90_r <- plx_r[["fit"]][1] - qt(0.95,plx_r$df)*plx_r$se[1]
      
      bs_99_l <- plx_l[["fit"]][n] + qt(0.995,plx_l$df)*plx_l$se[n]
      bi_99_l <- plx_l[["fit"]][n] - qt(0.995,plx_l$df)*plx_l$se[n]
      bs_95_l <- plx_l[["fit"]][n] + qt(0.975,plx_l$df)*plx_l$se[n]
      bi_95_l <- plx_l[["fit"]][n] - qt(0.975,plx_l$df)*plx_l$se[n]
      bs_90_l <- plx_l[["fit"]][n] + qt(0.95,plx_l$df)*plx_l$se[n]
      bi_90_l <- plx_l[["fit"]][n] - qt(0.95,plx_l$df)*plx_l$se[n]
      
      star <- 0
      
      if (  bs_90_l < bi_90_r |   bi_90_l > bs_90_r) {
        star <- 1
      } 
      if (  bs_95_l < bi_95_r |   bi_95_l > bs_95_r) {
        star <- 2
      }
      if (  bs_99_l < bi_99_r |   bi_99_l > bs_99_r){
        star <- 3
      }
      
      
      RDD_2nd_npsp_covariates[which(RDD_2nd_npsp_covariates$outcome == covariates[[var_n]]), paste("RDD", window, model_type, sep = "_")] <- paste0(effect, paste(rep("*", star), collapse = ""))
      
      
      data_temp <- data_temp[data_temp[, covariates[[var_n]]]< mean(unlist(data_temp[, covariates[[var_n]]]) , na.rm = T) + 1.5*sd(unlist(data_temp[, covariates[[var_n]]]), na.rm = T) & 
                               data_temp[, covariates[[var_n]]] > mean(unlist(data_temp[, covariates[[var_n]]]), na.rm = T) - 1.5*sd(unlist(data_temp[, covariates[[var_n]]]), na.rm = T), ]
      
      pdf(paste0(paste(covariates[[var_n]], window, model_type,  sep = "_"), ".pdf"))
      
      if (covariates[[var_n]] %in% c("higher_education",  "p.health_good_exc",  "alcohol_mother")){
        plot(unlist(data_temp["birth_timeline"]), unlist(data_temp[, covariates[[var_n]]]), type = "p", lty = 1, lwd = 1, col = "white", cex =0.2, 
             xlab = "birth timeline relative to Jan 2007", ylab = "Outcome",  ylim=c(0,1)) 
      } else if (max(data_temp[, covariates[[var_n]]],  na.rm = T) == 1){
        plot(unlist(data_temp["birth_timeline"]), unlist(data_temp[, covariates[[var_n]]]), type = "p", lty = 1, lwd = 1, col = "white", cex =0.2, 
             xlab = "birth timeline relative to Jan 2007", ylab = "Outcome", ylim=c(0,1))
      }  else{
        plot(unlist(data_temp["birth_timeline"]), unlist(data_temp[, covariates[[var_n]]]), type = "p", lty = 1, lwd = 1, col = "tomato2", cex =0.2, 
             xlab = "birth timeline relative to Jan 2007", ylab = "Outcome")
      }
      
      
      lines(newdata_l[, 2], plx_l$fit, type = "l", lty = 1, col= 33, lwd= 2.5) 
      lines(newdata_l[, 2],plx_l$fit - qt(0.995,plx_l$df)*plx_l$se, lty=4,  lwd= 2.0,  col="steelblue3")
      lines(newdata_l[, 2],plx_l$fit + qt(0.995,plx_l$df)*plx_l$se, lty=4,  lwd= 2.0,  col="steelblue3")
      #lines(newdata[, 2],plx$fit - qt(0.975,plx$df)*plx$se, lty=2,  lwd= 1.5)
      #lines(newdata[, 2],plx$fit + qt(0.975,plx$df)*plx$se, lty=2,  lwd= 1.5)
      lines(newdata_l[, 2],plx_l$fit - qt(0.95,plx_l$df)*plx_l$se, lty=2,  lwd= 2.5,  col="slategray3")
      lines(newdata_l[, 2],plx_l$fit + qt(0.95,plx_l$df)*plx_l$se, lty=2,  lwd= 2.5,  col="slategray3")
      
      
      lines(newdata_r[, 2], plx_r$fit, type = "l", lty = 1, col= 33, lwd= 2.5) 
      lines(newdata_r[, 2],plx_r$fit - qt(0.995,plx_r$df)*plx_r$se, lty=4,  lwd= 2.0,  col="steelblue3")
      lines(newdata_r[, 2],plx_r$fit + qt(0.995,plx_r$df)*plx_r$se, lty=4,  lwd= 2.0,  col="steelblue3")
      #lines(newdata[, 2],plx$fit - qt(0.975,plx$df)*plx$se, lty=2,  lwd= 1.5)
      #lines(newdata[, 2],plx$fit + qt(0.975,plx$df)*plx$se, lty=2,  lwd= 1.5)
      lines(newdata_r[, 2],plx_r$fit - qt(0.95,plx_r$df)*plx_r$se, lty=2,  lwd= 2.5,  col="slategray3")
      lines(newdata_r[, 2],plx_r$fit + qt(0.95,plx_r$df)*plx_r$se, lty=2,  lwd= 2.5,  col="slategray3")
      
      legend(x = "topright",    legend=c("90% conf.", "99% conf."),
             col=c("steelblue3", "slategray3"), lty=c(4, 2), cex=0.8,  lwd= 2.5)
      
      dev.off()
      
    }  
  }
}

write.csv(RDD_2nd_npsp_covariates, "C:/Users/Lenovo/OneDrive/Documents/new projects/MC resubmission/estimates/RDD_2nd_npsp_covariates.csv")

####Saving datasets ####
ls()

dir <- "C:/Users/Lenovo/OneDrive/Documents/new projects/MC resubmission/estimates"
setwd(dir)

write.csv(RDD_2nd, "RDD_2nd.csv")
write.csv(RDD_2nd_female, "RDD_2nd_female.csv")
write.csv(RDD_2nd_male, "RDD_2nd_male.csv")
write.csv(RDD_2nd_poor, "RDD_2nd_poor.csv")
write.csv(RDD_2nd_notpoor, "RDD_2nd_notpoor.csv")
write.csv(RDD_2nd_urb, "RDD_2nd_urb.csv")
write.csv(RDD_2nd_rur, "RDD_2nd_rur.csv")
write.csv(RDD_2nd_covariates, "RDD_2nd_covariates.csv")

write.csv(RDD_2nd_test_gender, "RDD_2nd_test_gender.csv")
write.csv(RDD_2nd_test_poverty, "RDD_2nd_test_poverty.csv")
write.csv(RDD_2nd_test_urbrur, "RDD_2nd_test_urbrur.csv")

write.csv(RDD_2nd_npsp, "RDD_2nd_npsp.csv")
write.csv(RDD_2nd_npsp_covariates, "RDD_2nd_npsp_covariates.csv")
write.csv(RDD_npsp_2nd_fem, "RDD_npsp_2nd_fem.csv")
write.csv(RDD_npsp_2nd_male, "RDD_npsp_2nd_male.csv")
write.csv(RDD_npsp_2nd_notpoor, "RDD_npsp_2nd_notpoor.csv")
write.csv(RDD_npsp_2nd_poor, "RDD_npsp_2nd_poor.csv")
write.csv(RDD_npsp_2nd_rur, "RDD_npsp_2nd_rur.csv")
write.csv(RDD_npsp_2nd_urb, "RDD_npsp_2nd_urb.csv")
write.csv(RDD_npsp_2nd_urb, "RDD_npsp_2nd_urb.csv")



####Short

dir <- "C:/Users/Lenovo/OneDrive/Documents/new projects/MC resubmission/estimates/short"
dir.create(dir, recursive = T)
setwd(dir)
models <-  c("w12.e1c1t2", "w12.e1c2t2", "w12.e1c3t2",
             "w24.e2c2t2", "w24.e2c3t2",
             "w36.e3c2t2", "w36.e3c3t2")
medels_hetero <- c("w12.e1c1t2", "w12.e1c2t2", "w12.e1c3t2",
                   "w24.e1c2t2", "w24.e2c3t2",
                   "w36.e3c3t2")
models_test <-  c("w12.e1c2t2")


outcomes_descr <-  list(list("age", "Age", "cont"), 
                        list("male" , "Sex (male)",  "bin"),  
                        list(  "year",  "Year", "cont"), 
                        
                        list(  "health_good_exc", "In good/excellent health", "bin"),
                        list(  "health_score", "Health score (1-highest, 5-lowest)", "cont"),
                        list(  "health_prob_30d", "Health problem in last 30d", "bin"),
                        list( "chronic_cnd_count", "Num. of chronic conditions", "cont"),
                        
                        list( "homework_min", "School homework/assignments (in mins per week)", "cont"),
                        list( "extracurricular_min", "Extracurricular study (in mins per week)", "cont"),                        
                        list( "extracurricular_art_min", "Extracurricular arts (in mins per week)", "cont"),
                        list(  "extra_phys_min", "Extracurricular sports (in mins per week)", "cont"),
                        list(  "productive_time_min", "Productive time (in mins per week", "cont"),
                        
                        list( "TV_internet_min", "Watching TV/on Internet (in mins per week)", "cont"),
                        list(  "used_PC", "Used a personal computer", "bin (in mins per week)"),
                        
                        #Only school students 
                        list("vacay_w_parent_1yr", "Vacation with parent in 1yr", "bin"),
                        list("total_exhibitions_excursions", "Num. of excursions/exhibition visits in 1yr", "cont"),
                        list("see_friends", "Sees friends >2 times per week", "bin"),
                        list("child_care_7d", "Received childcare in past 7 days", "bin"),
                        list("relatives_care_7d", "Received childcare from a relative in past 7 days", "bin"),
                        
                        list("p.age", "Mother's age", "cont"),
                        list("urban", "Urban", "bin"),
                        list("single_parent", "Single parent", "bin"),
                        list( "p.health_good_exc", "In good/excellent health", "bin"),
                        list("higher_education", "Higher education diploma", "bin"),
                        list("non_Russian", "Ethnically other than Russian", "bin"),
                        list( "poverty", "Poverty", "bin"),
                        list( "alcohol_mother", "Alcohol cons. >1 time per week", "bin"),
                        list( "life_satisfaction", "Mothers subjective life satisfaction (1-lowest, 5-highest)", "cont"),
                        
                        list(   "income_30d", "Household income in last 30 days (in 2011 prices)", "cont"),
                        list(   "savings_30d","Household savings in last 30 days (in 2011 prices)", "cont"),
                        list(   "savings_will_last_several_months", "Savings will last several months", "bin"),
                        list(   "consumer_loan_12m", "Consumer loans in last 12 months", "bin"),
                        list(   "loan_from_private_ind", "Borrowed from a private individual in last 12 months", "bin"),
                        list(   "loan_to_private_ind", "Loan to a private individual in last 12 months", "bin"),
                        list(   "loan_payments_30d", "Loan payments in last 30 days (in 2011 prices)", "cont"),
                        
                        list(  "veg_cons", "Vegetables/legumes (in kg)", "cont"),
                        list(  "fruit_cons", "Fruit (fresh and canned) (in kg)", "cont"),
                        list(  "meat_cons", "Meat and poultry (in kg)", "cont"),
                        list( "dairy_cons","Dairy (in kg)", "cont"),
                        list(  "vodka_spirits_cons", "Vodka and liquors (in liters)", "cont"),
                        list("refined_sugar_cons", "Refined sugar (in kg)", "cont"),
                        list(  "high_sugar_cons", "Candy and high-sugar treats (in kg)", "cont"),
                        list("high_starch_cons", "Starches (in kg)", "cont"),
                        
                        list(  "food_essential",  "Essential food items (in Rub)", "cont"),  
                        list(  "clothes",  "Clothes (in Rub)", "cont"),
                        list(  "all_basic_exp",  "All basic expenditure (in Rub)", "cont"),  
                        list(  "luxury_exp",  "All non-essential expenditure (in Rub)", "cont"), 
                        list(  "durable",  "Purchase of durable goods (in Rub)", "bin"), 
                        list(  "utilities",  "Utilities (in Rub)", "cont"), 
                        list(  "essential_sevices",  "Essential services  (in Rub)", "cont"),
                        
                        list("sh_food_essential", "Sh. Essential food items", "cont"),
                        list("sh_clothes", "Sh. Clothes", "cont"),
                        list("sh_utilities", "Sh. Utilities", "cont"),
                        list("sh_essential_sevices", "Sh. Essential services", "cont"),
                        list("sh_all_basic_exp", "Sh. Basic expenditure", "cont"),
                        list("sh_luxury_exp", "Sh. Non-essential expenditure ", "cont"),
                        list("sh_savings_30d","Sh. Savings", "cont"),
                        list("sh_loan_payments_30d", "Sh. Loan payments", "cont"),
                        
                        
                        list("home_ownership", "Household owns home", "cont"),
                        list("hot_water_supply", "Hot water supply", "bin"),
                        list("central_sewerage","Central sewerage", "bin"),
                        list("central_heating", "Central heating", "bin")
                        
)

outcomes_descr_var <- c()
outcomes_descr_lab <- c()

for (i in 1:length(outcomes_descr)){
  outcomes_descr_var[i] <- outcomes_descr[[i]][[1]]
  outcomes_descr_lab[i] <- outcomes_descr[[i]][[2]]
  
}


##DATA
table_relabel <- function(data){
  for(i in 1:nrow(data)){
    n_var <- which(outcomes_descr_var == data[i, "outcome"])
    if(length(n_var) > 0){
      data[i, "outcome"] <-outcomes_descr_lab[n_var] 
    }
  }
  return(data)
}


RDD_2nd <- table_relabel(RDD_2nd)
RDD_2nd_female <- table_relabel(RDD_2nd_female)
RDD_2nd_male <- table_relabel(RDD_2nd_male)
RDD_2nd_poor <- table_relabel(RDD_2nd_poor)
RDD_2nd_notpoor <- table_relabel(RDD_2nd_notpoor)
RDD_2nd_urb  <- table_relabel(RDD_2nd_urb)
RDD_2nd_rur  <- table_relabel(RDD_2nd_rur)

RDD_2nd_test_gender <- table_relabel(RDD_2nd_test_gender)
RDD_2nd_test_poverty <- table_relabel(RDD_2nd_test_poverty)
RDD_2nd_test_urbrur <- table_relabel(RDD_2nd_test_urbrur)


RDD_2nd_npsp <- table_relabel(RDD_2nd_npsp)
RDD_npsp_2nd_fem <- table_relabel(RDD_npsp_2nd_fem)
RDD_npsp_2nd_male <-  table_relabel(RDD_npsp_2nd_male)
RDD_npsp_2nd_notpoor <-  table_relabel(RDD_npsp_2nd_notpoor)
RDD_npsp_2nd_poor <-  table_relabel(RDD_npsp_2nd_poor)
RDD_npsp_2nd_rur <- table_relabel(RDD_npsp_2nd_rur)
RDD_npsp_2nd_urb <-  table_relabel(RDD_npsp_2nd_urb)



write.csv(RDD_2nd[, c("outcome", models)], "RDD_2nd.csv")
write.csv(RDD_2nd_female[, c("outcome", medels_hetero)], "RDD_2nd_female.csv")
write.csv(RDD_2nd_male[, c("outcome", medels_hetero)], "RDD_2nd_male.csv")
write.csv(RDD_2nd_poor[, c("outcome", medels_hetero)], "RDD_2nd_poor.csv")
write.csv(RDD_2nd_notpoor[, c("outcome", medels_hetero)], "RDD_2nd_notpoor.csv")
write.csv(RDD_2nd_urb[, c("outcome", medels_hetero)], "RDD_2nd_urb.csv")
write.csv(RDD_2nd_rur[, c("outcome", medels_hetero)], "RDD_2nd_rur.csv")


write.csv(RDD_2nd_test_gender[, c("outcome", models_test)], "RDD_2nd_test_gender.csv")
write.csv(RDD_2nd_test_poverty[, c("outcome", models_test)], "RDD_2nd_test_poverty.csv")
write.csv(RDD_2nd_test_urbrur[, c("outcome", models_test)], "RDD_2nd_test_urbrur.csv")


write.csv(RDD_2nd_npsp, "RDD_npsp_2nd.csv")
write.csv(RDD_2nd_npsp_covariates, "RDD_npsp_2nd_covariates.csv")
write.csv(RDD_npsp_2nd_fem, "RDD_npsp_2nd_fem.csv")
write.csv(RDD_npsp_2nd_male, "RDD_npsp_2nd_male.csv")
write.csv(RDD_npsp_2nd_notpoor, "RDD_npsp_2nd_notpoor.csv")
write.csv(RDD_npsp_2nd_poor, "RDD_npsp_2nd_poor.csv")
write.csv(RDD_npsp_2nd_rur, "RDD_npsp_2nd_rur.csv")
write.csv(RDD_npsp_2nd_urb, "RDD_npsp_2nd_urb.csv")

