#####Last lines from 18_12_19####


library(readr)
result <- read_csv("C:/Users/Lenovo/OneDrive/?????????????? ????????/past projects/RLMS data/python_completed/result.csv")


#Birth of second child
result$d_2nd_ch <- 0
result$d_2nd_ch  <- ifelse(result$sum_child_m == 2 & ( result$c_y_birth1_m == result$year| result$c_y_birth2_m == result$year) & 
                             (result$c_y_birth1_m != result$c_y_birth2_m),  1,  0)
result$d_2nd_ch[is.na(result$d_2nd_ch)] <- 0

#Birth of third child
result$d_3rd_ch <- 0
result$d_3rd_ch  <- ifelse(result$sum_child_m == 3 & (result$c_y_birth1_m == result$year| result$c_y_birth2_m == result$year | result$c_y_birth3_m == result$year) & 
                             (result$c_y_birth1_m != result$c_y_birth2_m &  
                                result$c_y_birth2_m != result$c_y_birth3_m &
                                result$c_y_birth1_m != result$c_y_birth3_m),  1,  0)
result$d_3rd_ch[is.na(result$d_3rd_ch)] <- 0
sum(result$d_3rd_ch)
table(result$d_3rd_ch, result$year)
#View(result[1:20,])

# Two children in total ("s" stands for stock)
result$s_2ch <- ifelse(result$sum_child_m >= 2, 1, 0)

#Pool (1 childen OR 2 childen OR 3 childen only 1 year after birth ) for Selection probit;  = 1 if 2 children only
result$pool_h <- NA
result$pool_h[result$sum_child_m  == 2] <- 1
result$pool_h[result$sum_child_m  == 1] <- 0
result$pool_h[result$sum_child_m  == 3 & (result$c_y_birth1_m == result$year| result$c_y_birth2_m == result$year | result$c_y_birth3_m == result$year) ] <- 0

result$pool_exit <- 0
result$pool_exit[result$sum_child_m  == 3 & (result$c_y_birth1_m == result$year| result$c_y_birth2_m == result$year | result$c_y_birth3_m == result$year) ] <- 1 

result$year_f <- factor(result$year)
result$year_f <-  relevel(result$year_f , ref = "2006")


#MC right acquired  (after 2nd child  & post 2006)
a = result[result$d_2nd_ch == 1 & result$year > 2006, c("year", "idind_m")]
mothers <- unique(a$idind_m)

i = 1
result$MC_2nd_p2007 <- 0
for (i  in 1:nrow(a)){
  result[which(result$year >= as.numeric(a[i, 1]) & result$idind_m ==  as.numeric(a[i, 2])), "MC_2nd_p2007"] <- 1
}
rm(list = c("a",  "i", "mothers"))


#Ordrer of children 


i = 1 
result$child_order <- NA
for (i in 1:nrow(result)){
  a  = na.omit(as.numeric(result[i, grep("c_y_birt", names(result), value =T )]))
  if (length(a) > 0){
    a  = sort(a)
    order_ch  = which(a == as.numeric(result[i, "j69.9c"]))
    result[i, "child_order"] <- order_ch[1] 
    #if (length(order_ch) == 0) print(paste(as.numeric(result[i, c("j69.9c","j72.172_m", "j72.172m_m", "idind_m")]), sep = " "))
  }
}
table(result$child_order, useNA = 'always')



result$post_birth_timeline <- result$birth_timeline *result$post

####Outcomes child health####
#Number of vaccinations
result$n_vacc <- 0 

table(result$health_good_exc)
paste("l43.", 1:12, sep= "")
for (var in paste("l43.", 1:12, sep= "")){
  result$n_vacc <- result$n_vacc + ifelse(unlist(result[, var]) %in% 1 | is.na(unlist(result[, var])), 1, 0)
}

#Health self-evaluation
result$health_good_exc <- NA
result$health_good_exc[result$m3 %in% 1:2 ] <- 1
result$health_good_exc[result$m3 %in% 3:6 ] <- 0

#Health problems 30d
result$health_prob_30d <- result$l5

#Chronic conditions couunt 
result$chronic_cnd_count 

#Hospitalization in 30d
result$hospit_30d <- result$l20

#Hospitalization in 30d (2004-2010)
result$profilactic <- result$l26.1

#Voluntary health insurance (2009-2017)
result$volunt_insurance <- result$l2.2

#BMI 
result$BMI[result$BMI < 10 & result$BMI > 70] <- NA

result$BMI_under <- NA
result$BMI_over <-  NA


normal_bmi <- list(  c(1, NA, NA), c(2, 14.8, 18.1), c(3, 14.5, 17.5), c(4, 14, 17), c(5, 13.8, 16.8), c(6, 13.7, 17), c(7, 13.7,17.5), c(8, 13.8, 18),
                    c(9, 14.2,18.7), c(10, 14.4,19.4), c(11, 14.6,20.3), c(12, 15,21), c(13, 15.5,21.8), c(14, 16,22.7), c(15, 16.6,23.5), c(16, 17.2, 24.2), c(17, 17.7, 25), c(18, 18.2,25))

i =1
for (i in 1:nrow(result)){
  if (result$age[i] %in% 1:18){
      result$BMI_under[i] <- ifelse(result$BMI[i] < normal_bmi[[result$age[i]]][2],  1,  0)  
      result$BMI_over[i] <- ifelse(result$BMI[i] > normal_bmi[[result$age[i]]][3],  1,  0)  
  }
}


####Outcomes  child education & development####
#Educational attainment (excellent_good: 4.3-5.0, satisf_bad:  2.6-3.4) (2010-2017)
result$k3.4 <- as.numeric(result$k3.4)
result$GPA_excellent_good <- ifelse(result$k3.4 %in% 1:2, 1,0)
result$GPA_excellent_good <- ifelse(is.na(result$k3.4), NA, result$GPA_excellent_good )


result$GPA_satisf_bad <- ifelse(result$k3.4 %in% 4:6, 1,0)
result$GPA_satisf_bad <- ifelse(is.na(result$k3.4), NA, result$GPA_satisf_bad )



#Time spent preparing homework per week with parent (2010-2017)
time_var <- grep("k7.3|k7.6|k761|k7.7|k771", names(result), value = T)
for (var in time_var){
  result[,var] <- as.numeric(unlist(result[,var]))
  result[ ifelse(result[,var] > 60 | is.na(result[,var]),  T, F), var] <-  NA 
}

result$homework_min <- NA
result$homework_min <- as.numeric(result$k771.3b)*60 + as.numeric(result$k771.3c) 
result$homework_min <- ifelse(as.numeric(result$k771.3a) == 2, 0,  result$homework_min)



#Time spent on extracurricular computer  science in mins per week (2010-2017)
result$CS_min <- NA
result$CS_min <- as.numeric(result$k761.4b)*60 + as.numeric(result$k761.4c) 
result$CS_min <- ifelse(as.numeric(result$k761.4a) == 0, 0,  result$CS_min)



#Time spent on extracurricular foreign language in mins per week (2010-2017)
result$foreign_lang_min <- NA
result$foreign_lang_min <- as.numeric(result$k761.5b)*60 + as.numeric(result$k761.5c) 
result$foreign_lang_min <- ifelse(as.numeric(result$k761.5a) == 0, 0,  result$foreign_lang_min)


#Time spent on extracurricular other school subjects  in mins per week  (2010-2017)
result$other_subjects_min <- NA
result$other_subjects_min <- as.numeric(result$k761.6b)*60 + as.numeric(result$k761.6c) 
result$other_subjects_min <- ifelse(as.numeric(result$k761.6a) == 0, 0,  result$other_subjects_min)

#Time spent on extracurricular (CS + language + other school subjects) in mins per week  (2010-2017)
result$extracurricular_min <- result$CS_min + result$foreign_lang_min + result$other_subjects_min 

#Time spent on extracurricular arts (2010-2017)
result$extra_arts_min  <-  ifelse(result$k761.1a == 0, 0, result$k761.1b)*60 + ifelse(result$k761.1a == 0, 0, result$k761.1c) +
  ifelse(result$k761.2a == 0, 0, result$k761.2b)*60 + ifelse(result$k761.2a == 0, 0, result$k761.2b) +
  ifelse(result$k761.3a == 0, 0, result$k761.3b)*60 + ifelse(result$k761.3a == 0, 0, result$k761.3c) 

#Time spent wathing TV and Intenet (2010-2017)
result$TV_internet_min  <-  ifelse(result$k7.7.1a == 2, 0, result$k7.7.1b)*60 + ifelse(result$k7.7.1a == 2, 0, result$k7.7.1c) +
  ifelse(result$k7.7.7a == 2, 0, result$k7.7.7b)*60 + ifelse(result$k7.7.7a == 2, 0, result$k7.7.7c)

#Household money spent on extracurriculars
result$e13.1b_m <- as.numeric(result$e13.1b_m)
result$extracur_mon <-  ifelse(result$e13.1a_m == 2, 0, result$e13.1b_m)
result$extracur_mon[result$extracur_mon>999999] <-  NA




####Outcomes  well-being ####
###Time spent on out-of-school physical activity  in mins (2010-2017)
result$extra_phys_min  <-  ifelse(result$k7.6.1a == 0, 0, result$k7.6.1b)*60 + ifelse(result$k7.6.1a == 0, 0, result$k7.6.1c) +
  ifelse(result$k7.6.2a == 0, 0, result$k7.6.2b)*60 + ifelse(result$k7.6.2a == 0, 0, result$k7.6.2c) +
  ifelse(result$k7.6.3a == 0, 0, result$k7.6.3b)*60 + ifelse(result$k7.6.3a == 0, 0, result$k7.6.3c) +
  ifelse(result$k7.6.4a == 0, 0, result$k7.6.4b)*60 + ifelse(result$k7.6.4a == 0, 0, result$k7.6.4c)
  
#Has cell (2005-2017)
result$j184 <- as.numeric(result$j184)
result$has_cell <- result$j184 

#Has PC (2006-2016)
result$j72160 <- as.numeric(result$j72160)
result$has_PC <- ifelse(result$j72160 == 1, 1,  0)

#Vacation spent with child in past 12mnths (2010-2017)
result$k8.17 <- as.numeric(result$k8.17)
result$vacay_w_parent_1yr <- ifelse(result$k8.17 == 1, 1,  0)

#Household money spent on vacations
result$e13.2b_m <- as.numeric(result$e13.2b_m)

result$vacay_mon <-  ifelse(result$e13.2a_m == 2, 0, result$e13.2b_m)
result$vacay_mon[result$vacay_mon > 999999] <-  NA


#Exhibition excursions in  past 1yr (2010-2017)
result$k816.1a <- as.numeric(result$k816.1a)
result$k816.2a <- as.numeric(result$k816.2a)
result$excursion  <-  ifelse(result$k816.1a == 1, 1, 0) + ifelse(result$k816.2a == 1, 1, 0)
result$excursion[result$excursion > 1] <- 1


#Sees friends frequently  more than 2 times per week (2010-2017)
result$k8.20 <- as.numeric(result$k8.20)
result$see_friends <- ifelse(result$k8.20 == 4 | result$k8.20 == 5, 1, 0)




####Household diatery choices ####

food_var <- grep("e1.", names(result), value = T)
for (var in food_var){
  result[,var] <- as.numeric(unlist(result[,var]))
  result[ ifelse(result[,var] > 999999 | is.na(result[,var]),  T, F), var] <-  NA 
}



result$veg_cons  <-  ifelse(result$e1.7a == 2, 0, result$e1.7b) + ifelse(result$e1.8a == 2, 0, result$e1.8b) +
  ifelse(result$e1.9a == 2, 0, result$e1.9b) + ifelse(result$e1.10a == 2, 0, result$e1.10b) +
  ifelse(result$e1.11a == 2, 0, result$e1.11b) + ifelse(result$e1.12a == 2, 0, result$e1.12b) +
  ifelse(result$e1.13a == 2, 0, result$e1.13b) + ifelse(result$e1.14a == 2, 0, result$e1.14b)

result$fruit_cons  <-  ifelse(result$e1.15a == 2, 0, result$e1.15b) + ifelse(result$e1.16a == 2, 0, result$e1.16b) +
  ifelse(result$e1.17a == 2, 0, result$e1.17b) + ifelse(result$e1.18a == 2, 0, result$e1.18b) +
  ifelse(result$e1.19a == 2, 0, result$e1.19b) + ifelse(result$e1.20a == 2, 0, result$e1.20b) 


result$meat_cons  <-  ifelse(result$e1.21a == 2, 0, result$e1.21b) + ifelse(result$e1.22a == 2, 0, result$e1.22b) +
  ifelse(result$e1.23a == 2, 0, result$e1.23b) + ifelse(result$e1.24a == 2, 0, result$e1.24b) +
  ifelse(result$e1.25a == 2, 0, result$e1.25b) + ifelse(result$e1.26a == 2, 0, result$e1.26b) 

result$dairy_cons  <-  ifelse(result$e1.30a == 2, 0, result$e1.30b) + ifelse(result$e1.31a == 2, 0, result$e1.31b) +
  ifelse(result$e1.32a == 2, 0, result$e1.32b) + ifelse(result$e1.33a == 2, 0, result$e1.33b) +
  ifelse(result$e1.34a == 2, 0, result$e1.34b) + ifelse(result$e1.35a == 2, 0, result$e1.35b) +
  ifelse(result$e1.36a == 2, 0, result$e1.36b)

result$vodka_spirits_cons  <-  ifelse(result$e1.53a == 2, 0, result$e1.53b) + ifelse(result$e1.54a == 2, 0, result$e1.54b) 

result$refined_sugar_cons <- ifelse(result$e1.40a == 2, 0, result$e1.40b) 

result$high_sugar_cons  <- ifelse(result$e1.37a == 2, 0, result$e1.37b) + ifelse(result$e1.41a == 2, 0, result$e1.41b) +
  ifelse(result$e1.44a == 2, 0, result$e1.44b) 

result$high_starch_cons  <- ifelse(result$e1.1a == 2, 0, result$e1.1b) + ifelse(result$e1.2a == 2, 0, result$e1.2b) +
  ifelse(result$e1.3a == 2, 0, result$e1.3b)+ ifelse(result$e1.4a == 2, 0, result$e1.4b) + ifelse(result$e1.5a == 2, 0, result$e1.5b) +
  ifelse(result$e1.6a == 2, 0, result$e1.6b) 



#Eating out in past  1wk
result$e2_m <- as.numeric(result$e2_m)
result$eat_out <- NA
result$eat_out[which(result$e2_m == 2)] <- 0
result$eat_out[which(result$e2_m == 1)] <- 1



####Covariates####
#Non-Russian
result$non_Russian <- NA
result$non_Russian <- ifelse(result$i4_m == 1, 0, 1)  

#Poverty
result$poverty <- NA
result$poverty <- ifelse(result$j66_m == 1, 1, 0)

#Higher education
result$higher_education <- NA
result$higher_education <- ifelse(result$j72.5c_m %in% 1, 1, 0)

#Higher education
result$higher_education <- NA
result$higher_education <- ifelse(result$j72.5c_m %in% 1, 1, 0)

#High alcohol  (>=2 per week) 
result$alcohol_mother <- NA
result$alcohol_mother <- ifelse(result$m81_m %in% 1:3, 1, 0)

#Urban
result$urban <- NA
result$urban <- ifelse(result$status_m %in% 1:2, 1, 0)

#Sex of child 
result$male <- NA
result$male <- ifelse(result$h5 %in% 1, 1, 0)

result$h_nfm_m <- NA
result$male <- ifelse(result$h5 %in% 1, 1, 0)
 
#Single-parent family (2009-2017)
result$j324_m <- as.numeric(result$j324_m)
result$single_parent <- 0
result$single_parent <-  ifelse(result$j324_m %in% 3, 1, 0)



#Number of household  memebers 
result$n_household_memb <- NA
year = 2004
for  (wave in letters[9:22]) {
  var <-paste0(wave, "_nfm_m")
  result[var] <- as.numeric(unlist(result[var]))
  result[result$year == year, "n_household_memb"] <- result[result$year == year, var]
  year = year + 1
}




#Health  evaluation mother
result$health_good_exc_m <- NA
result$health_good_exc_m[result$m3_m %in% 1:2 ] <- 1
result$health_good_exc_m[result$m3_m %in% 3:6 ] <- 0
min(result$m81_m ,  na.rm = T)

#z-BMI
#install.packages('anthro')
library(anthro)

table(result$`_born_m`, useNA ="always")
table(result$`_born_m`, useNA ="always")
table(result$j69.9c, useNA ="always")
table(result$h7.2, useNA ="always")
table(result$`_int_y`, useNA ="always")



result$age_months <- result$`_int_y`*12 + result$h7.2 - result$j69.9c*12 - result$`_born_m`

#2nd child
result$second_child <- 0
result$second_child  <- ifelse(result$child_order == 2, 1, 0)
result$age_months[result$age_months < 0] <- NA

#z-BMI
result$zBMI <- NA
result$zBMI <- anthro_zscores(
  sex = result$h5, age = result$age_months, is_age_in_month = F, measure = "H", 
  weight = ifelse(result$m1 > 250 | result$m1 < 8 , NA,   result$m1), lenhei = ifelse(result$m2 > 250 | result$m2 < 50, NA,   result$m2))["zbmi"]

result$zBMI <- as.numeric(unlist(result$zBMI))


              
####Household income & spending####
inflation <- list(list(2019,  3.05, 185.34),
                   list(2018,  4.27, 177.87), 
                   list(2017,  2.52, 173.5),
                   list(2016,  5.38, 164.64), 
                   list(2015,  12.91, 145.81), 
                   list(2014,  11.36, 130.94),
                   list(2013,  6.45, 123.01),
                   list(2012,  6.58, 115.41),
                   list(2011,  6.10, 108.78),
                   list(2010,  8.78, 100),
                   list(2009,  8.80, 91.2),
                   list(2008,  13.28, 79.08),
                   list(2007,  11.87, 69.7),
                   list(2006,  9.00, 63.42),
                   list(2005,  10.91, 57.71),
                   list(2004,  11.74, 50.94))
options("scipen"=100, "digits"=4)

#Income
result$f11_m[which(result$f11_m > 900000)] <- NA 


#Savings  
result$savings <- ifelse(is.na(result$e17_m) |  result$e17_m > 900000, 0, result$e17_m)
#loans
result$loan <- ifelse(is.na(result$e13.72b_m) |  result$e13.72b_m > 900000, 0, result$e13.72b_m)


#Durable goods
result$durable <- 0

for (var in c("e7.1.0a_m", "e7.1.1a_m", "e7.2a_m", "e7.3a_m", 
              "e7.4a_m", "e7.5a_m",  "e7.6a_m", "e7.7a_m",
              "e7.8a_m", "e7.9a_m",  "e7.10a_m")){
  result[, var] <- as.numeric(unlist(result[, var]))
}


for (var in c("e7.1.0a_m", "e7.1.1a_m", "e7.2a_m", "e7.3a_m", 
              "e7.4a_m", "e7.5a_m",  "e7.6a_m", 
              "e7.8a_m",  "e7.10a_m")){

  result[which(result[, var] == 1), "durable"] <- 1
}

#Goods (luxury vs. basic)  

result$food_essential <- 0
for (var in c(paste0("e1.",  c(1:52), "c"))){
  result$food_essential <-  result$food_essential + (30/7)*ifelse(is.na(result[, var])| result[, var] > 900000, 0, unlist(result[, var]))
}

result$clothes <- 0

for (var in c("e6.1_m", "e6.2_m")){
  result$clothes <-  result$clothes + (1/3)*ifelse(is.na(result[, var])| result[, var] > 900000, 0, unlist(result[, var]))
}


result$fuel <- 0
for (var in c("e8.1b_m", "e8.2b_m",  "e8.3b_m")){
  result$fuel <-  result$fuel + ifelse(is.na(result[, var])| result[, var] > 900000, 0, as.numeric(unlist(result[, var])))
  print(max(ifelse(is.na(result[, var])| result[, var] > 900000, 0, as.numeric(unlist(result[, var])))))
}


result$essential_sevices <- 0
for (var in c(paste0("e9.",  c(1:3, 5:11), "b_m"))){
  result$essential_sevices <-  result$essential_sevices + ifelse(is.na(result[, var])| result[, var] > 900000, 0, as.numeric(unlist(result[, var])))
  print(max(ifelse(is.na(result[, var])| result[, var] > 900000, 0, as.numeric(unlist(result[, var])))))
}

result$utilities <- 0
for (var in c("e11_m")){
  result$utilities <-  result$utilities + ifelse(is.na(result[, var])| result[, var] > 900000, 0, as.numeric(unlist(result[, var])))
  print(max(ifelse(is.na(result[, var])| result[, var] > 900000, 0, as.numeric(unlist(result[, var])))))
}




result$luxury_exp <- 0
for (var in c(paste0("e1.",  c(53:57), "c"))){
  result$luxury_exp <-  result$luxury_exp + (30/7)*ifelse(is.na(result[, var])| result[, var] > 900000, 0, unlist(result[, var]))
}


for (var in c("e7.4b_m", "e7.5b_m",  "e7.6b_m", "e7.10b_m")){
  result$luxury_exp <-  result$luxury_exp + (1/3)*ifelse(is.na(result[, var])| result[, var] > 900000, 0, unlist(result[, var]))
}

for (var in c("e13.1b_m", "e13.2b_m",  "e13.21b_m", "e13.34b_m", 
              "e13.4b_m", "e13.5b_m", "e13.10b_m")){
  result$luxury_exp <-  result$luxury_exp + ifelse(is.na(result[, var])| result[, var] > 900000, 0, as.numeric(unlist(result[, var])))
}




#Inflation adjustment
for(year in 2004:2019){
  i <- which(2019:2004 == year) 
  result$luxury_exp[result[,"year"] == year] <- result$luxury_exp[result[,"year"] == year]/(inflation[[i]][[3]]/100)
}

for(year in 2004:2019){
  i <- which(2019:2004 == year) 
  result$food_essential[result[,"year"] == year] <- result$food_essential[result[,"year"] == year]/(inflation[[i]][[3]]/100)
}

for(year in 2004:2019){
  i <- which(2019:2004 == year) 
  result$loan[result[,"year"] == year] <- result$loan[result[,"year"] == year]/(inflation[[i]][[3]]/100)
}

for(year in 2004:2019){
  i <- which(2019:2004 == year) 
  result$clothes[result[,"year"] == year] <- result$clothes[result[,"year"] == year]/(inflation[[i]][[3]]/100)
}

for(year in 2004:2019){
  i <- which(2019:2004 == year) 
  result$f11_m[result[,"year"] == year] <- result$f11_m[result[,"year"] == year]/(inflation[[i]][[3]]/100)
}

for(year in 2004:2019){
  i <- which(2019:2004 == year) 
  result$savings[result[,"year"] == year] <- result$savings[result[,"year"] == year]/(inflation[[i]][[3]]/100)
}

for(year in 2004:2019){
  i <- which(2019:2004 == year) 
  result$fuel[result[,"year"] == year] <- result$fuel[result[,"year"] == year]/(inflation[[i]][[3]]/100)
}

for(year in 2004:2019){
  i <- which(2019:2004 == year) 
  result$utilities[result[,"year"] == year] <- result$utilities[result[,"year"] == year]/(inflation[[i]][[3]]/100)
}

for(year in 2004:2019){
  i <- which(2019:2004 == year) 
  result$essential_sevices[result[,"year"] == year] <- result$essential_sevices[result[,"year"] == year]/(inflation[[i]][[3]]/100)
}



result$f11_m <- result$f11_m/1000
result$food_essential <- result$food_essential/1000
result$clothes <- result$clothes/1000 
result$luxury_exp <- result$luxury_exp/1000
result$savings <-result$savings/1000
result$loan <- result$loan/1000
result$fuel <- result$fuel/1000
result$utilities <-result$utilities/1000
result$essential_sevices <- result$essential_sevices/1000


#All basic expenditure
result$all_basic_exp <- result$food_essential  + result$clothes + result$fuel + result$utilities + result$essential_sevices


#Cleaning
result[which(result$zBMI > 6 | result$zBMI < -6),  "zBMI"] <- NA
result[which(result$high_starch_cons > 100),  "high_starch_cons"] <- NA


result$chronic_cnd_count
table(unlist(result[result$age %in% 6:8  & is.na(result$child3_m)  & result$child_order == 2 & 
               result$birth_timeline <= 36 & result$birth_timeline >= -36 & result$year %in% 2011:2017, "chronic_cnd_count" ]>0))


table(result[, "n126.3_m" ])
table(result$j72.5c_m, result$year)



#Cases

table(result[result$age %in% 6:8  & is.na(result$child3_m) & result$child_order == 2 & 
               result$birth_timeline <= 36 & result$birth_timeline >= -36 & result$year %in% 2011:2017,  "year"])

table(result[result$age %in% 6:8  & is.na(result$child3_m) & result$child_order == 2 & 
               result$birth_timeline <= 36 & result$birth_timeline >= 0 & result$year %in% 2011:2017,  "year"])

table(result[result$age %in% 6:8  & is.na(result$child3_m) & result$child_order == 2 & 
               result$birth_timeline < 0 & result$birth_timeline >= -36 & result$year %in% 2011:2017,  "year"])


table(result[result$age %in% 6:8  & is.na(result$child3_m) & result$child_order == 2 & 
               result$birth_timeline <= 36 & result$birth_timeline >= -36 & result$year %in% 2011:2017,  "year_hh_m"])

table(result[result$age %in% 6:8  & is.na(result$child3_m) & result$child_order == 2 & 
               result$birth_timeline <= 36 & result$birth_timeline >= 0 & result$year %in% 2011:2017,  "year_hh_m"])

table(result[result$age %in% 6:8  & is.na(result$child3_m) & result$child_order == 2 & 
               result$birth_timeline < 0 & result$birth_timeline >= -36 & result$year %in% 2011:2017,  "year_hh_m"])


table(result[result$age %in% 6:8  & is.na(result$child3_m) & result$child_order == 2 & 
               result$birth_timeline <= 36 & result$birth_timeline >= -36 & result$year %in% 2011:2017,  "j69.9c"])
table(result[result$age %in% 6:8  & is.na(result$child3_m) & result$child_order == 2 & 
               result$birth_timeline <= 36 & result$birth_timeline >= 0 & result$year %in% 2011:2017,  "j69.9c"])

table(result[result$age %in% 6:8  & is.na(result$child3_m) & result$child_order == 2 & 
               result$birth_timeline < 0 & result$birth_timeline >= -36 & result$year %in% 2011:2017,  "j69.9c"])




