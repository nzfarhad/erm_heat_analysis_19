##################################################
## Title: Data analysis script for ERM HEAT
## Author: Sayed Farhad Nabizada
## Date created: 10/20/2019
##################################################

# load libraries
library(dplyr)
library(readxl)

# Clean column names
rename1 <- function(d1) {
  sub("/", ".", names(d1))
} 

# row sum function
row_sum <- function(data, vars) {
  rowSums(data[vars], na.rm = TRUE)
  
}

# load data
df <- read_excel("input/data/HEAT_master_June_July_Agust_21102019.xlsx") %>%
  type.convert()
names(df) <- rename1(df)

# filter data for a particular month
df <- df %>%
  filter(Month == "June")


# create data frame for results
results <- data.frame(row.names = 1)
# General information 

# total households
results$num_hh <- nrow(df)

# total population vars
population_all_vars <- c(
"S1_headOfHousehold.m_under5",
"S1_headOfHousehold.f_under5",
"S1_headOfHousehold.m6_18",
"S1_headOfHousehold.f6_18",
"S1_headOfHousehold.m19_59",
"S1_headOfHousehold.f19_59",
"S1_headOfHousehold.m60",
"S1_headOfHousehold.f60"
)

female_pop_vars <- c( 
  "S1_headOfHousehold.f_under5",
  "S1_headOfHousehold.f6_18",
  "S1_headOfHousehold.f19_59",
  "S1_headOfHousehold.f60"
)

male_pop_vars <- c(
  "S1_headOfHousehold.m_under5",
  "S1_headOfHousehold.m6_18",
  "S1_headOfHousehold.m19_59",
  "S1_headOfHousehold.m60"
)

total_population <- row_sum(df, population_all_vars) %>% sum()
total_female_pop <- row_sum(df,female_pop_vars ) %>% sum()
total_male_pop <- row_sum(df,male_pop_vars) %>% sum()
results$num_ind <- total_population

### Female percentages
results$p_female <- (total_female_pop / total_population) * 100
results$p_female_0_5 <- (sum(df$S1_headOfHousehold.f_under5) / total_population) * 100
results$p_female_6_17 <- (sum(df$S1_headOfHousehold.f6_18) / total_population) * 100
results$p_female_18_59 <- (sum(df$S1_headOfHousehold.f19_59) / total_population) *100
results$p_female_60 <- (sum(df$S1_headOfHousehold.f60) / total_population) *100

### Male percentages
results$p_male <- (total_male_pop / total_population) * 100
results$p_male_0_5 <- (sum(df$S1_headOfHousehold.m_under5) / total_population) * 100
results$p_male_6_17 <- (sum(df$S1_headOfHousehold.m6_18) / total_population) * 100
results$p_male_18_59 <- (sum(df$S1_headOfHousehold.m19_59) / total_population) *100
results$p_male_60 <- (sum(df$S1_headOfHousehold.m60) / total_population) *100

# average hh size
results$avg_hh_size <- mean(row_sum(df, population_all_vars))

# female hhh
df$vulnerability.fem_hhh[is.na(df$vulnerability.fem_hhh)] <- 0
results$p_female_hh_head <- (sum(df$vulnerability.fem_hhh, na.rm = T ) / results$num_hh) * 100 
results$p_nfemale_hh_head <- 100 - results$p_female_hh_head

# elderly hhh
df$vulnerability.elderly_hhh[is.na(df$vulnerability.elderly_hhh)] <- 0
results$p_elderly_hh_head <- (sum(df$vulnerability.elderly_hhh, na.rm = T) / results$num_hh) * 100
results$p_nelderly_hh_head <- 100 - results$p_elderly_hh_head

# child hhh
df$vulnerability.child_hhh[is.na(df$vulnerability.child_hhh)] <- 0
results$p_child_hh_head <- (sum(df$vulnerability.child_hhh, na.rm = T) / results$num_hh) * 100
results$p_nchild_hh_head <- 100 - results$p_child_hh_head

# hh with at least one disabled member
df$vulnerability.disable[is.na(df$vulnerability.disable)] <- 0
results$p_hh_1dbility <- (sum(df$vulnerability.disable, na.rm = T) / results$num_hh) * 100
results$p_hh_n1dbility <- 100 - results$p_hh_1dbility

# hh with at least one chronically ill member
df$vulnerability.chro_ill[is.na(df$vulnerability.chro_ill)] <- 0
results$p_hh_1ill <- (sum(df$vulnerability.chro_ill, na.rm = T) / results$num_hh) * 100
results$p_hh_n1ill <- 100 - results$p_hh_1ill

# displacement
# Households reporting being displaced:
displaced_perc <- prop.table(table(df$S3_GEN_ASSESS.status))
results$p_hh_dp <- (displaced_perc[["conf_idp"]] + displaced_perc[["nat_disaster_idp"]]) * 100

# Households reporting being displaced due to conflict:
results$p_hh_dp_conflict <- displaced_perc[["conf_idp"]] * 100

# Households reporting being displaced due to natural disasters:
results$p_hh_dp_natdis <- displaced_perc[["nat_disaster_idp"]] * 100

# Average reported monthly household income: Income before shock:
results$in_b4_shk <- mean(df$S4_financial_ASSESS.income_before, na.rm = T)

#Average reported monthly household income: Income after shock:
results$in_aft_shk <- mean(df$S4_financial_ASSESS.income_after, na.rm = T)

# Reported main source of income since attack: None
income_source_table <- table(df$S4_financial_ASSESS.main_income)
income_source_table_prop <- (prop.table(income_source_table))
results$p_in_none <- income_source_table_prop[["jobless"]] * 100
results$p_in_unskil <- income_source_table_prop[["unskill_lab"]] * 100
results$p_in_agri <- income_source_table_prop[["farmer"]] * 100
results$p_in_skil <- income_source_table_prop[["skill_lab"]] * 100
results$p_in_other <- income_source_table_prop[["other"]] * 100


# Working demographics: Avg by HH of men / women (>16) working:
results$avg_m_wrk <- mean(df$S4_financial_ASSESS.m_bread, na.rm = T)
results$avg_f_wrk <- mean(df$S4_financial_ASSESS.f_bread, na.rm = T)


# Proportion of households reporting newly accrued debt since shock: More than 8000 AFN
# results$p_hh_debt_8k <- 0
  
new_debt <- prop.table(table(df$Smore_8000_financial_ASSESS.new_debts)) 

# Proportion of households reporting newly accrued debt since shock: More than 8000 AFN
results$p_hh_debt_8k <- new_debt[["more_8000"]] * 100

# Proportion of households reporting newly accrued debt since shock: 2000 to 8000 AFN
results$p_hh_debt_2k8k <- new_debt[["between_2000_8000"]] * 100

# Proportion of households reporting newly accrued debt since shock: Less than 2000 AFN
results$p_hh_debt_2k <- new_debt[["less_afs_2000"]] * 100

# Proportion of households reporting newly accrued debt since shock: No debt
results$p_hh_debt_0 <- new_debt[["no_debts"]] * 100


# Average dependency ratio: Elderly-headed households
df_elderly_hh <- df %>%
  filter(vulnerability.elderly_hhh == 1)
total_population_elderly <- row_sum(df_elderly_hh, population_all_vars) %>% sum()

total_working_members_elderly <- sum(df_elderly_hh$S4_financial_ASSESS.m_bread) + sum(df_elderly_hh$S4_financial_ASSESS.f_bread)

results$avg_dr_elderly <- total_population_elderly / total_working_members_elderly

# Average dependency ratio: Male-headed households
df_male_hh <- df %>%
  filter(vulnerability.fem_hhh == 0)
total_population_male_hh <- row_sum(df_male_hh, population_all_vars) %>% sum()

total_working_members_male_hh <- sum(df_male_hh$S4_financial_ASSESS.m_bread, na.rm = T) + sum(df_male_hh$S4_financial_ASSESS.f_bread, na.rm = T)

results$avg_dr_m <- total_population_male_hh / total_working_members_male_hh


# Average dependency ratio: Female-headed households
df_female_hh <- df %>%
  filter(vulnerability.fem_hhh == 1)

total_population_female_hh <- row_sum(df_female_hh, population_all_vars) %>% sum()

total_working_members_female_hh <- sum(df_female_hh$S4_financial_ASSESS.m_bread) + sum(df_female_hh$S4_financial_ASSESS.f_bread)

results$avg_dr_f <- total_population_female_hh / total_working_members_female_hh


# fcs score
df$fcs_grp.fc_cereals_w <- df$fcs_grp.fc_cereals * 2
df$fcs_grp.fc_pulses_w <- df$fcs_grp.fc_pulses * 3
df$fcs_grp.fc_vegetables_w <- df$fcs_grp.fc_vegetables * 1
df$fcs_grp.fc_fruits_w <- df$fcs_grp.fc_fruits * 1
df$fcs_grp.fc_meat_w <- df$fcs_grp.fc_meat * 4
df$fcs_grp.fc_milk_w <- df$fcs_grp.fc_milk * 4
df$fcs_grp.fc_sugar_w <- df$fcs_grp.fc_sugar * 0.5
df$fcs_grp.fc_oil_w <- df$fcs_grp.fc_oil * 0.5

fcs_vars <- c(
  "fcs_grp.fc_cereals_w",
  "fcs_grp.fc_pulses_w",
  "fcs_grp.fc_vegetables_w",
  "fcs_grp.fc_fruits_w",
  "fcs_grp.fc_meat_w",
  "fcs_grp.fc_milk_w",
  "fcs_grp.fc_sugar_w",
  "fcs_grp.fc_oil_w"
)

df$fcs_score <- row_sum(df, fcs_vars)


df <- df %>% 
  mutate(
    fcs_cat = case_when(
      fcs_score <= 28  ~ "poor",
      fcs_score >= 28.1 & fcs_score < 42  ~ "borderline",
      fcs_score >= 42  ~ "accceptable",
      TRUE ~ NA_character_
    )
  )


fcs_prop_table <- prop.table(table(df$fcs_cat))

#FCS percentages
results$p_fd_con_poor <- fcs_prop_table[["poor"]] * 100
results$p_fd_con_border <- fcs_prop_table[["borderline"]] * 100
results$p_fd_con_accept <- fcs_prop_table[["accceptable"]] * 100


# Food stocks
food_tock_perc <- prop.table(table(df$food_stocks))

# Food stocks: No stocks:
results$p_fd_stk_no <- food_tock_perc[["1"]] * 100

# Food stocks: Less than 3 weeks
results$p_fd_stk_1w <- food_tock_perc[["2"]] * 100

# Food stocks: 1 to 3 weeks
results$p_fd_stk_1w3w <- food_tock_perc[["3"]] * 100

# Food stocks: 3 weeks to 3 months
results$p_fd_stk_3w3m <- food_tock_perc[["4"]] * 100

# Food stocks: Over 3 months - Zero entries in dataset 
results$p_fd_stk_3m <- food_tock_perc[["5"]] * 100


# Coping strategies used as reported by households in the 7 days prior to data collection: Relying on less

# recode 
strat_relya <- ifelse(as.numeric(as.character(df$rCSI_grp.rCSI_1)) > 0, 1,0)
strat_brrw <- ifelse(as.numeric(as.character(df$rCSI_grp.rCSI_2)) > 0, 1,0)
p_fd_strat_redsize <- ifelse(as.numeric(as.character(df$rCSI_grp.rCSI_3)) > 0, 1,0)
p_fd_strat_restric <- ifelse(as.numeric(as.character(df$rCSI_grp.rCSI_4)) > 0, 1,0)
p_fd_strat_rednum <- ifelse(as.numeric(as.character(df$rCSI_grp.rCSI_5)) > 0, 1,0)
  

# Coping strategies used as reported by households in the 7 days prior to data collection: Relying on less prefered or cheaper food
strat_relya_perc <- prop.table(table(strat_relya))
results$p_fd_strat_rely <- strat_relya_perc[["1"]] * 100

# Coping strategies used as reported by households in the 7 days prior to data collection: Borrowing food; assistance from community
strat_brrw_perc <- prop.table(table(strat_brrw))
results$p_fd_strat_brrw <- strat_brrw_perc[["1"]] * 100

# Coping strategies used as reported by households in the 7 days prior to data collection: Reducing size of meals
p_fd_strat_redsize_pecr <- prop.table(table(p_fd_strat_redsize))
results$p_fd_strat_redsize <- p_fd_strat_redsize_pecr[["1"]] * 100

# Coping strategies used as reported by households in the 7 days prior to data collection: Restricting consumption by adults for children to eat
p_fd_strat_restric_perc <- prop.table(table(p_fd_strat_restric))
results$p_fd_strat_restric <- p_fd_strat_restric_perc[["1"]] * 100

# Coping strategies used as reported by households in the 7 days prior to data collection: Reducing number of meals daily
p_fd_strat_rednum_perc <- prop.table(table(p_fd_strat_rednum))
results$p_fd_strat_rednum <- p_fd_strat_rednum_perc[["1"]] * 100

  
# Reported distance to closest functioning market: Average distance to closest market in kilometres:
results$avg_mkt_dist <- mean(df$mark_dist_km, na.rm = T)

# Reported distance to closest functioning market: Average time to closest market in minutes:
results$avg_mkt_time <- mean(df$mark_dist_min, na.rm = T)

# Households reporting not having access to enough water at the time of the assessment: Not enough drinking water
drinking_water_perc <- prop.table(table(df$S6_wash.drinking))
results$p_wat_drk <- drinking_water_perc[["0"]] * 100 
results$p_nwat_drk <- drinking_water_perc[["1"]] * 100

# Households reporting not having access to enough water at the time of the assessment: Not enough bathing water
bathing_water_perc <- prop.table(table(df$S6_wash.bathing))
results$p_wat_bath <- bathing_water_perc[["0"]] * 100
results$p_nwat_bath <- bathing_water_perc[["1"]] * 100

# Households reporting not having access to enough water at the time of the assessment: Not enough cooking water
cooking_water_perc <- prop.table(table(df$S6_wash.cooking))
results$p_wat_cook <- cooking_water_perc[["0"]] * 100
results$p_nwat_cook <- cooking_water_perc[["1"]] * 100

# Types of latrine available at the time of the assessment as reported by households:
df$S6_wash.lat_type <- as.character(df$S6_wash.lat_type)

df <- df %>%
  mutate(
    S6_wash.lat_type_2 = case_when(
      is.na(S6_wash.lat_type) & S6_wash.lat_avail == 1 ~ "unknown",
      S6_wash.lat_type == "family_lat" ~ "family_lat",
      S6_wash.lat_type == "comm_lat" ~ "comm_lat",
      S6_wash.lat_type == "family_vip" ~ "family_vip",
      S6_wash.lat_type == "open_Def" | S6_wash.lat_avail == 0 ~ "open_Def"
      
    )
  )

latrine_type <- prop.table(table(df$S6_wash.lat_type_2))

results$p_lat_fam <- latrine_type[["family_lat"]] * 100
results$p_lat_no <- latrine_type[["open_Def"]] * 100
results$p_lat_com <- latrine_type[["comm_lat"]] * 100
results$p_lat_vip <- latrine_type[["family_vip"]] * 100
results$p_lat_ukwn <- latrine_type[["unknown"]] * 100

# Stable water source: Households reporting access to a stable water source:
stable_water <- prop.table(table(df$S6_wash.wat_stable))
results$p_swat_yes <- stable_water[["1"]] * 100

# Stable water source: Average distance to their main stable water source:
results$avg_swat_dist <- mean(df$S6_wash.wat_km, na.rm = T)

# Stable water source: Average time to their main stable water source:
results$avg_swat_time <- mean(df$S6_wash.wat_min, na.rm = T)


# Households self reported needs by level of priority: 
priority_1 <- prop.table(table(df$s9_priorities.priority1))
priority_2 <- prop.table(table(df$s9_priorities.priority2))
priority_3 <- prop.table(table(df$s9_priorities.priority3))

# foood
results$p_pri1_fd <- priority_1[["Food"]] * 100
results$p_pri2_fd <- priority_2[["Food"]] * 100
results$p_pri3_fd <- priority_3[["Food"]] * 100

# Shelter 
results$p_pri1_shl <- priority_1[["Shelter"]] * 100
results$p_pri2_shl <- priority_2[["Shelter"]] * 100
results$p_pri3_shl <- priority_3[["Shelter"]] * 100

# NFI
results$p_pri1_nfi <- priority_1[["NFIs"]] * 100
results$p_pri2_nfi <- priority_2[["NFIs"]] * 100
results$p_pri3_nfi <- priority_3[["NFIs"]] * 100

# Cash
results$p_pri1_csh <- priority_1[["cash"]] * 100
results$p_pri2_csh <- priority_2[["cash"]] * 100
results$p_pri3_csh <- priority_3[["cash"]] * 100

# Health
results$p_pri1_hth <- priority_1[["Health"]] * 100
results$p_pri2_hth <- priority_2[["Health"]] * 100
results$p_pri3_hth <- priority_3[["Health"]] * 100

# WASH
results$p_pri1_wsh <- priority_1[["WASH"]] * 100
results$p_pri2_wsh <- priority_2[["WASH"]] * 100
results$p_pri3_wsh <- priority_3[["WASH"]] * 100

# Other
results$p_pri1_other <- priority_1[["other"]] * 100
results$p_pri2_other <- priority_2[["other"]] * 100
results$p_pri3_other <- priority_3[["other"]] * 100

# Proportion of children reportedly not attending school at the time of the assessment: 
male_attending <-  sum(df$S3_GEN_ASSESS.m_attend, na.rm = T)
female_attending <- sum(df$S3_GEN_ASSESS.f_attend, na.rm = T)
sum_male_6_18 <- sum(df$S1_headOfHousehold.m6_18)
sum_female_6_18 <- sum(df$S1_headOfHousehold.f6_18)

# Proportion of children reportedly not attending school at the time of the assessment: Girl
results$p_grl_nschl <- (female_attending / sum_female_6_18) * 100
results$p_grl_schl <- 100 - results$p_grl_nschl

# Proportion of children reportedly not attending school at the time of the assessment: Boys
results$p_boy_nschl <- (male_attending/ sum_male_6_18) * 100
results$p_boy_schl <- 100 - results$p_boy_nschl


shelter_type <- prop.table(table(df$S7_SHELTER.h_type))

# Shelter types: House
results$p_shl_hse <- shelter_type[["house"]] * 100

# Shelter types: Open area
results$p_shl_open <- shelter_type[["open_area"]] * 100

# Shelter types: Tent
results$p_shl_tent <- shelter_type[["tent"]] * 100

# Shelter types: Shelter
results$p_shl_shel <- shelter_type[["shelter"]] * 100

# Shelter types: public_compoun
results$p_shl_public_compoun <- shelter_type[["public_compoun"]] * 100

# Accommodation agreements: Hosted/Shared
accom_arrangement <- prop.table(table(df$S7_SHELTER.h_arrainge)) 

# Accommodation agreements: Hosted/Shared
results$p_acm_host <- accom_arrangement[["Hosted"]] * 100

# Accommodation agreements: Free of charge
results$p_acm_free <- accom_arrangement[["Free_of_charge"]] * 100

# Accommodation agreements: Rented
results$p_acm_rent <- accom_arrangement[["Rented"]] * 100

# Accommodation agreements: Owned
results$p_acm_own <- accom_arrangement[["Owned"]] * 100

# Accommodation agreements: Squatting
results$p_acm_squat <- accom_arrangement[["Squatting"]] * 100


# rcsi index
df$rCSI_grp.rCSI_1 <- as.integer(as.character(df$rCSI_grp.rCSI_1))
df$rCSI_grp.rCSI_1 <- df$rCSI_grp.rCSI_1 * 1
df$rCSI_grp.rCSI_2 <- df$rCSI_grp.rCSI_2 * 2
df$rCSI_grp.rCSI_3 <- df$rCSI_grp.rCSI_3 * 1
df$rCSI_grp.rCSI_4 <- df$rCSI_grp.rCSI_4 * 3
df$rCSI_grp.rCSI_5 <- df$rCSI_grp.rCSI_5 * 1


rcsi_vars <- c(
  "rCSI_grp.rCSI_1",
  "rCSI_grp.rCSI_2",
  "rCSI_grp.rCSI_3",
  "rCSI_grp.rCSI_4",
  "rCSI_grp.rCSI_5")

df$rcsi_score_weighted <- row_sum(df, rcsi_vars)

df <- df %>%
  mutate(
    rcsi_severity = case_when(
      rcsi_score_weighted >= 0 & rcsi_score_weighted < 4 ~ "no_or_low_coping",
      rcsi_score_weighted > 3 & rcsi_score_weighted < 10 ~ "medium_coping",
      rcsi_score_weighted >= 10 ~ "high_coping"
    )
  )

# rcsi serrity perc
rcsi_severity_perc <- prop.table(table(df$rcsi_severity))
results$rcsi_high <- rcsi_severity_perc[["high_coping"]] * 100
results$rcsi_medium <- rcsi_severity_perc[["medium_coping"]] * 100
results$rcsi_low <- rcsi_severity_perc[["no_or_low_coping"]] * 100



# output
write.csv(results, "output/HEAT_datamerge_August_2019.csv", row.names = F)


### end
  
  