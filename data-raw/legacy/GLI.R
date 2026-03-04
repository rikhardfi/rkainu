source("Constants.R")

library(rspiro)
library(dplyr)
library(tidyr)
library(flextable)
library(officer)
library(patchwork)
library(ggplot2)

df_GLI <- df %>%
  filter(!is.na(FVC)) %>%
  select(ID, age, sex, days_since_zero, weight, height, FVC, FEV1, FEV1FVC)

#Lisätään ethnicity osio ja siihen numerot 1 (caucasian) ja muutetaan sukupuolet 1 ja 2 numeroiksi

# Muuta sex: 0 = mies → 1, 1 = nainen → 2
df_GLI$sex <- ifelse(df_GLI$sex == 0, 1, 2)

# Lisää ethnicity-sarake: kaikilla arvo 1 (Caucasian)
df_GLI$ethnicity <- 1


# Oletetaan, että df_GLI:ssa on sarakkeet age, height (cm), sex (1=mies, 2=nainen), ethnicity (1=suomalaiset), FEV1, FVC, FEV1FVC
# Ja funktiot pred_GLI ja zscore_GLI ovat ladattu

# Alustetaan sarakkeet
df_GLI$FEV1_pred <- NA
df_GLI$FVC_pred <- NA
df_GLI$FEV1FVC_pred <- NA

df_GLI$FEV1_z <- NA
df_GLI$FVC_z <- NA
df_GLI$FEV1FVC_z <- NA

valid_rows <- complete.cases(df_GLI[, c("age", "height", "sex", "ethnicity", "FEV1", "FVC", "FEV1FVC")]) &
  df_GLI$height > 100 & df_GLI$height < 250

for (i in which(valid_rows)) {
  age_i <- df_GLI$age[i]
  height_i <- df_GLI$height[i] / 100
  sex_i <- df_GLI$sex[i]
  eth_i <- df_GLI$ethnicity[i]
  
  # Ennustearvot
  pred_i <- pred_GLI(age = age_i, height = height_i, gender = sex_i, ethnicity = eth_i,
                     param = c("FEV1", "FVC", "FEV1FVC"))
  
  if (is.data.frame(pred_i) && nrow(pred_i) == 1) {
    df_GLI$FEV1_pred[i] <- pred_i$pred.FEV1
    df_GLI$FVC_pred[i] <- pred_i$pred.FVC
    df_GLI$FEV1FVC_pred[i] <- pred_i$pred.FEV1FVC
  }
  
  # Z-scoret
  zs_i <- zscore_GLI(age = age_i, height = height_i, gender = sex_i, ethnicity = eth_i,
                     FEV1 = df_GLI$FEV1[i], FVC = df_GLI$FVC[i], FEV1FVC = df_GLI$FEV1FVC[i])
  
  if (is.data.frame(zs_i) && nrow(zs_i) == 1) {
    df_GLI$FEV1_z[i] <- zs_i$z.score.FEV1
    df_GLI$FVC_z[i] <- zs_i$z.score.FVC
    df_GLI$FEV1FVC_z[i] <- zs_i$z.score.FEV1FVC
  }
}



#Muodostetaan table 1

df_summary_GLI <-df_GLI %>%
  group_by(ID) %>%
  summarize(
    n_visits             = n(),
    age_min              = min(age, na.rm = TRUE),
    age_max              = max(age, na.rm = TRUE),
    follow_up_time       = age_max - age_min,
    sex                  = first(sex),  
    FVC_baseline         = FVC[which.min(age)],
    z_FVC_baseline       = FVC_z[which.min(age)],
    FEV1_baseline        = FEV1[which.min(age)],
    z_FEV1_baseline      = FEV1_z[which.min(age)],
    FEV1FVC_baseline     = FEV1FVC[which.min(age)],
    z_FEV1FVC_baseline   = FEV1FVC_z[which.min(age)],
    FVC_max              = max(FVC, na.rm = TRUE),
    z_FVC_max            = max(FVC_z, na.rm = TRUE),
    FEV1_max             = max(FEV1, na.rm = TRUE),
    z_FEV1_max           = max(FEV1_z, na.rm = TRUE),
    FEV1FVC_max          = max(FEV1FVC, na.rm = TRUE),
    z_FEV1FVC_max        = max(FEV1FVC_z, na.rm = TRUE),
    pred_FVC_baseline    = FVC_pred[which.min(age)],
    pred_FEV1_baseline   = FEV1_pred[which.min(age)],
    pred_FEV1FVC_baseline= FEV1FVC_pred[which.min(age)],
    pred_FVC_max         = FVC_pred[which.max(FVC)],
    pred_FEV1_max        = FEV1_pred[which.max(FEV1)],
    pred_FEV1FVC_max     = FEV1FVC_pred[which.max(FEV1FVC)],
    FVC_perc_baseline    = (FVC_baseline    / pred_FVC_baseline)    * 100,
    FEV1_perc_baseline   = (FEV1_baseline   / pred_FEV1_baseline)   * 100,
    FEV1FVC_perc_baseline= (FEV1FVC_baseline/ pred_FEV1FVC_baseline)* 100,
    FVC_perc_max         = (FVC_max         / pred_FVC_max)         * 100,
    FEV1_perc_max        = (FEV1_max        / pred_FEV1_max)        * 100,
    FEV1FVC_perc_max     = (FEV1FVC_max     / pred_FEV1FVC_max)     * 100
    ) %>%
  
  ungroup() %>%
  mutate(
    # compute change from baseline to max
    delta_FVC         = FVC_perc_max      - FVC_perc_baseline,
    delta_FEV1        = FEV1_perc_max     - FEV1_perc_baseline,
    delta_FEV1FVC     = FEV1FVC_perc_max  - FEV1FVC_perc_baseline,
    delta_zFVC        = z_FVC_max         - z_FVC_baseline,
    delta_zFEV1       = z_FEV1_max        - z_FEV1_baseline,
    delta_zFEV1FVC    = z_FEV1FVC_max     - z_FEV1FVC_baseline,
  ) 

df_summary_GLI <- df_summary_GLI %>%
  mutate(across(ends_with("_max"), ~ifelse(is.infinite(.), NA, .)))

# Compute overall and sex‐specific medians of the deltas 
#(lasketaan mediaanit yleisesti ja sukupuolittain)
delta_FVC_total      <- median(df_summary_GLI$delta_FVC,      na.rm = TRUE)
delta_FEV1_total     <- median(df_summary_GLI$delta_FEV1,     na.rm = TRUE)
delta_FEV1FVC_total  <- median(df_summary_GLI$delta_FEV1FVC,  na.rm = TRUE)
delta_zFVC_total     <- median(df_summary_GLI$delta_zFVC,     na.rm = TRUE)
delta_zFEV1_total    <- median(df_summary_GLI$delta_zFEV1,    na.rm = TRUE)
delta_zFEV1FVC_total <- median(df_summary_GLI$delta_zFEV1FVC, na.rm = TRUE)

delta_FVC_male      <- median(df_summary_GLI$delta_FVC[df_summary_GLI$sex==1],      na.rm = TRUE)
delta_FEV1_male     <- median(df_summary_GLI$delta_FEV1[df_summary_GLI$sex==1],     na.rm = TRUE)
delta_FEV1FVC_male  <- median(df_summary_GLI$delta_FEV1FVC[df_summary_GLI$sex==1],  na.rm = TRUE)
delta_zFVC_male     <- median(df_summary_GLI$delta_zFVC[df_summary_GLI$sex==1],     na.rm = TRUE)
delta_zFEV1_male    <- median(df_summary_GLI$delta_zFEV1[df_summary_GLI$sex==1],    na.rm = TRUE)
delta_zFEV1FVC_male <- median(df_summary_GLI$delta_zFEV1FVC[df_summary_GLI$sex==1], na.rm = TRUE)

delta_FVC_female      <- median(df_summary_GLI$delta_FVC[df_summary_GLI$sex==2],      na.rm = TRUE)
delta_FEV1_female     <- median(df_summary_GLI$delta_FEV1[df_summary_GLI$sex==2],     na.rm = TRUE)
delta_FEV1FVC_female  <- median(df_summary_GLI$delta_FEV1FVC[df_summary_GLI$sex==2],  na.rm = TRUE)
delta_zFVC_female     <- median(df_summary_GLI$delta_zFVC[df_summary_GLI$sex==2],     na.rm = TRUE)
delta_zFEV1_female    <- median(df_summary_GLI$delta_zFEV1[df_summary_GLI$sex==2],    na.rm = TRUE)
delta_zFEV1FVC_female <- median(df_summary_GLI$delta_zFEV1FVC[df_summary_GLI$sex==2], na.rm = TRUE)


### PART 2. Split summary by sex and count participants
male_data_GLI   <- df_summary_GLI %>% filter(sex == 1)
female_data_GLI <- df_summary_GLI %>% filter(sex == 2)
n_total     <- nrow(df_summary_GLI)
n_male      <- nrow(male_data_GLI)
n_female    <- nrow(female_data_GLI)

### PART 4. Helper functions for table summaries ja lisätään merkinnät * ja †
summarize_visits <- function(x) {
  paste0(
    round(mean(x, na.rm = TRUE), 1), " ± ", round(sd(x, na.rm = TRUE), 1), "†",
    "; ",
    round(median(x, na.rm = TRUE), 0), " (",
    round(quantile(x, 0.25, na.rm = TRUE), 0), "–",
    round(quantile(x, 0.75, na.rm = TRUE), 0), ")*"
  )
}
summarize_median_iqr <- function(x, digits = 2) {
  paste0(
    format(round(median(x, na.rm = TRUE), digits), nsmall = digits),
    " (",
    format(round(quantile(x, 0.25, na.rm = TRUE), digits), nsmall = digits),
    "–",
    format(round(quantile(x, 0.75, na.rm = TRUE), digits), nsmall = digits),
    ")*"
  )
}
summarize_mean_sd <- function(x, digits = 2) {
  paste0(
    format (round(mean(x, na.rm = TRUE), digits), nsmall = digits), 
    " ± ", 
    format (round(sd(x, na.rm = TRUE), digits), nsmall = digits),
    "†")
}
summarize_binary <- function(x) {
  paste0(sum(x == 1, na.rm = TRUE), " (", round(mean(x == 1, na.rm = TRUE) * 100, 1), "%)")
}

### PART 5. Build rows for the summary table
row_N_GLI                    <- tibble(Variable="N",                               Total=paste0("n = ",n_total),              Male=paste0("n = ",n_male),              Female=paste0("n = ",n_female))
row_n_visits_GLI             <- tibble(Variable="Number of Visits",              Total=summarize_visits(df_summary_GLI$n_visits),   Male=summarize_visits(male_data_GLI$n_visits), Female=summarize_visits(female_data_GLI$n_visits))
row_age_baseline_GLI         <- tibble(Variable="Age at first visit (Years)",       Total=summarize_median_iqr(df_summary_GLI$age_min,1), Male=summarize_median_iqr(male_data_GLI$age_min,1), Female=summarize_median_iqr(female_data_GLI$age_min,1))
row_followup_GLI             <- tibble(Variable="Follow-Up Time (Years)",       Total=summarize_mean_sd(df_summary_GLI$follow_up_time), Male=summarize_mean_sd(male_data_GLI$follow_up_time), Female=summarize_mean_sd(female_data_GLI$follow_up_time))
row_sex_GLI                 <- tibble(Variable="Sex (female, %)",               Total=paste0(sum(df_summary_GLI$sex==2)," (",round(mean(df_summary_GLI$sex==2)*100,1),")"), Male="", Female="")
row_FVC_baseline_abs_GLI     <- tibble(Variable="FVC at first visit (L)",         Total="", Male=summarize_median_iqr(male_data_GLI$FVC_baseline,2), Female=summarize_median_iqr(female_data_GLI$FVC_baseline,2))
row_FVC_perc_baseline_GLI    <- tibble(Variable="FVC % predicted at first visit", Total=summarize_median_iqr(df_summary_GLI$FVC_perc_baseline,2), Male=summarize_median_iqr(male_data_GLI$FVC_perc_baseline,2), Female=summarize_median_iqr(female_data_GLI$FVC_perc_baseline,2))
row_zFVC_baseline_GLI        <- tibble(Variable="FVC z-score at first visit",     Total=summarize_median_iqr(df_summary_GLI$z_FVC_baseline,2),     Male=summarize_median_iqr(male_data_GLI$z_FVC_baseline,2),     Female=summarize_median_iqr(female_data_GLI$z_FVC_baseline,2))
row_FEV1_baseline_abs_GLI    <- tibble(Variable="FEV1 at first visit (L)",        Total="", Male=summarize_median_iqr(male_data_GLI$FEV1_baseline,2),     Female=summarize_median_iqr(female_data_GLI$FEV1_baseline,2))
row_FEV1_perc_baseline_GLI   <- tibble(Variable="FEV1 % predicted at first visit",Total=summarize_median_iqr(df_summary_GLI$FEV1_perc_baseline,2), Male=summarize_median_iqr(male_data_GLI$FEV1_perc_baseline,2), Female=summarize_median_iqr(female_data_GLI$FEV1_perc_baseline,2))
row_zFEV1_baseline_GLI       <- tibble(Variable="FEV1 z-score at first visit",   Total=summarize_median_iqr(df_summary_GLI$z_FEV1_baseline,2),  Male=summarize_median_iqr(male_data_GLI$z_FEV1_baseline,2),    Female=summarize_median_iqr(female_data_GLI$z_FEV1_baseline,2))
row_FEV1FVC_baseline_abs_GLI <- tibble(Variable="FEV1FVC at first visit (%)",    Total="", Male=summarize_median_iqr(male_data_GLI$FEV1FVC_baseline,2), Female=summarize_median_iqr(female_data_GLI$FEV1FVC_baseline,2))
row_FEV1FVC_perc_baseline_GLI<- tibble(Variable="FEV1FVC % predicted at first visit",Total=summarize_median_iqr(df_summary_GLI$FEV1FVC_perc_baseline,2),Male=summarize_median_iqr(male_data_GLI$FEV1FVC_perc_baseline,2),Female=summarize_median_iqr(female_data_GLI$FEV1FVC_perc_baseline,2))
row_zFEV1FVC_baseline_GLI    <- tibble(Variable="FEV1FVC z-score at first visit", Total=summarize_median_iqr(df_summary_GLI$z_FEV1FVC_baseline,2), Male=summarize_median_iqr(male_data_GLI$z_FEV1FVC_baseline,2), Female=summarize_median_iqr(female_data_GLI$z_FEV1FVC_baseline,2))
row_delta_FVC_GLI           <- tibble(Variable="Change in FVC (first visit to max) (%)",       Total=format(round(delta_FVC_total,2),  nsmall=2), Male=format(round(delta_FVC_male,2), nsmall=2), Female=format(round(delta_FVC_female,2),nsmall=2))
row_delta_FEV1_GLI          <- tibble(Variable="Change in FEV1 (first visit to max) (%)",      Total=format(round(delta_FEV1_total,2), nsmall=2), Male=format(round(delta_FEV1_male,2),nsmall=2), Female=format(round(delta_FEV1_female,2),nsmall=2))
row_delta_FEV1FVC_GLI       <- tibble(Variable="Change in FEV1FVC (first visit to max) (%)",  Total=format(round(delta_FEV1FVC_total,2),  nsmall=2),Male=format(round(delta_FEV1FVC_male,2),nsmall=2),Female=format(round(delta_FEV1FVC_female,2),nsmall=2))
row_delta_zFVC_GLI          <- tibble(Variable="Change in FVC z-score (first visit to max)",Total=format(round(delta_zFVC_total,2),  nsmall=2),Male=format(round(delta_zFVC_male,2),nsmall=2),Female=format(round(delta_zFVC_female,2), nsmall=2))
row_delta_zFEV1_GLI         <- tibble(Variable="Change in FEV1 z-score (first visit to max)",Total=format(round(delta_zFEV1_total,2),  nsmall=2),Male=format(round(delta_zFEV1_male,2),nsmall=2),Female=format(round(delta_zFEV1_female,2),nsmall=2))
row_delta_zFEV1FVC_GLI      <- tibble(Variable="Change in FEV1FVC z-score (first visit to max)",Total=format(round(delta_zFEV1FVC_total,2),nsmall=2),Male=format(round(delta_zFEV1FVC_male,2),nsmall=2),Female=format(round(delta_zFEV1FVC_female,2),nsmall=2))
row_FVC_max_abs_GLI         <- tibble(Variable="FVC max (L)",                          Total="", Male=summarize_median_iqr(male_data_GLI$FVC_max,2),  Female=summarize_median_iqr(female_data_GLI$FVC_max,2))
row_FVC_perc_max_GLI        <- tibble(Variable="FVC % predicted max",                  Total=summarize_median_iqr(df_summary_GLI$FVC_perc_max,2),Male=summarize_median_iqr(male_data_GLI$FVC_perc_max,2),Female=summarize_median_iqr(female_data_GLI$FVC_perc_max,2))
row_zFVC_max_GLI            <- tibble(Variable="FVC z-score max",                     Total=summarize_median_iqr(df_summary_GLI$z_FVC_max,2),     Male=summarize_median_iqr(male_data_GLI$z_FVC_max,2),     Female=summarize_median_iqr(female_data_GLI$z_FVC_max,2))
row_FEV1_max_abs_GLI        <- tibble(Variable="FEV1 max (L)",                        Total="", Male=summarize_median_iqr(male_data_GLI$FEV1_max,2),  Female=summarize_median_iqr(female_data_GLI$FEV1_max,2))
row_FEV1_perc_max_GLI       <- tibble(Variable="FEV1 % predicted max",                Total=summarize_median_iqr(df_summary_GLI$FEV1_perc_max,2),Male=summarize_median_iqr(male_data_GLI$FEV1_perc_max,2),Female=summarize_median_iqr(female_data_GLI$FEV1_perc_max,2))
row_zFEV1_max_GLI           <- tibble(Variable="FEV1 z-score max",                   Total=summarize_median_iqr(df_summary_GLI$z_FEV1_max,2),   Male=summarize_median_iqr(male_data_GLI$z_FEV1_max,2),   Female=summarize_median_iqr(female_data_GLI$z_FEV1_max,2))
row_FEV1FVC_max_abs_GLI     <- tibble(Variable="FEV1FVC max (%)",                    Total="", Male=summarize_median_iqr(male_data_GLI$FEV1FVC_max,2),Female=summarize_median_iqr(female_data_GLI$FEV1FVC_max,2))
row_FEV1FVC_perc_max_GLI    <- tibble(Variable="FEV1FVC % predicted max",          Total=summarize_median_iqr(df_summary_GLI$FEV1FVC_perc_max,2),Male=summarize_median_iqr(male_data_GLI$FEV1FVC_perc_max,2),Female=summarize_median_iqr(female_data_GLI$FEV1FVC_perc_max,2))
row_zFEV1FVC_max_GLI        <- tibble(Variable="FEV1FVC z-score max",               Total=summarize_median_iqr(df_summary_GLI$z_FEV1FVC_max,2),Male=summarize_median_iqr(male_data_GLI$z_FEV1FVC_max,2),Female=summarize_median_iqr(female_data_GLI$z_FEV1FVC_max,2))

### PART 6. Combine all rows into final master table with all results
table_1_values_GLI <- bind_rows(
  row_N_GLI,
  row_FVC_baseline_abs_GLI, row_FVC_perc_baseline_GLI, row_zFVC_baseline_GLI,
  row_FEV1_baseline_abs_GLI, row_FEV1_perc_baseline_GLI, row_zFEV1_baseline_GLI,
  row_FEV1FVC_baseline_abs_GLI, row_FEV1FVC_perc_baseline_GLI, row_zFEV1FVC_baseline_GLI,
  row_FVC_max_abs_GLI, row_FVC_perc_max_GLI, row_zFVC_max_GLI,
  row_FEV1_max_abs_GLI, row_FEV1_perc_max_GLI, row_zFEV1_max_GLI,
  row_FEV1FVC_max_abs_GLI, row_FEV1FVC_perc_max_GLI, row_zFEV1FVC_max_GLI,
  row_delta_FVC_GLI, row_delta_FEV1_GLI, row_delta_FEV1FVC_GLI,
  row_delta_zFVC_GLI, row_delta_zFEV1_GLI, row_delta_zFEV1FVC_GLI
)


### PART 7. Build flextable and export as Word document
ft <- flextable(table_1_values_GLI) %>%
  theme_vanilla() %>%
  border_remove() %>%
  hline_top(part = "header",   border = fp_border(width = 1)) %>%
  hline(i = 1, part = "body",  border = fp_border(width = 1)) %>%
  hline_bottom(border = fp_border(width = 1)) %>%
  set_table_properties(layout = "autofit", width = 1) %>%
  add_footer_lines(
    c(
      "* Presented as median and interquartile range (IQR)",
      "† Presented as mean and standard deviation (SD)"
    )
  ) %>%
  bold(i = 1:2, part = "footer") %>%
  italic(i = 1:2, part = "footer")





### Artikkeliin menevä taulukko:


row_N_GLI_a                    <- tibble(Variable="N",                                 Male=paste0("n = ",n_male),              Female=paste0("n = ",n_female))
row_FVC_max_abs_GLI_a         <- tibble(Variable="FVC (liters)",                           Male=summarize_mean_sd(male_data_GLI$FVC_max,2),  Female=summarize_mean_sd(female_data_GLI$FVC_max,2))
row_FVC_perc_max_GLI_a        <- tibble(Variable="FVC % of predicted",                  Male=summarize_mean_sd(male_data_GLI$FVC_perc_max,0),Female=summarize_mean_sd(female_data_GLI$FVC_perc_max,0))
row_zFVC_max_GLI_a            <- tibble(Variable="FVC z-score",                      Male=summarize_mean_sd(male_data_GLI$z_FVC_max,2),     Female=summarize_mean_sd(female_data_GLI$z_FVC_max,2))
row_FEV1_max_abs_GLI_a        <- tibble(Variable="FEV1 (liters)",                       Male=summarize_mean_sd(male_data_GLI$FEV1_max,2),  Female=summarize_mean_sd(female_data_GLI$FEV1_max,2))
row_FEV1_perc_max_GLI_a       <- tibble(Variable="FEV1 % of predicted",               Male=summarize_mean_sd(male_data_GLI$FEV1_perc_max,0),Female=summarize_mean_sd(female_data_GLI$FEV1_perc_max,0))
row_zFEV1_max_GLI_a           <- tibble(Variable="FEV1 z-score",                   Male=summarize_mean_sd(male_data_GLI$z_FEV1_max,2),   Female=summarize_mean_sd(female_data_GLI$z_FEV1_max,2))
row_FEV1FVC_max_abs_GLI_a     <- tibble(Variable="FEV1/FVC (%)",                    Male=summarize_mean_sd(male_data_GLI$FEV1FVC_max,2),Female=summarize_mean_sd(female_data_GLI$FEV1FVC_max,2))
row_FEV1FVC_perc_max_GLI_a    <- tibble(Variable="FEV1/FVC % of predicted",          Male=summarize_mean_sd(male_data_GLI$FEV1FVC_perc_max,0),Female=summarize_mean_sd(female_data_GLI$FEV1FVC_perc_max,0))
row_zFEV1FVC_max_GLI_a        <- tibble(Variable="FEV1/FVC z-score",               Male=summarize_mean_sd(male_data_GLI$z_FEV1FVC_max,2),Female=summarize_mean_sd(female_data_GLI$z_FEV1FVC_max,2))

### PART 6. Combine all rows into final master table with all results
table_1_values_GLI_a <- bind_rows(
  row_N_GLI_a,
  row_FVC_max_abs_GLI_a, row_FVC_perc_max_GLI_a, row_zFVC_max_GLI_a,
  row_FEV1_max_abs_GLI_a, row_FEV1_perc_max_GLI_a, row_zFEV1_max_GLI_a,
  row_FEV1FVC_max_abs_GLI_a, row_FEV1FVC_perc_max_GLI_a, row_zFEV1FVC_max_GLI_a
)


### PART 7. Build flextable and export as Word document
# Varmista, että sarake Variable on character
table_1_values_GLI_a <- table_1_values_GLI_a %>%
  mutate(Variable = as.character(Variable))

ft_a <- flextable(table_1_values_GLI_a) %>%
  theme_vanilla() %>%
  border_remove() %>%
  hline_top(part = "header", border = fp_border(width = 1)) %>%
  hline(i = 1, part = "body", border = fp_border(width = 1)) %>%
  hline_bottom(border = fp_border(width = 1)) %>%
  set_table_properties(layout = "autofit", width = 1) %>%
  add_footer_lines(
    c(
      "* Presented as median and interquartile range (IQR)",
      "† Presented as mean and standard deviation (SD)"
    )
  ) %>%
  bold(i = 1:2, part = "footer") %>%
  italic(i = 1:2, part = "footer")

## Tallennetaan word dokumentti
# Luo tyhjä Word-dokumentti
doc <- read_docx()
# Lisää otsikko
doc <- body_add_par(doc, "Table 1: Spirometry Measures by Sex with GLI reference values", style = "heading 1")
# Lisää flextable taulukko dokumenttiin
doc <- body_add_flextable(doc, value = ft_a)
# Tallenna tiedosto
print(doc, target = file.path(data, "Table_1_spiros_GLI.docx"))



# Tehdään vielä kuvaaja
# 1. Määrittele ikäryhmät ja laske max-arvot
breaks <- c(20, 22, 24, 26, 28, 30, 32, 34)
labels <- c("20-21", "22-23", "24-25", "26-27", "28-29", "30-31", "32-33")

df_age_group_max_2_GLI <- df_GLI %>%
  mutate(age_group = cut(age, breaks = breaks, labels = labels, right = FALSE, include.lowest = TRUE)) %>%
  filter(!is.na(age_group)) %>%
  group_by(ID, age_group) %>%
  summarise(
    sex = first(sex), 
    weight = first(weight),
    FVC_z = max(FVC_z, na.rm = TRUE),
    FVC = max(FVC, na.rm = TRUE),
    FEV1_z = max(FEV1_z, na.rm = TRUE),
    FEV1 = max(FEV1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    FVC_z = ifelse(is.infinite(FVC_z), NA, FVC_z),
    FVC = ifelse(is.infinite(FVC), NA, FVC),
    FEV1_z = ifelse(is.infinite(FEV1_z), NA, FEV1_z),
    FEV1 = ifelse(is.infinite(FEV1), NA, FEV1)
  )

# 2. Summary-funktio, joka luo myös sex_label-faktorin legendaa varten

create_summary <- function(df_GLI, value_col) {
  if (value_col == "time_min") {
    df_GLI <- df_GLI %>%
      mutate(
        !!value_col := ifelse(sex == 2, .data[[value_col]] - 6, .data[[value_col]])
      )
  }
  
  summary_df_GLI <- df_GLI %>%
    group_by(age_group, sex) %>%
    summarise(
      mean_val = mean(.data[[value_col]], na.rm = TRUE),
      sd_val = sd(.data[[value_col]], na.rm = TRUE),
      n = sum(!is.na(.data[[value_col]])),
      lower = mean_val - 1.96 * sd_val / sqrt(n),
      upper = mean_val + 1.96 * sd_val / sqrt(n),
      .groups = "drop"
    ) %>%
    mutate(
      sex_label = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
      age_group = factor(age_group, levels = labels)
    )
  
  return(summary_df_GLI)
}


# 3. Kuvaajan piirtofunktio
plot_summary <- function(summary_df_GLI, mean_var, y_label, filename = NULL, show_n_labels = TRUE, y_breaks = NULL) {
  
  label_data <- summary_df_GLI %>%
    filter(sex_label %in% c("Male", "Female")) %>%
    select(age_group, sex_label, n) %>%
    pivot_wider(
      names_from = sex_label, 
      values_from = n,
      values_fill = list(n = 0)   # <— täyttää puuttuvat arvot nollalla
    ) %>%
    mutate(
      m_label = paste0("m=", Male),
      f_label = paste0("f=", Female)
    )
  
  age_levels <- levels(summary_df_GLI$age_group)
  
  p_main_GLI <- ggplot(summary_df_GLI, aes(x = age_group, y = .data[[mean_var]], group = sex_label, color = sex_label)) +
    geom_point(size = 3) +
    geom_line(linewidth = 1) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
    labs(
      title = paste("Mean", y_label, "progression by age group"),
      x = NULL,
      y = paste("Mean", y_label),
      color = "Group"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = margin(t = 10, r = 10, b = 0, l = 10)
    )
  
  if (!is.null(y_breaks)) {
    p_main_GLI <- p_main_GLI + scale_y_continuous(breaks = y_breaks)
  }
  
  if (show_n_labels) {
    label_long <- label_data %>%
      select(age_group, m_label, f_label) %>%
      pivot_longer(cols = c(m_label, f_label),
                   names_to = "group", values_to = "label") %>%
      mutate(
        group = factor(group, levels = c("m_label", "f_label")),
        age_group = factor(age_group, levels = age_levels),
        y = as.numeric(group)
      )
    
    p_labels_GLI <- ggplot(label_long, aes(x = age_group, y = y, label = label)) +
      geom_text(size = 2.5) +
      scale_y_continuous(
        breaks = 1:2,
        labels = c("m", "f"),
        expand = expansion(add = c(0.5, 0.5))
      ) +
      scale_x_discrete(position = "bottom") +
      theme_void() +
      theme(
        axis.title.x = element_text(margin = margin(t = 5)),
        axis.text.x = element_text(size = 12),
        axis.ticks.x = element_line(),
        plot.margin = margin(t = 0, r = 10, b = 10, l = 10)
      ) +
      labs(x = "Age group")
    
    p_final_GLI <- p_main_GLI / p_labels_GLI + patchwork::plot_layout(heights = c(4, 1))
  } else {
    p_final_GLI <- p_main_GLI
  }
  
  if (!is.null(filename)) {
    ggsave(filename, plot = p_final_GLI, width = 8, height = 6)
  }
  
  return(p_final_GLI)
}

# 4. Lasketaan summaryt
zFVC_summary_2_GLI <- create_summary(df_age_group_max_2_GLI, "FVC_z")
zFEV1_summary_2_GLI <- create_summary(df_age_group_max_2_GLI, "FEV1_z")
FEV1_summary_2_GLI <- create_summary(df_age_group_max_2_GLI, "FEV1")
FVC_summary_2_GLI <- create_summary(df_age_group_max_2_GLI, "FVC")

# 5. Piirretään kuvaajat
p_zFVC_2_GLI <- plot_summary(zFVC_summary_2_GLI, "mean_val", "FVC_z", file.path(kuvaajat, "z_FVC_ikäryhmittäin_GLI.png"))
p_zFEV1_2_GLI <- plot_summary(zFEV1_summary_2_GLI, "mean_val", "FEV1_z", file.path(kuvaajat, "z_FEV1_ikäryhmittäin_GLI.png"))
p_FEV1_2_GLI <- plot_summary(FEV1_summary_2_GLI, "mean_val", "FEV1 (l)", file.path(kuvaajat, "FEV1_ikäryhmittäin_GLI.png"))
p_FVC_2_GLI <- plot_summary(FVC_summary_2_GLI, "mean_val", "FVC (l)", file.path(kuvaajat, "FVC_ikäryhmittäin_GLI.png"))


#Luodaan vielä ruudukkokuvaaja:

library(patchwork)

# Luo yksittäiset kuvaajat ilman otsikkoja, ilman n-merkintöjä
p_A_GLI <- plot_summary(FVC_summary_2_GLI, "mean_val", "FVC (l)", filename = NULL, show_n_labels = FALSE) + ggtitle("A: FVC (l)") +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(), axis.ticks.x = element_line())

p_B_GLI <- plot_summary(FEV1_summary_2_GLI, "mean_val", "FEV1 (l)", filename = NULL, show_n_labels = FALSE) + ggtitle(expression("B: FEV"[1] * " (l)")) + ylab(expression("Mean " * FEV[1] * " (l)")) + 
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(), axis.ticks.x = element_line())

p_C_GLI <- plot_summary(zFVC_summary_2_GLI, "mean_val", "zFVC", filename = NULL, show_n_labels = FALSE) + ggtitle("C: FVC z-score") +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(), axis.ticks.x = element_line())

p_D_GLI <- plot_summary(zFEV1_summary_2_GLI, "mean_val", "zFEV1", filename = NULL, show_n_labels = FALSE) + ggtitle(expression("D: FEV"[1] * " z-score"))+ ylab(expression("Mean " * FEV[1] * " z-score")) +
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(), axis.ticks.x = element_line())

# 1. Luo yhteinen selite (legend)
legend_plot <- plot_summary(FVC_summary_2_GLI, "mean_val", "FVC (l)", filename = NULL) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  ) +
  guides(color = guide_legend(title = "Group"), linetype = "none") +
  theme_void()

# 2.Tee taulukko datasta
library(gridExtra)
library(tibble)
library(ggpubr)
library(dplyr)
library(tidyr)

## 2.1. Lasketaan n-määrät sukupuolen ja ikäryhmän mukaan
n_counts <- zFVC_summary_2_GLI %>%
  filter(sex_label %in% c("Male", "Female")) %>%
  group_by(age_group, sex_label) %>%
  summarise(n = first(n), .groups = "drop") %>%
  pivot_wider(names_from = age_group, values_from = n)

## 2.3 Yhdistetään kaikki ja järjestetään haluttuun järjestykseen
n_table_df_GLI <- bind_rows(n_counts) %>%
  mutate(sex_label = factor(sex_label, levels = c("Male", "Female"))) %>%
  arrange(sex_label) %>%
  replace_na(as.list(setNames(rep(0, ncol(n_counts) - 1), names(n_counts)[-1]))) %>%
  column_to_rownames("sex_label")

## 2.4. Luo ggtexttable taulukolla ja otsikolla
table_plot_GLI <- ggtexttable(
  n_table_df_GLI,
  rows = rownames(n_table_df_GLI),
  theme = ttheme("classic")
) %>%
  tab_add_title(text = "Participants by age group", face = "bold", size = 12)

# 3. Yhdistä kaikki: 2x2 kuvaajat, legenda ja n-taulukko
p_all_2x2_GLI <- (
  (p_A_GLI | p_B_GLI) / (p_C_GLI | p_D_GLI)
) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  )

# 4. Tallenna
final_figure_2x2_GLI <- wrap_elements(p_all_2x2_GLI) / wrap_elements(table_plot_GLI) +
  plot_layout(heights = c(10, 1))

ggsave(file.path(kuvaajat, "combined_2x2_with_clean_legend_GLI.png"),
       plot = final_figure_2x2_GLI,
       width = 12,
       height = 12)
