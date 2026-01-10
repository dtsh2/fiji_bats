## =========================================================

## ---- libraries ----
library(RColorBrewer)
library(tidyverse)
library(ggplot2)
library(skimr)
library(tidyr)
library(visreg)
library(reshape2)

## ---- read data ----
data <- read.csv(
  "Fiji_Bat_Human_data.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "latin1",
  header = TRUE,
  na.strings = c("", "NA")
)

## ---- quick checks ----
summary(data)
str(data)
skimr::skim(data)

## ---- helper: robust Yes/No -> 0/1 ----
to01 <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  xl <- stringr::str_to_lower(x)
  
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    
    # YES-like
    x %in% c("Yes", "Y", "1") ~ 1,
    stringr::str_detect(xl, "yes|will|do|currently|often|sometimes") ~ 1,
    
    # NO-like
    x %in% c("No", "N", "0") ~ 0,
    stringr::str_detect(xl, "no|never|not") ~ 0,
    
    TRUE ~ NA_real_
  )
}

## ---- tidy / recode key variables ----
data <- data %>%
  mutate(
    ## Age: fix one level
    Age = dplyr::recode(Age, "25-30" = "25-29"),
    Age = factor(Age),
    
    ## Gender: recode M/F (if present) and set order
    Gender = dplyr::recode(Gender, "M" = "Male", "F" = "Female"),
    Gender = factor(Gender, levels = c("Female", "Male")),
    
    ## Harm exposure (0/1)
    harm01 = if ("Bat_harm_humans" %in% names(.)) to01(Bat_harm_humans) else NA_real_
  )

## ---- custom ggplot discrete palette helper ----
gg_color_hue <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

## =========================================================
## Descriptives / plots
## =========================================================

## ---- Religion plots ----
if ("Religion" %in% names(data)) {
  n_rel <- length(levels(factor(data$Religion)))
  cols_rel <- gg_color_hue(n_rel)
  
  ggplot(data, aes(Religion)) +
    geom_bar() +
    ylab("Count") + xlab("") +
    theme(legend.position = "none") +
    scale_x_discrete(guide = guide_axis(n.dodge = 3))
  ggsave("religion_all.png", width = 20, height = 10, units = "cm")
  
  data <- data %>%
    mutate(
      Religion = dplyr::recode(
        Religion,
        "Anglican" = "Other Christian",
        "Pentecostal" = "Other Christian",
        "Seventh-day Adventist" = "Other Christian"
      )
    )
  
  n <- 4
  cols <- gg_color_hue(n)
  cols <- c(
    "Other Christian" = cols[1],
    "Catholic"        = cols[1],
    "Christian"       = cols[1],
    "Methodist"       = cols[1],
    "Hindu"           = cols[2],
    "No religion"     = cols[3]
  )
  
  ggplot(data, aes(Religion, fill = Religion)) +
    geom_bar() +
    ylab("Count") + xlab("") +
    scale_fill_manual(values = cols) +
    theme(legend.position = "none", text = element_text(size = 16)) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
  ggsave("religion.png", width = 20, height = 10, units = "cm")
}

## ---- Plot all columns (base plot) ----
# for (i in 1:ncol(data)) plot(data[, i])

## ---- Make combined sick column ----
sick_cols <- intersect(
  c("Leave_sick", "Move_sick", "Care_sick", "Kill_sick", "Give_sick", "Eat_sick"),
  names(data)
)

if (length(sick_cols) > 0 && !"sick" %in% names(data)) {
  data <- data %>%
    tidyr::unite("sick", dplyr::all_of(sick_cols), sep = ",", remove = FALSE) %>%
    mutate(
      sick = stringr::str_replace_all(sick, "NA", ""),
      sick = stringr::str_replace_all(sick, ",", ""),
      sick = stringr::str_squish(sick)
    )
  
  ggplot(data, aes(sick)) + geom_bar()
}

## ---- Make combined population trend column ----
trend_cols <- intersect(c("Bats_increasing", "Bats_decreasing", "Bats_same"), names(data))

if (length(trend_cols) > 0 && !"trend" %in% names(data)) {
  data <- data %>%
    tidyr::unite("trend", dplyr::all_of(trend_cols), sep = ",", remove = FALSE) %>%
    mutate(
      trend = stringr::str_replace_all(trend, "NA", ""),
      trend = stringr::str_replace_all(trend, ",", ""),
      trend = stringr::str_squish(trend)
    )
  
  ggplot(data, aes(trend)) + geom_bar()
  
  data <- data %>%
    mutate(
      trend = str_replace(trend, "c", "None"),
      trend = str_replace(trend, "a", "Increasing"),
      trend = str_replace(trend, "b", "Decreasing"),
      trend = str_replace(trend, "IncreasingDecreasing", "Inc/Dec"),
      trend = str_replace(trend, "^$", "NA")
    )
  
  ggplot(data, aes(trend, col = trend, fill = trend)) +
    geom_bar(width = 0.4) +
    theme(legend.position = "none", text = element_text(size = 18)) +
    ylab("Count") + xlab("") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
  ggsave("trend.png", width = 20, height = 10, units = "cm")
}

## ---- Bat eating by age/gender ----
if (all(c("Eat_bat", "Past_eat_bat", "Age", "Gender") %in% names(data))) {
  df2 <- data %>%
    group_by(Eat_bat, Past_eat_bat, Age, Gender) %>%
    tally(name = "n")
  
  to_plot2 <- df2 %>%
    pivot_longer(cols = c(Eat_bat, Past_eat_bat), names_to = "Eat", values_to = "Response")
  
  to_plot2$Eat <- factor(to_plot2$Eat, levels = c("Past_eat_bat", "Eat_bat"), labels = c("Past", "Current"))
  
  ggplot(data = subset(to_plot2, !is.na(Age)), aes(x = Age, y = n, fill = Response)) +
    geom_bar(position = "stack", stat = "identity") +
    theme_bw() + facet_wrap(~Eat) +
    theme(text = element_text(size = 16), legend.title = element_blank()) +
    ylab("Count")
  ggsave("eating_age_current.png", width = 20, height = 10, units = "cm")
  
  ggplot(data = subset(to_plot2, !is.na(Age)), aes(x = Age, y = n, fill = Response)) +
    geom_bar(position = "stack", stat = "identity") +
    theme_bw() + facet_wrap(~Gender) +
    theme(text = element_text(size = 16), legend.title = element_blank()) +
    ylab("Count")
  ggsave("eating_gender_current.png", width = 20, height = 10, units = "cm")
  
  ggplot(data = subset(to_plot2, !is.na(Age)), aes(x = Age, y = n, fill = Response)) +
    geom_bar(position = "stack", stat = "identity", width = 0.6) +
    theme_bw() + facet_wrap(~Gender + Eat) +
    theme(text = element_text(size = 16), legend.title = element_blank()) +
    ylab("Count")
  ggsave("eating.png", width = 20, height = 15, units = "cm")
}

## ---- Age plot ----
if ("Age" %in% names(data)) {
  ggplot(data, aes(Age)) +
    geom_bar(width = 0.4) +
    theme(legend.position = "none", text = element_text(size = 16)) +
    ylab("Count") + xlab("Age")
  ggsave("age_grey.png", width = 20, height = 10, units = "cm")
}

## ---- Contact data by age and gender ----
if (all(c("Contact", "Age", "Gender") %in% names(data))) {
  df3 <- data %>%
    group_by(Contact, Age, Gender) %>%
    tally(name = "n")
  
  ggplot(data = subset(df3, !is.na(Age) & !is.na(Gender)), aes(x = Age, y = n, fill = Contact)) +
    geom_bar(position = "stack", stat = "identity") +
    theme_bw() + facet_wrap(~Gender) +
    ylab("Count") +
    theme(text = element_text(size = 16), legend.title = element_blank())
  ggsave("contacts_gender.png", width = 20, height = 10, units = "cm")
}

## ---- Education plots ----
if ("Education_yrs" %in% names(data)) {
  data <- data %>%
    mutate(
      Education_yrs = factor(Education_yrs, levels = c("3","4","5","6","7","8","9","10","11","12","12+","13","13+")),
      Education_yrs = ordered(Education_yrs, levels = c("3","4","5","6","7","8","9","10","11","12","12+","13","13+"))
    )
  
  ggplot(data, aes(Education_yrs)) +
    geom_bar() +
    ylab("Count") + xlab("Education in years")
  ggsave("education.png", width = 20, height = 10, units = "cm")
  
  ggplot(data, aes(Gender, fill = Gender)) +
    geom_bar() +
    ylab("Count") + xlab("") +
    theme(legend.position = "none")
  ggsave("gender.png", width = 8, height = 8, units = "cm")
  
  # Education by age & gender
  df4 <- data %>%
    group_by(Education_yrs, Age, Gender) %>%
    tally(name = "n")
  
  colourCount <- length(unique(data$Education_yrs))
  getPalette <- colorRampPalette(brewer.pal(9, "RdYlGn"))
  
  ggplot(data = subset(df4, !is.na(Age) & !is.na(Gender))) +
    geom_bar(aes(x = Age, y = n, fill = Education_yrs), stat = "identity") +
    theme_bw() + facet_wrap(~Gender) +
    ylab("Count") +
    guides(fill = guide_legend(ncol = 2, title = "Years")) +
    scale_fill_manual(values = getPalette(colourCount), na.value = "grey")
  ggsave("edu_gender_raw.png", width = 20, height = 10, units = "cm")
  
  ggplot(data = subset(df4, !is.na(Age) & !is.na(Gender))) +
    geom_bar(aes(x = Age, y = n, fill = Education_yrs), stat = "identity", position = "fill") +
    theme_bw() + facet_wrap(~Gender) +
    ylab("Proportion") +
    guides(fill = guide_legend(ncol = 2, title = "Years"))
  ggsave("edu_gender.png", width = 20, height = 10, units = "cm")
}

## ---- Prepare bats plot ----
if ("Prepare_eat" %in% names(data)) {
  ggplot(data, aes(Prepare_eat, fill = Prepare_eat)) +
    geom_bar() +
    ylab("Count") + xlab("") +
    theme(legend.position = "none")
  ggsave("Prepare_bats.png", width = 8, height = 8, units = "cm")
}

## ---- Bat consumption (past/current) ----
if (all(c("Person.ID.number", "Past_eat_bat", "Eat_bat") %in% names(data))) {
  data_eat <- reshape2::melt(data, id.vars = "Person.ID.number", measure.vars = c("Past_eat_bat", "Eat_bat"))
  levels(data_eat$variable) <- c("Past", "Current")
  
  ggplot(data_eat, aes(x = variable, fill = value)) +
    geom_bar() +
    ylab("Count") + xlab("")
  ggsave("eat_bats.png", width = 10, height = 10, units = "cm")
}

## ---- Catching bats (past/current) ----
if (all(c("Person.ID.number", "Past_catch_bats", "Catch_bats") %in% names(data))) {
  data_catch <- reshape2::melt(data, id.vars = "Person.ID.number", measure.vars = c("Past_catch_bats", "Catch_bats"), na.rm = TRUE)
  
  data_catch$value <- sub("No .*", "", data_catch$value)
  data_catch$value <- sub("Will go -.*", "", data_catch$value)
  data_catch[data_catch == ""] <- NA
  
  levels(data_catch$variable) <- c("Past", "Current")
  colnames(data_catch)[3] <- "Response"
  
  ggplot(data_catch, aes(x = variable, fill = Response)) +
    geom_bar(width = 0.4) +
    theme(legend.title = element_blank()) +
    ylab("Count") + xlab("")
  ggsave("catch_bats.png", width = 10, height = 10, units = "cm")
}

## ---- Collecting guano (past/current) ----
if (all(c("Person.ID.number", "Past_mine_guano", "Mine_guano") %in% names(data))) {
  data_guano <- reshape2::melt(data, id.vars = "Person.ID.number", measure.vars = c("Past_mine_guano", "Mine_guano"))
  levels(data_guano$variable) <- c("Past_mine_guano", "Mine_guano")
  
  data_guano$value <- sub("No .*", "", data_guano$value)
  data_guano[data_guano == ""] <- NA
  data_guano[data_guano == "- "] <- NA
  data_guano[data_guano == "a"] <- "No"
  
  levels(data_guano$variable) <- c("Past", "Current")
  colnames(data_guano)[3] <- "Response"
  
  ggplot(data_guano, aes(x = variable, fill = Response)) +
    geom_bar(width = 0.5) +
    ylab("Count") + xlab("") +
    theme(legend.title = element_blank())
  ggsave("collect_guano.png", width = 10, height = 10, units = "cm")
}

## =========================================================
## Logistic regression models INCLUDING harm01
## (Outcomes converted to 0/1 using to01 for robustness)
## =========================================================

## ---- Sanity check harm coding ----
if ("Bat_harm_humans" %in% names(data)) {
  print(data %>% count(Bat_harm_humans, harm01))
}
print(data %>% summarise(n = n(), n_harm = sum(!is.na(harm01))))

## ---- Eat bats (current) ----
if ("Eat_bat" %in% names(data)) {
  data <- data %>%
    mutate(
      Eat_bat = as.character(Eat_bat),
      Eat_bat = str_replace(Eat_bat, "Will eat", "Yes"),
      Eat01   = to01(Eat_bat)
    )
  
  logit_eat_harm <- glm(Eat01 ~ harm01 + Age + Gender, data = data, family = binomial, na.action = na.omit)
  logit_eat_harm_i <- glm(Eat01 ~ harm01 + Age * Gender, data = data, family = binomial, na.action = na.omit)
  
  print(summary(logit_eat_harm))
  print(summary(logit_eat_harm_i))
  print(AIC(logit_eat_harm, logit_eat_harm_i))
  print(anova(logit_eat_harm, logit_eat_harm_i, test = "Chisq"))
}

## ---- Past bat eating (past) ----
if ("Past_eat_bat" %in% names(data)) {
  data <- data %>%
    mutate(
      Past_eat_bat = as.character(Past_eat_bat),
      PastEat01    = to01(Past_eat_bat)
    )
  
  logit_past_eat_harm   <- glm(PastEat01 ~ harm01 + Age + Gender,
                               data = data, family = binomial, na.action = na.omit)
  
  logit_past_eat_harm_i <- glm(PastEat01 ~ harm01 + Age * Gender,
                               data = data, family = binomial, na.action = na.omit)
  
  print(summary(logit_past_eat_harm))
  print(summary(logit_past_eat_harm_i))
  print(AIC(logit_past_eat_harm, logit_past_eat_harm_i))
  print(anova(logit_past_eat_harm, logit_past_eat_harm_i, test = "Chisq"))
}

## ---- Contact ----
if ("Contact" %in% names(data)) {
  data <- data %>% mutate(Contact01 = to01(Contact))
  
  logit_contact_harm <- glm(Contact01 ~ harm01 + Age + Gender, data = data, family = binomial, na.action = na.omit)
  logit_contact_harm_i <- glm(Contact01 ~ harm01 + Age * Gender, data = data, family = binomial, na.action = na.omit)
  
  print(summary(logit_contact_harm))
  print(summary(logit_contact_harm_i))
  print(AIC(logit_contact_harm, logit_contact_harm_i))
  print(anova(logit_contact_harm, logit_contact_harm_i, test = "Chisq"))
}

## ---- Prepare bats ----
if ("Prepare_eat" %in% names(data)) {
  data <- data %>% mutate(Prepare01 = to01(Prepare_eat))
  
  logit_prepare_harm <- glm(Prepare01 ~ harm01 + Age + Gender, data = data, family = binomial, na.action = na.omit)
  logit_prepare_harm_i <- glm(Prepare01 ~ harm01 + Age * Gender, data = data, family = binomial, na.action = na.omit)
  
  print(summary(logit_prepare_harm))
  print(summary(logit_prepare_harm_i))
  print(AIC(logit_prepare_harm, logit_prepare_harm_i))
  print(anova(logit_prepare_harm, logit_prepare_harm_i, test = "Chisq"))
}

## =========================================================
##  export model summaries
## =========================================================
broom::tidy(logit_contact_harm, conf.int = TRUE, exponentiate = TRUE) %>% write.csv("logit_contact_harm_OR.csv", row.names = FALSE)

## ---- done ----
message("Finished running Fiji bat–human analysis script (with harm01 in regressions).")

