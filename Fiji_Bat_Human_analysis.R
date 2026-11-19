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

## ---- Combine the recorded options for descriptive plots ----
option_labels <- function(df, labels, other = NULL) {
  vapply(seq_len(nrow(df)), function(i) {
    flags <- unlist(df[i, names(labels)], use.names = FALSE)
    if (!all(is.na(flags) | flags %in% c(0, 1))) {
      stop("Expected 0/1 option columns. For the original CSV, see the README.")
    }
    answers <- unname(labels[which(flags == 1)])
    if (!is.null(other) && !is.na(df[[other]][i])) {
      answers <- c(answers, df[[other]][i])
    }
    if (length(answers)) paste(answers, collapse = " / ") else NA_character_
  }, character(1))
}

sick_labels <- c(Leave_sick = "Leave", Move_sick = "Move", Care_sick = "Care",
                 Kill_sick = "Kill", Give_sick = "Give", Eat_sick = "Eat")
if (all(c(names(sick_labels), "Other_sick") %in% names(data))) {
  data$sick <- option_labels(data, sick_labels, other = "Other_sick")
  ggplot(data, aes(sick)) + geom_bar()
}

trend_labels <- c(Bats_increasing = "Increasing", Bats_decreasing = "Decreasing",
                  Bats_same = "Same")
if (all(names(trend_labels) %in% names(data))) {
  data$trend <- option_labels(data, trend_labels)
  ggplot(data, aes(trend, col = trend, fill = trend)) +
    geom_bar(width = 0.4) +
    theme(legend.position = "none", text = element_text(size = 18)) +
    ylab("Count") + xlab("") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
  ggsave("trend.png", width = 20, height = 10, units = "cm")
}

## ---- Age plot ----
if ("Age" %in% names(data)) {
  ggplot(data, aes(Age)) +
    geom_bar(width = 0.4) +
    theme(legend.position = "none", text = element_text(size = 16)) +
    ylab("Count") + xlab("Age")
  ggsave("age_grey.png", width = 20, height = 10, units = "cm")
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
  
  data_catch$value <- sub("^No -.*", "No", data_catch$value)
  data_catch$value <- sub("^Will go -.*", "Will go", data_catch$value)

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
  
  data_guano$value <- sub("^No -.*", "No", data_guano$value)
  data_guano$value[data_guano$value %in% "a"] <- "No"

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

## ---- Publication figures ----
# Reload the CSV in a separate environment to retain "Will eat" for plotting.
source("Fiji_Bat_Human_RSOS_figures.R", local = new.env())

## ---- done ----
message("Finished running Fiji bat–human analysis script (with harm01 in regressions).")

