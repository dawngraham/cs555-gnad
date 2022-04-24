# CS555 Term Project - Dawn Graham

library(stringr)
library(dplyr)
library(tidyr)

# Session > Set Working Directory > To Source File Location 
campaigns <- read.csv("data/campaigns.csv")
attach(campaigns)

campaigns[campaigns==""] <- "none"

# country
campaigns$country_usa <- as.integer(str_detect(country, "United States"))
campaigns$country_canada <- as.integer(str_detect(country, "Canada"))
campaigns$country_mexico <- as.integer(str_detect(country, "Mexico"))
campaigns$country_total <- str_count(country, ",") + 1

# pcs
campaigns$pcs_indigenous <- as.integer(str_detect(pcs, "Indigenous Participants"))
campaigns$pcs_poc <- as.integer(str_detect(pcs, "People of Color"))
campaigns$pcs_students <- as.integer(str_detect(pcs, "Student Participants"))
campaigns$pcs_women <- as.integer(str_detect(pcs, "Women"))
campaigns$pcs_repression <- as.integer(str_detect(pcs, "An Example of Paradox of Repression"))
campaigns$pcs_regime <- as.integer(str_detect(pcs, "An Example of Paradox of Regime Change"))
campaigns$pcs_innovative <- as.integer(str_detect(pcs, "Included Innovative Organizational Forms/Communication Forms"))
campaigns$pcs_class <- as.integer(str_detect(pcs, "Included Participation by More Than One Social Class"))
campaigns$pcs_local <- as.integer(str_detect(pcs, "Local Community or Neighborhood-level Campaign"))
campaigns$pcs_total <- ifelse(campaigns$pcs == "none", 0, str_count(pcs, ",") + 1)
# Provides same result as above
# campaigns$pcs_total <- rowSums(campaigns[, grepl( "pcs_" , names( campaigns ) )])

# Check above
x <- campaigns[ , grepl("pcs", names(campaigns))]

# opp_nvresponses
query <- "no nonviolent responses|not know|none|not applicable|no known|unknown|no opponent nonviolent"
campaigns$opp_nv <- as.integer(str_detect(tolower(campaigns$opp_nvresponses), query, negate=TRUE))
campaigns$opp_nv[317] <- 1

x <- campaigns[, c("opp_nv", "opp_nvresponses")]

# methods
campaigns$methods_1 <- str_replace(campaigns$methods_1, "Other...", "200. Other...")
campaigns$methods_2 <- str_replace(campaigns$methods_2, "Other...", "200. Other...")
campaigns$methods_3 <- str_replace(campaigns$methods_3, "Other...", "200. Other...")
campaigns$methods_4 <- str_replace(campaigns$methods_4, "Other...", "200. Other...")
campaigns$methods_5 <- str_replace(campaigns$methods_5, "Other...", "200. Other...")
campaigns$methods_6 <- str_replace(campaigns$methods_6, "Other...", "200. Other...")
campaigns$methods_addl <- str_replace(campaigns$methods_addl, "Other...", "200. Other...")

campaigns$methods_total <- str_extract_all(paste(methods_1, methods_2, methods_3, methods_4, methods_5, methods_6, methods_addl), ("\\d{3}"))

for (i in 1:nrow(campaigns)) {
  campaigns$methods_unique[i] <- list(as.numeric(unique(unlist(campaigns$methods_total[i]))))
  campaigns$methods_unique_ct[i] <- n_distinct(unlist(campaigns$methods_unique[i]))
  
  # https://nvdatabase.swarthmore.edu/browse-methods
  campaigns$methods_nvprotest[i] <- sum(between(unlist(campaigns$methods_unique[i]), 1, 54))
  campaigns$methods_social[i] <- sum(between(unlist(campaigns$methods_unique[i]), 55, 70))
  campaigns$methods_boycotts[i] <- sum(between(unlist(campaigns$methods_unique[i]), 71, 96))
  campaigns$methods_strikes[i] <- sum(between(unlist(campaigns$methods_unique[i]), 97, 119))
  campaigns$methods_political[i] <- sum(between(unlist(campaigns$methods_unique[i]), 120, 157))
  campaigns$methods_nvintervention[i] <- sum(between(unlist(campaigns$methods_unique[i]), 158, 198))
  campaigns$methods_other[i] <- sum(between(unlist(campaigns$methods_unique[i]), 199, 200))
}

campaigns$methods_1_ct <- str_count(methods_1, ("\\d{3}"))
campaigns$methods_2_ct <- str_count(methods_2, ("\\d{3}"))
campaigns$methods_3_ct <- str_count(methods_3, ("\\d{3}"))
campaigns$methods_4_ct <- str_count(methods_4, ("\\d{3}"))
campaigns$methods_5_ct <- str_count(methods_5, ("\\d{3}"))
campaigns$methods_6_ct <- str_count(methods_6, ("\\d{3}"))
campaigns$methods_addl_ct <- str_count(methods_addl, ("\\d{3}"))

x <- campaigns[ , grepl("methods", names(campaigns))]

# segment_length
campaigns$segment_hours <- replace_na(as.numeric(str_extract(tolower(segment_length), "\\d+\\.*\\d*[^h]+")), 0)
campaigns$segment_days <- replace_na(as.numeric(str_extract(tolower(segment_length), "\\d+\\.*\\d*[^d]+")), 0)
campaigns$segment_weeks <- replace_na(as.numeric(str_extract(tolower(segment_length), "\\d+\\.*\\d*[^w]+")), 0)
campaigns$segment_months <- replace_na(as.numeric(str_extract(tolower(segment_length), "\\d+\\.*\\d*[^m]+")), 0)
campaigns$segment_years <- replace_na(as.numeric(str_extract(tolower(segment_length), "\\d+\\.*\\d*[^y]+")), 0)
campaigns$segment_length_days <- campaigns$segment_hours/24 + campaigns$segment_days + campaigns$segment_weeks*7 + campaigns$segment_months*30.42 + campaigns$segment_years*365

# This is not perfect, but gives a decent estimate for most
x <- campaigns[ , grepl("segment", names(campaigns))]

# partners
query <- "not know|none"
campaigns$had_partners <- as.integer(str_detect(tolower(campaigns$partners), query, negate=TRUE))

# allies
query <- "not know|none|unknown"
campaigns$had_allies <- as.integer(str_detect(tolower(campaigns$allies), query, negate=TRUE))

# social_elites
query <- "not know|none|unknown"
campaigns$had_social_elites <- as.integer(str_detect(tolower(campaigns$social_elites), query, negate=TRUE))

# campaigner_violence
query <- "not know|none|unknown|no known|no campaigner violence|no violence"
campaigns$had_campaigner_violence <- as.integer(str_detect(tolower(campaigns$campaigner_violence), query, negate=TRUE))

# repressive_violence
query <- "not know|none|no known|no violence|no repressive violence"
campaigns$had_repressive_violence <- as.integer(str_detect(tolower(campaigns$repressive_violence), query, negate=TRUE))

# cluster
campaigns$cluster_democracy <- as.integer(str_detect(cluster, "Democracy"))
campaigns$cluster_economic <- as.integer(str_detect(cluster, "Economic Justice"))
campaigns$cluster_environment <- as.integer(str_detect(cluster, "Environment"))
campaigns$cluster_humanrights <- as.integer(str_detect(cluster, "Human Rights"))
campaigns$cluster_ethnicidentity <- as.integer(str_detect(cluster, "National-Ethnic Identity"))
campaigns$cluster_peace <- as.integer(str_detect(cluster, "Peace"))
campaigns$cluster_total <- ifelse(campaigns$cluster == "none", 0, str_count(cluster, ",") + 1)

x <- campaigns[ , grepl("cluster", names(campaigns))]

# classification
campaigns$class_change <- as.integer(str_detect(classification, "Change"))
campaigns$class_defense <- as.integer(str_detect(classification, "Defense"))
campaigns$class_intervention <- as.integer(str_detect(classification, "Third-party nonviolent intervention"))
campaigns$class_total <- ifelse(campaigns$classification == "none", 0, str_count(classification, ",") + 1)

x <- campaigns[ , grepl("class", names(campaigns))]

# group characterization
campaigns$groups <- ifelse(campaigns$group == "none", 0, str_count(group, ",") + 1)

# success
campaigns$success_goal <- as.numeric(str_extract(success_goal, "[^ out of ]+"))
campaigns$success_survival <- as.numeric(str_extract(success_survival, "[^ out of ]+"))
campaigns$success_growth <- as.numeric(str_extract(success_growth, "[^ out of ]+"))
campaigns$success_total <- as.numeric(str_extract(success_total, "[^ out of ]+"))
campaigns$success_above_avg <- ifelse(campaigns$success_total >= 7, 1, 0)

# Keep only desired columns
colnames(campaigns)
colnames(campaigns[c(40:54, 57:71, 77:94, 34:37, 95)])
campaigns <- campaigns[c(40:54, 57:71, 77:94, 34:37, 95)]

# Save cleaned csv
write.csv(campaigns, "data/campaigns_clean.csv", row.names=FALSE)
