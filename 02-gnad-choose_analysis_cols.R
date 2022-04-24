# CS555 Term Project - Dawn Graham

# Session > Set Working Directory > To Source File Location 
campaigns <- read.csv("data/campaigns_clean.csv")
attach(campaigns)

# Keep only desired columns for modeling
campaigns <- campaigns[c('country_total', 'pcs_local', 'pcs_total', 'methods_unique_ct', 'segment_length_days', 'had_social_elites', 'had_campaigner_violence', 'had_repressive_violence', 'cluster_total', 'class_total', 'success_goal', 'success_survival', 'success_growth', 'success_total', 'success_above_avg')]

# Save cleaned csv
write.csv(campaigns, "data/campaigns_clean_subset.csv", row.names=FALSE)