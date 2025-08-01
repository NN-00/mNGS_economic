###cost effectiveness analysis (time)
df <- read_excel("250730-Final-dataset-mNGS_economic.xlsx")
mNGS_time <- df %>%
  filter(label==0)
mNGS_time <- mNGS_time %>%
  mutate(mNGS_to_hospitalization = as.numeric(mNGS_to_hospitalization))
mNGS_time_long <- mNGS_time %>%
  mutate(
    group_3 = ifelse(mNGS_to_hospitalization <= 3, "≤ 3 Days", "> 3 Days"),
    group_4 = ifelse(mNGS_to_hospitalization <= 4, "≤ 4 Days", "> 4 Days"),
    group_5 = ifelse(mNGS_to_hospitalization <= 5, "≤ 5 Days", "> 5 Days"),
    group_6 = ifelse(mNGS_to_hospitalization <= 6, "≤ 6 Days", "> 6 Days"),
    group_7 = ifelse(mNGS_to_hospitalization <= 7, "≤ 7 Days", "> 7 Days")
  ) %>%
  pivot_longer(cols = starts_with("group_"), 
               names_to = "group_type", 
               values_to = "group") %>%
  filter(!group_type %in% c("group_type", "group_label")) 

mNGS_time_long <- distinct(mNGS_time_long)

mNGS_time_long <- mNGS_time_long %>%
  select(-antibiotic)

lower_bound_cost2 <- quantile(mNGS_time$cost, 0.05)
upper_bound_cost2 <- quantile(mNGS_time$cost, 0.95)
mNGS_time_cleaned_cost <- mNGS_time %>%
  filter(cost >= lower_bound_cost2 & cost <= upper_bound_cost2)

mNGS_time_cleaned_cost_long <- mNGS_time_cleaned_cost %>%
  mutate(
    group_3 = ifelse(mNGS_to_hospitalization <= 3, "≤ 3 Days", "> 3 Days"),
    group_4 = ifelse(mNGS_to_hospitalization <= 4, "≤ 4 Days", "> 4 Days"),
    group_5 = ifelse(mNGS_to_hospitalization <= 5, "≤ 5 Days", "> 5 Days"),
    group_6 = ifelse(mNGS_to_hospitalization <= 6, "≤ 6 Days", "> 6 Days"),
    group_7 = ifelse(mNGS_to_hospitalization <= 7, "≤ 7 Days", "> 7 Days")
  ) %>%
  pivot_longer(cols = starts_with("group_"), 
               names_to = "group_type", 
               values_to = "group") %>%
  filter(!group_type %in% c("group_type", "group_label")) 

mNGS_time_cleaned_cost_long <- distinct(mNGS_time_cleaned_cost_long)
##Fig.5
library(gridExtra)
library(ggsignif) 
Fig_5_A <- ggplot(mNGS_time_long, aes(x = group, y = hospitalization_length_new, fill = group)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 1, width = 0.3) +  
  scale_fill_manual(values = c("≤ 3 Days" = "skyblue", "> 3 Days" = "#E64B35B2",
                               "≤ 4 Days" = "skyblue", "> 4 Days" = "#E64B35B2",
                               "≤ 5 Days" = "skyblue", "> 5 Days" = "#E64B35B2",
                               "≤ 6 Days" = "skyblue", "> 6 Days" = "#E64B35B2",
                               "≤ 7 Days" = "skyblue", "> 7 Days" = "#E64B35B2")) +
  labs(title = "Hospital Length of Stay",
       x = "", y = "Hospital Length of Stay (days)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) +  
  facet_grid(~ group_type, scales = "free_x", space = "free_x") 
Fig_5_B <- ggplot(mNGS_time_cleaned_cost_long, aes(x = group, y = cost, fill = group)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 1, width = 0.3) +  
  scale_fill_manual(values = c("≤ 3 Days" = "skyblue", "> 3 Days" = "#E64B35B2",
                               "≤ 4 Days" = "skyblue", "> 4 Days" = "#E64B35B2",
                               "≤ 5 Days" = "skyblue", "> 5 Days" = "#E64B35B2",
                               "≤ 6 Days" = "skyblue", "> 6 Days" = "#E64B35B2",
                               "≤ 7 Days" = "skyblue", "> 7 Days" = "#E64B35B2"))+
  labs(title = "Hospitalization Cost (CNY)",
       x = "", y = "Hospitalization Cost (CNY)") +
  scale_y_continuous(breaks = seq(0, max(Fig_3_B$cost), by = 10000)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) +  
  facet_grid(~ group_type, scales = "free_x", space = "free_x")  

Fig_5_plot  <- Fig_5_A + Fig_5_B + plot_layout(ncol = 1, widths = c(1, 1))
cairo_pdf("Fig.5-2.pdf", width = 12, height = 8)
print(Fig_5_plot)
dev.off()

