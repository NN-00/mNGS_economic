###cost-effectiveness analysis###
library(tidyverse)
library(ggpubr)
library(readxl)
library(rstatix)
library(ggplot2)
library(patchwork)

df <- read_excel("250730-Final-dataset-mNGS_economic.xlsx")
df$admission_date_new <- as.Date(df$admission_date, format = "%Y-%m-%d")
df$discharge_date_new <- as.Date(df$discharge_date, format = "%Y-%m-%d")
df$hospitalization_length_new <- as.numeric(df$discharge_date_new - df$admission_date_new +1)
upper_bound_cost <- quantile(df$cost, 0.90, na.rm = TRUE)
lower_bound_cost <- quantile(df$cost,0.05, na.rm=TRUE )
upper_bound_los <- quantile(df$hospitalization_length_new, 0.95, na.rm = TRUE)
lower_bound_los <- quantile(df$hospitalization_length_new, 0.05, na.rm=TRUE)


Fig_3_A <- df %>%
  filter(hospitalization_length_new >= lower_bound_los & hospitalization_length_new <= upper_bound_los) %>%
  mutate(
    age_group = cut(age, 
                    breaks = c(54, 59, 64, 69),
                    labels = c("55-59", "60-64", "65-69"),
                    right = TRUE,
                    include.lowest = TRUE),
    infection_level = factor(infection_type_all, 
                             levels = c("1", "2", "3"),
                             labels = c("Not Detected Pathogens", "Common", "Uncommon or Rare")),
    method = factor(label, 
                    levels = c("0", "1"),
                    labels = c("mNGS(+)", "Culture(+)")))
Fig_3_B <- df %>%
  filter(cost <= upper_bound_cost) %>%
  mutate(
    age_group = cut(age, 
                    breaks = c(54, 59, 64, 69),
                    labels = c("55-59", "60-64", "65-69"),
                    right = TRUE,
                    include.lowest = TRUE),
    infection_level = factor(infection_type_all, 
                             levels = c("1", "2", "3"),
                             labels = c("Not Detected Pathogens", "Common", "Uncommon or Rare")),
    method = factor(label, 
                    levels = c("0", "1"),
                    labels = c("mNGS(+)", "Culture(+)")))

##Fig.3 A
Fig_3_A_sex <- ggplot(Fig_3_A, aes(x = sex, y = hospitalization_length_new, fill = method)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("mNGS(+)" = "#66C2B5", "Culture(+)" = "#D4A9E3")) +
  labs(x = "Sex", y = "Hospital Length of Stay") +
  scale_y_continuous(breaks = seq(0, max(Fig_3_A$hospitalization_length), by = 5)) +
  scale_x_discrete(labels = c("male" = "male", "female" = "female")) +
  theme_minimal(base_family = "Arial") +
  theme(
    axis.text.x = element_text(hjust = 0.5),
    legend.position = "none",
    axis.title.y = element_text(size = 12, face = "bold"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank()
  )

Fig_3_A_age <- ggplot(Fig_3_A, aes(x = age_group, y = hospitalization_length_new, fill = method)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("mNGS(+)" = "#66C2B5", "Culture(+)" = "#D4A9E3")) +
  labs(x = "Age Group", y = NULL) +
  theme_minimal(base_family = "Arial") +
  theme(
    axis.text.x = element_text(hjust = 0.5),
    legend.position = "none",
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank()
  )

Fig_3_A_infection <- ggplot(Fig_3_A, aes(x = infection_level, y = hospitalization_length_new, fill = method)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("mNGS(+)" = "#66C2B5", "Culture(+)" = "#D4A9E3"),
                    labels = c("mNGS", "Culture")) +
  labs(x = "Infection Type", y = NULL) +
  scale_x_discrete(labels = c("Not Detected Pathogens", "Common", "Uncommon or Rare")) +
  theme_minimal(base_family = "Arial") +
  theme(
    axis.text.x = element_text(hjust = 0.5),
    legend.position = "right",
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    legend.title = element_blank()
  )

Fig_3_A_plot  <- Fig_3_A_sex + Fig_3_A_age + Fig_3_A_infection + plot_layout(ncol = 3, widths = c(1, 1, 1))
cairo_pdf("Fig.3-A.pdf", width = 12, height = 8)
print(Fig_3_A_plot)
dev.off()

##Fig.3 B
Fig_3_B_sex <- ggplot(Fig_3_B, aes(x = sex, y = cost, fill = method)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("#66C2B5", "#D4A9E3")) +
  labs(x = "sex", y = "Hospitalization Cost (CNY)") +
  scale_y_continuous(breaks = seq(0, max(Fig_3_B$cost), by = 10000)) +
  scale_x_discrete(labels = c("male" = "male", "female" = "female")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 0.5, family = "Arial"),
    legend.position = "none", 
    plot.title = element_blank(),
    axis.title.y = element_text(size = 12, face = "bold", family = "Arial"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank())

Fig_3_B_age <- ggplot(Fig_3_B, aes(x = age_group, y = cost, fill = method)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("#66C2B5", "#D4A9E3")) +
  labs(x = "Age Group", y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 0.5), 
    legend.position = "none",
    axis.text.y = element_blank(),   
    axis.ticks.y = element_blank(),  
    panel.background = element_blank(), 
    plot.background = element_blank(), 
    plot.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank())

Fig_3_B_infection <- ggplot(Fig_3_B, aes(x = infection_level, y = cost, fill = method)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("#66C2B5", "#D4A9E3")) +
  labs(x = "Infection Type", y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 0.5),
    axis.text.y = element_blank(),   
    axis.ticks.y = element_blank(),  
    panel.background = element_blank(), 
    plot.background = element_blank(),
    legend.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank()
  ) +
  guides(fill = guide_legend(title = NULL))

Fig_3_B_plot  <- Fig_3_B_sex + Fig_3_B_age + Fig_3_B_infection + plot_layout(ncol = 3, widths = c(1, 1, 1))
cairo_pdf("Fig.3-B.pdf", width = 12, height = 8)
print(Fig_3_B_plot)
dev.off()

##wilcoxon test
perform_wilcoxon_tests <- function(data, group_var, outcome_var) {
  results <- data %>%
    group_by(across(all_of(group_var))) %>%
    group_modify(~ {
      if(n_distinct(.x$method) < 2) {
        return(tibble(
          statistic = NA_real_,
          p.value = NA_real_,
          note = "Insufficient groups"
        ))
      }
      test_result <- suppressWarnings(
        wilcox.test(as.formula(paste(outcome_var, "~ method")), 
                    data = .x,
                    exact = FALSE)
      )
      
      tibble(
        statistic = test_result$statistic,
        p.value = test_result$p.value,
        note = ifelse(is.null(test_result$warning), NA_character_, "Ties present")
      )
    }) %>%
    ungroup()
  results <- results %>%
    mutate(
      significance = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        TRUE ~ "ns"
      )
    )
  colnames(results)[1] <- "Group" 
  results$Outcome <- outcome_var
  
  return(results)
}
run_all_tests <- function(data, outcome_var, analysis_name) {
  bind_rows(
    perform_wilcoxon_tests(data, "sex", outcome_var) %>% mutate(Group_Type = "Sex"),
    perform_wilcoxon_tests(data, "age_group", outcome_var) %>% mutate(Group_Type = "Age Group"),
    perform_wilcoxon_tests(data, "infection_level", outcome_var) %>% mutate(Group_Type = "Infection Type")
  ) %>%
    mutate(Analysis = analysis_name) %>%  
    select(
      Analysis,             
      Group_Type,
      Group,
      W_statistic = statistic, 
      p_value = p.value,       
      significance,
      note
    )
}
df_new <- df %>%
  mutate(
    age_group = cut(age, 
                    breaks = c(54, 59, 64, 69),
                    labels = c("55-59", "60-64", "65-69"),
                    right = TRUE,
                    include.lowest = TRUE),
    infection_level = factor(infection_type_all, 
                             levels = c("1", "2", "3"),
                             labels = c("Not Detected Pathogens", "Common", "Uncommon or Rare")),
    method = factor(label, 
                    levels = c("0", "1"),
                    labels = c("mNGS(+)", "Culture(+)")))
los_results <- run_all_tests(df_new, "hospitalization_length_new", "Length of Stay")
cost_results <- run_all_tests(df_new, "cost", "Hospital Cost")
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "Test_Results")
writeData(wb, "Test_Results", bind_rows(los_results, cost_results))
addWorksheet(wb, "Processed_Data_A")
writeData(wb, "Processed_Data_A", Fig_3_A)
addWorksheet(wb, "Processed_Data_B")
writeData(wb, "Processed_Data_B", Fig_3_B)
saveWorkbook(wb, "Analysis_Results.xlsx", overwrite = TRUE)