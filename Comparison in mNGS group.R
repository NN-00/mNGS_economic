###Comparison in mNGS group##
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(rstatix)

df <- read_excel("250730-Final-dataset-mNGS_economic.xlsx",sheet="Sheet1")
mngs_df <- df %>% filter(label == 0)
mngs_df <- mngs_df %>%
  mutate(
    Culture_count = rowSums(
      across(c(Culture_result_1, Culture_result_2, Culture_result_3),
             ~ !(. %in% c("(-)", "NA") | is.na(.)))
    )
  )
mngs_df <- mngs_df %>%
  mutate(mNGS_count = sapply(strsplit(mNGS_result, "\\|"), function(x) {
    if (all(is.na(x))) 0 else length(x)
  }))

## Fig.2 A
test_result <- wilcox_test(
  data = mngs_df %>% 
    select(Culture_count, mNGS_count) %>% 
    pivot_longer(everything(), names_to = "group", values_to = "value"),
  formula = value ~ group,
  paired = TRUE,
  detailed = TRUE
)
p_value <- test_result$p
# signify p-value
get_signif_symbol <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ "ns"
  )
}
F2_A_data <- mngs_df %>%
  select(Culture_count, mNGS_count) %>%
  pivot_longer(everything(), names_to = "Method", values_to = "Count") %>%
  mutate(Method = factor(Method, 
                         levels = c("Culture_count", "mNGS_count"),
                         labels = c("Culture (+)", "mNGS (+)")))
F2_A_plot <- ggplot(F1_A_data, aes(x = Method, y = Count, fill = Method)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  scale_fill_manual(values = c("#E64B35B2", "skyblue")) +
  labs(
    title = "Comparison of Pathogen Detection",
    y = "Number of Pathogen Species (Per Individual)",
    x = "Detection Method"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  geom_signif(
    comparisons = list(c("Culture (+)", "mNGS (+)")),
    annotations = get_signif_symbol(p_value),
    y_position = max(F1_A_data$Count) * 1.1,
    tip_length = 0.01
  ) +
  annotate(
    "text", x = 1.5, y = max(F1_A_data$Count) * 1.15,
    label = ifelse(p_value < 0.001, 
                   "p < 0.001", 
                   paste0("p = ", round(p_value, 3))),
    size = 4
  )
print(F1_A_plot)
ggsave("Fig.2 A.pdf", F2_A_plot, width = 8, height = 6, units = "in")

## Fig.2 B
F2_B_data <- read_excel("250730-Final-dataset-mNGS_economic.xlsx", sheet = "mNGS group")
filtered_F2_B_data <- F2_B_data %>%
  filter(rowSums(select(., `mNGS(+),Culture(+)`, `mNGS(+),Culture(-)`, `mNGS(-),Culture(+)`)) > 0)
pathogen_order <- filtered_F2_B_data$name
long_F2_B_data <- filtered_F2_B_data %>%
  mutate(name = factor(name, levels = rev(pathogen_order))) %>%  
  pivot_longer(cols = c(`mNGS(+),Culture(+)`, `mNGS(+),Culture(-)`, `mNGS(-),Culture(+)`),
               names_to = "Detection_Type",
               values_to = "Frequency") %>%
  mutate(Detection_Type = factor(Detection_Type,
                                 levels = c("mNGS(+),Culture(+)", "mNGS(+),Culture(-)", "mNGS(-),Culture(+)")))

F2_B_plot <- ggplot(long_F2_B_data, aes(x = Frequency, y = name, fill = Detection_Type))  +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_fill_manual(values = c("#E8B3A0", "#81B5CA", "#EFD171"),
                    labels = c("mNGS(+), Culture(+)", 
                               "mNGS(+), Culture(-)", 
                               "mNGS(-), Culture(+)")) +
  labs(title = "Pathogen Detection Frequency by Method",
       x = "Detection Frequency",
       y = "Pathogen Species",
       fill = "Detection Type") +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y = element_text(size = 8),  
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()  
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)))  
                     
print(F2_B_plot)
ggsave("Fig.2 B.pdf", F2_B_plot, width = 8, height = 6, units = "in")


##Fig.2 C
F2_C_data <- filtered_F2_B_data %>%
  group_by(type) %>%
  summarise(
    Culture_pos = sum(`mNGS(+),Culture(+)` + `mNGS(-),Culture(+)`),
    mNGS_pos = sum(`mNGS(+),Culture(+)` + `mNGS(+),Culture(-)`),
    Total = Culture_pos + mNGS_pos
  ) %>%
  ungroup() %>%
  mutate(
    type = factor(type, levels = c("Common", "Uncommon", "Rare")),
    Culture_pct = Culture_pos / sum(Culture_pos) * 100,
    mNGS_pct = mNGS_pos / sum(mNGS_pos) * 100
  )
long_F2_C_data <- F2_C_data %>%
  pivot_longer(
    cols = c(Culture_pos, mNGS_pos),
    names_to = "Method",
    values_to = "Count"
  ) %>%
  mutate(
    Method = factor(Method, 
                    levels = c("Culture_pos", "mNGS_pos"),
                    labels = c("Culture", "mNGS")),
    label = ifelse(Method == "Culture",
                   sprintf("%d(%.1f%%)", Count, Culture_pct),
                   sprintf("%d(%.1f%%)", Count, mNGS_pct)))
F2_C_plot <- ggplot(long_F2_C_data, aes(x = Count, y = Method, fill = type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3.5, color = "white") +
  scale_fill_manual(values = c("#1B2D63", "#D88D23", "#A62025"),  
                    breaks = c("Common", "Uncommon", "Rare")) +   
  labs(
    title = "Pathogen Detection by Frequency Category",
    x = "Number of Detections",
    y = "Detection Method",
    fill = "Pathogen Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",
    panel.grid.major.y = element_blank(),  # 移除垂直网格线
    axis.text.y = element_text(size = 11)  # 加粗方法标签
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) 
print(F2_C_plot)

ggsave("Fig.2 C.pdf", F2_C_plot, width = 8, height = 6, units = "in")
