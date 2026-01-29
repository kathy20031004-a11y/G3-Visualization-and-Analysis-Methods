library(openxlsx)
library(vegan)
library(ggplot2)
library(ggrepel)

# 1. 读取数据
species <- read.xlsx("物种信息.xlsx", rowNames = TRUE)
env <- read.xlsx("理化因子.xlsx", rowNames = TRUE)

# 2. 预处理
common_samples <- intersect(rownames(species), rownames(env))
species <- species[common_samples, ]
env <- env[common_samples, ]
species_hell <- decostand(species, method = "hellinger")
env_scaled <- scale(env)

# 3. 执行 RDA
rda_result <- rda(species_hell ~ ., data = as.data.frame(env_scaled))

# 4. 提取得分
site_scores <- scores(rda_result, display = "sites", choices = 1:2)
species_scores <- scores(rda_result, display = "species", choices = 1:2, scaling = 2)

# 转换为数据框
sites_df <- as.data.frame(site_scores)
colnames(sites_df) <- c("RDA1", "RDA2")
sites_df$Sample <- rownames(sites_df)

species_df <- as.data.frame(species_scores)
colnames(species_df) <- c("RDA1", "RDA2")
species_df$Species <- rownames(species_df)

# 筛选重要的物种（在 ggplot 调用之前）
species_important <- species_df[abs(species_df$RDA1) > 0.1 | abs(species_df$RDA2) > 0.1, ]

# 获取环境因子坐标（从 RDA 对象中直接提取）
if(!is.null(rda_result$CCA$biplot)) {
  env_df <- as.data.frame(rda_result$CCA$biplot[, 1:2])
} else {
  # 尝试其他方法
  biplot_scores <- scores(rda_result, display = "bp", choices = 1:2)
  if(!is.null(biplot_scores)) {
    env_df <- as.data.frame(biplot_scores)
  } else {
    # 手动计算相关性作为环境因子坐标
    site_coords <- scores(rda_result, choices = 1:2, display = "wa")
    env_cor <- cor(env_scaled, site_coords)
    env_df <- as.data.frame(env_cor)
  }
}

colnames(env_df) <- c("RDA1", "RDA2")
env_df$Env <- rownames(env_df)

# 5. 创建 RDA 图
rda_plot <- ggplot() +
  # 样本点
  geom_point(data = sites_df, aes(x = RDA1, y = RDA2), size = 3, color = "#FF9800", alpha = 0.7) +
  geom_text_repel(data = sites_df, aes(x = RDA1, y = RDA2, label = Sample), 
                  size = 3.5, color = "#FF9800", max.overlaps = 20, 
                  box.padding = 0.3, point.padding = 0.3) +
  
  # 物种箭头（筛选后的重要物种）
  geom_segment(data = species_important, 
               aes(x = 0, y = 0, xend = RDA1*0.7, yend = RDA2*0.7),
               arrow = arrow(length = unit(0.15, "cm")), 
               color = "red", alpha = 0.7, linewidth = 0.5) +
  geom_text_repel(data = species_important, 
                  aes(x = RDA1*0.75, y = RDA2*0.75, label = Species),
                  size = 3, color = "red", max.overlaps = 30, alpha = 0.8,
                  box.padding = 0.2, point.padding = 0.2) +
  
  # 环境因子箭头
  geom_segment(data = env_df, 
               aes(x = 0, y = 0, xend = RDA1*0.9, yend = RDA2*0.9),
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "black", linewidth = 1) +
  geom_text_repel(data = env_df, 
                  aes(x = RDA1*1.05, y = RDA2*1.05, label = Env),
                  size = 4, color = "black", fontface = "bold", 
                  box.padding = 0.4, point.padding = 0.4) +
  
  # 添加坐标轴和原点
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60", alpha = 0.5) +
  
  # 添加标签
  labs(
    title = "RDA analysis : Relationship between species composition and environmental factors",
    subtitle = paste("Constraint variance interpretation rate :", 
                     round(100 * rda_result$CCA$tot.chi / rda_result$tot.chi, 1), "%"),
    x = paste("RDA1 (", round(100 * summary(rda_result)$cont$importance[2, 1], 1), "%)", sep = ""),
    y = paste("RDA2 (", round(100 * summary(rda_result)$cont$importance[2, 2], 1), "%)", sep = "")
  ) +
  
  # 主题设置
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40"),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none"
  ) +
  coord_fixed(ratio = 1)

# 显示图形
print(rda_plot)

# 6. 保存图形（可选）
# ggsave("RDA_plot.png", rda_plot, width = 10, height = 8, dpi = 300)

# 7. 统计检验
cat("\n=== RDA 整体显著性检验 ===\n")
anova_result <- anova(rda_result, permutations = 999)
print(anova_result)

cat("\n=== RDA 各轴显著性检验 ===\n")
anova_axis <- anova(rda_result, by = "axis", permutations = 999)
print(anova_axis)

# 8. 查看环境因子与RDA轴的相关性
cat("\n=== 环境因子与RDA轴相关性 ===\n")
site_coords <- scores(rda_result, choices = 1:2, display = "sites")
env_cor <- cor(env_scaled, site_coords)
print(round(env_cor, 3))

# 9. 查看物种与环境因子的相关性
cat("\n=== 重要物种与环境因子相关性 ===\n")
# 提取前5个重要物种
top_species <- names(sort(rowSums(abs(species_scores)), decreasing = TRUE)[1:5])
species_cor <- cor(species[, top_species], env_scaled)
print(round(species_cor, 3))