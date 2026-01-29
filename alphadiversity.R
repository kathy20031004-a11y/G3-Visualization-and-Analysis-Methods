# ==============================================================================
# 1. 加载必要的R包
# ==============================================================================
library(readxl)    # 用于读取Excel文件
library(vegan)     # 核心生态学包，用于计算Shannon, Simpson, Chao1, ACE等
library(tidyverse) # 用于数据处理 (dplyr, tidyr) 和绘图 (ggplot2)
library(patchwork) # 用于将多个图表合并 (拼图)

# ==============================================================================
# 2. 数据导入与预处理
# =============================================================================

# ----------------- 读取实际 Excel 文件  ---------
data <- read_excel("zuoye.xlsx") 

# ----------------- 数据清洗 -----------------
otu_table <- data %>% 
  column_to_rownames(var = "Location_Site")

# 移除全为0的物种列 (可选，避免计算干扰，但针对你的数据Delftia等全为0)
otu_table <- otu_table[, colSums(otu_table) > 0]

# ==============================================================================
# 3. 计算 Alpha 多样性指数
# ==============================================================================

# (1) Richness (Observed Species): 观测到的物种数
# specnumber 计算每个样本中丰度大于0的物种数量
richness <- specnumber(otu_table)

# (2) Shannon index: 香农指数
shannon <- diversity(otu_table, index = "shannon")

# (3) Simpson index: 辛普森指数 (默认计算的是 Gini-Simpson = 1 - D)
simpson <- diversity(otu_table, index = "simpson")

# (4) Evenness (Pielou's J): 均匀度
evenness <- shannon / log(richness)
evenness[is.nan(evenness)] <- 0 

# (5) Chao1 和 ACE: 稀疏度估计
estimators <- estimateR(otu_table)
# 提取 Chao1 和 ACE (注意 estimateR 返回的是转置的矩阵，样本在列)
chao1 <- estimators["S.chao1", ]
ace   <- estimators["S.ACE", ]

# ==============================================================================
# 4. 整合结果数据
# ==============================================================================
alpha_df <- data.frame(
  Sample = rownames(otu_table),
  Richness = richness,
  Shannon = shannon,
  Simpson = simpson,
  Evenness = evenness,
  Chao1 = chao1,
  ACE = ace
)

print("计算结果表：")
print(alpha_df)

# ==============================================================================
# 5. 绘制柱状图
# ==============================================================================

# 定义一个绘图函数，避免重复写代码
plot_alpha <- function(data, y_col, title_text) {
  ggplot(data, aes(x = Sample, y = .data[[y_col]], fill = Sample)) +
    geom_bar(stat = "identity", width = 0.6, color = "black", alpha = 0.8) +
    theme_classic() + # 使用经典简洁主题
    labs(title = title_text, y = y_col, x = "") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"), # 标题居中加粗
      legend.position = "none",  # 隐藏图例(因为X轴已经是样本名了)
      axis.text.x = element_text(angle = 45, hjust = 1) # X轴标签倾斜45度
    )
}

# 分别绘制6张图
p1 <- plot_alpha(alpha_df, "Richness", "Richness Index")
p2 <- plot_alpha(alpha_df, "Shannon",  "Shannon Index")
p3 <- plot_alpha(alpha_df, "Chao1",    "Chao1 Estimator")
p4 <- plot_alpha(alpha_df, "Simpson",  "Simpson Index")
p5 <- plot_alpha(alpha_df, "Evenness", "Evenness (Pielou's J)")
p6 <- plot_alpha(alpha_df, "ACE",      "ACE Estimator")

# ==============================================================================
# 6. 合并图表 (2行 * 3列)
# ==============================================================================

# 使用 patchwork 包进行拼图
combined_plot <- (p1 + p2 + p3) / (p4 + p5 + p6) + 
  plot_annotation(
    title = 'Alpha Diversity Indices across Sites',
    caption = 'Data source: zuoye.xlsx'
  )

# 显示图表
print(combined_plot)

# ==============================================================================
# 7. 保存结果 (可选)
# ==============================================================================
ggsave("Alpha_Diversity.pdf", combined_plot, width = 12, height = 8, dpi = 300)
