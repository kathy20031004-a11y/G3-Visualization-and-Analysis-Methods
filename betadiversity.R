# ==============================================================================
# 1. 加载必要的包
# ==============================================================================

library(readxl)
library(vegan)     # 核心包：计算距离和排序
library(tidyverse) # 作图和数据处理
library(ggrepel)   # 让图上的标签互斥，不重叠

# ==============================================================================
# 2. 读取并清洗数据
# ==============================================================================
# 读取数据
raw_data <- read_excel("zuoye.xlsx")

# 转换为数值矩阵 (样本名为行名)
otu_table <- raw_data %>%
  column_to_rownames(var = "Location_Site")

# 简单过滤：移除没有任何计数的物种列（防止干扰）
otu_table <- otu_table[, colSums(otu_table) > 0]

print("用于分析的数据矩阵：")
print(otu_table)

# ==============================================================================
# 3. 计算 Beta 多样性距离矩阵 (Bray-Curtis Distance)
# ==============================================================================
dist_bray <- vegdist(otu_table, method = "bray")

print("Bray-Curtis 距离矩阵：")
print(dist_bray)

# ==============================================================================
# 4. PCoA 分析 (主坐标分析) 并绘图
# ==============================================================================

# 计算 PCoA
pcoa <- cmdscale(dist_bray, k = 2, eig = TRUE)

# 提取坐标点 (前两轴)
pcoa_points <- data.frame(pcoa$points)
colnames(pcoa_points) <- c("PCoA1", "PCoA2")
pcoa_points$Sample <- rownames(pcoa_points)

# 计算解释度 (坐标轴代表了多少差异百分比)
eig_percent <- round(pcoa$eig / sum(pcoa$eig) * 100, 1)

# 绘制 PCoA 散点图
plot_pcoa <- ggplot(pcoa_points, aes(x = PCoA1, y = PCoA2, color = Sample)) +
  geom_point(size = 5, alpha = 0.8) +             # 画点
  geom_text_repel(aes(label = Sample), size = 4) + # 加标签
  labs(title = "PCoA Analysis (Bray-Curtis)",
       x = paste0("PCoA 1 (", eig_percent[1], "%)"),
       y = paste0("PCoA 2 (", eig_percent[2], "%)")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 0, linetype="dashed", color="gray") +
  geom_hline(yintercept = 0, linetype="dashed", color="gray")

print(plot_pcoa)


# ==============================================================================
# 5. 保存图片
# ==============================================================================
ggsave("Beta_Diversity_PCoA.pdf", plot_pcoa, width = 6, height = 5)
