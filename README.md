# 加载必要的包
library(openxlsx)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(showtext)

# 1. 读取数据
data <- read.xlsx("Phylum_summary_zhu.xlsx", sheet = 1)

# 查看数据
print(data)

# 2. 数据预处理
# 确保数据格式正确
data$Percentage <- as.numeric(data$Percentage)
data$Count <- as.numeric(data$Count)

# 5. 创建环形图（donut chart） - 更现代
donut_plot <- ggplot(data, aes(x = 2, y = Percentage, fill = reorder(Phylum, -Percentage))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) +  # 创建环形效果
  
  # 在中心添加总数
  annotate("text", x = 0.5, y = 0, 
           label = paste("Total number of species\n", sum(data$Count)), 
           size = 5, fontface = "bold", color = "darkblue") +
  
  # 添加百分比标签
  geom_text(aes(y = ypos, label = paste0(round(Percentage, 1), "%")), 
            color = "black", size = 4.5, fontface = "bold") +
  
  # 颜色和主题
  scale_fill_brewer(palette = "Set2") +
  theme_void() +
  
  labs(
    title = "Ring diagram of species composition at gate level",
    fill = "Phylum"
  ) +
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right"
  )

print(donut_plot)

