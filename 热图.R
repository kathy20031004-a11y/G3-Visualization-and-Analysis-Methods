
library(openxlsx)
library(pheatmap)
library(RColorBrewer)

# 读取 Excel 文件
data <- read.xlsx("热图数据(1).xlsx", sheet = 1, rowNames = TRUE)

# 查看数据
print(head(data))

# 将数据转换为数值矩阵（确保都是数字）
mat <- as.matrix(data)

# 自定义颜色梯度（从浅到深，表示低到高）
my_colors <- colorRampPalette(c("white", "yellow", "orange", "red"))(50)

# 绘制热图
pheatmap(
  mat,
  color = my_colors,
  cluster_rows = TRUE,   # 是否对行进行聚类
  cluster_cols = TRUE,   # 是否对列进行聚类
  display_numbers = TRUE, # 在热图中显示数值
  fontsize_number = 8,   # 数值字体大小
  fontsize_row = 8,     # 行名字体大小
  fontsize_col = 8,     # 列名字体大小
  angle_col = 45,        # 列名倾斜角度
  border_color = "grey60",
  cellwidth = 25,        # 单元格宽度
  cellheight = 20        # 单元格高度
)

