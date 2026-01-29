#将所有fasta文件移动到同一个文件夹中
#合并所有fasta文件为一个文件
cat /path/to/file/*seq > name.fasta
#多序列比对：mafft工具
mafft --auto name.fasta > aligned.fasta
#修剪：trimal
trimal -in aligned.fasta -out trimmed.fasta -automated
#iqtree
##激活iqtree环境
conda activate iqtree
##运行iqtree
iqtree -s trimmed.fasta -m MFP -B 1000 -T AUTO --prefix tree
#iTOL美化进化树