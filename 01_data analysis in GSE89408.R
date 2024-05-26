## load the packsges
library(DESeq2)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)


## load the RNA splicing related genes
SRGs <- read_excel(path = "./GO0008380.xlsx")[,c(2,3,4,6,9)]
SRGs$Symbol <- toupper(SRGs$Symbol)


## load gene annotations 
# apath <- paste(urld, "type=rnaseq_counts", "file=Human.GRCh38.p13.annot.tsv.gz", sep="&")
annot <- data.table::fread("./Human.GRCh38.p13.annot.tsv.gz", header=T, quote="", stringsAsFactors=F, data.table=F)
rownames(annot) <- annot$GeneID


## load counts table from GSE89408
# urld <- "https://www.ncbi.nlm.nih.gov/geo/download/?format=file&type=rnaseq_counts"
# path <- paste(urld, "acc=GSE89408", "file=GSE89408_raw_counts_GRCh38.p13_NCBI.tsv.gz", sep="&")
count_matrix <- as.matrix(data.table::fread("./GSE89408_raw_counts_GRCh38.p13_NCBI.tsv.gz",header=T, colClasses="integer"), rownames="GeneID")


## sample selection :RA vs Normal
gsms <- paste0("11111111111111111111111XXXXXXXXXXXXXXXXXXXXXXXXXXX",
               "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
               "XXXXXXXXXXXXXX000000000000000000000000000000000000",
               "00000000000000000000000000000000000000000000000000",
               "0000000")
sml <- strsplit(gsms, split="")[[1]]
sel <- which(sml != "X")
sml <- sml[sel]
count_matrix <- count_matrix[ ,sel]
gs <- factor(sml)
groups <- make.names(c("RA","Normal"))
levels(gs) <- groups
sample_info <- data.frame(Group = gs, row.names = colnames(count_matrix))
write.table(sample_info,file = "./sample_info.txt",sep = "\t")

## pre-filter low count genes
# keep genes with at least N counts > 10, where N = size of smallest group
keep <- rowSums( count_matrix >= 10 ) >= min(table(gs))
count_matrix <- count_matrix[keep, ]
ds <- DESeqDataSetFromMatrix(countData=count_matrix, colData=sample_info, design= ~Group)
ds <- DESeq(ds, test="Wald", sfType="poscount")


## extract results for top genes table
r <- results (ds, contrast=c("Group", groups[1], groups[2]), alpha=0.05, pAdjustMethod ="fdr")
tT <- r[order(r$padj),] 
tT <- merge(as.data.frame(tT), annot, by=0, sort=F)
DEGS <- subset(tT, select=c("Symbol","Description","log2FoldChange","pvalue","padj"))
DEGS_sig <- DEGS[DEGS$pvalue <0.05 & abs(DEGS$log2FoldChange)> 1.5,]
# write.table(tT, file=stdout(), row.names=F, sep="\t")


## count 转 FPKM
count_matrix_1 <- data.frame(row.name = tT[order(tT$Row.names,decreasing = F),"Symbol"],
                             count_matrix[order(rownames(count_matrix),decreasing = F),],
                             tT[order(tT$Row.names,decreasing = F),"Length"])
colnames(count_matrix_1)[ncol(count_matrix_1)] <- "Length"
count_matrix_2 <- aggregate(.~row.name,mean,data=count_matrix_1)
rownames(count_matrix_2) <- count_matrix_2$row.name
count_matrix_2 <- count_matrix_2[,-1]
fpkm_matrix <- data.frame(row.names = rownames(count_matrix_2))
for (i in 1:(ncol(count_matrix_2)-1)){
  col <- count_matrix_2[[i]]
  N <- sum(col) %>% as.numeric() # 计算每个样本的mapped reads数
  FPKMi <- (col*1e9)/(N*count_matrix_2[[dim(count_matrix_2)[2]]]) # 计算FPKM值
  FPKMi <- pmax(FPKMi,0) %>% as.data.frame() # 去掉矫正带来的负值
  colnames(FPKMi) <- colnames(count_matrix_2)[i]
  fpkm_matrix <- cbind(fpkm_matrix,FPKMi)
}
rm(FPKMi,col,i,N)

write.table(fpkm_matrix,file = "./fpkm_matrix.txt",sep = "\t")

## 交集基因
SRGs_sig <- DEGS_sig[DEGS_sig$Symbol %in% SRGs$Symbol,]


## 热图
library(pheatmap)
log2(fpkm_matrix[SRGs_sig$Symbol,] +1)%>% 
  pheatmap(scale = "column",
           cluster_col = F,
           cluster_rows = T,
           border_color = "grey", 
           show_rownames=T,
           show_colnames=F,  
           # cellwidth = 40,  
           fontsize = 10,
           fontfamily= "serif",
           annotation_col = sample_info,
           main = "GSE89408"
  )


## 箱撕图
data <- cbind(sample_info,as.data.frame(t(fpkm_matrix)))
# write.csv(data[,c("Group","SF3B6")],file = "./data for boxplot.csv")

data %>%   
  ggplot(mapping = aes(x=Group,y=log2(SF3B6)))+
  geom_boxplot()+
  geom_jitter()+
  geom_point()+
  theme_bw()+
  # ylim(0,1)+
  ylab("log2(SF3B6 FPKM)")+
  xlab(NULL)+
  stat_compare_means(method = "t.test",
                     label = "p.format",
                     show.legend = T)

