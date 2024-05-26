library(IsoformSwitchAnalyzeR, quietly=TRUE)
library(WGCNA) 

## 导入Salmon 定量数据
isoformExpr <- importIsoformExpression(parentDir="./salmon_quant_sf", pattern="quant.sf")
str(isoformExpr)

isoform_TPM <- isoformExpr$abundance
rownames(isoform_TPM) <- isoform_TPM$isoform_id
isoform_TPM <- isoform_TPM[,-1]


## 导入样本信息
sample_name <- read.table("./SraRunTable_SRP092408.txt",header = T,sep = ",")
sample_name$Run <- paste(sample_name$Run,".salmon.count",sep = "")
sample_name <- sample_name[sample_name$Run %in% colnames(isoform_TPM),]
# (sample_name$Run == colnames(isoform_TPM)) %>% table()

colnames(isoform_TPM) <- sample_name$Sample.Name


## 导入fpkm表达数据
fpkm_matrix <- read.table("./fpkm_matrix.txt",sep = "\t",header = T)


## 选取重合样本
fpkm_matrix <- fpkm_matrix[,colnames(fpkm_matrix) %in% colnames(isoform_TPM)]
isoform_TPM <- isoform_TPM[,colnames(isoform_TPM) %in% colnames(fpkm_matrix)]
# (colnames(fpkm_matrix) == colnames(isoform_TPM)) %>% table()
isoform_TPM <- isoform_TPM[rowSums(isoform_TPM)>100,
                           colSums(isoform_TPM)>0]


## WGCNA
datExpr <- log2(isoform_TPM+1) %>% t() %>% as.data.frame() 
  

# choose the Soft threshold
# powers = c(c(1:10), seq(from = 12, to=30, by=2))
# sft = pickSoftThreshold(datExpr,dataIsExpr = TRUE,
#                         powerVector = powers,
#                         corFnc = cor,
#                         corOptions = list(use = 'p'),
#                         networkType = 'unsigned') 
# power =sft$powerEstimate 
power =4


## Construct gene co-expression network
# TOM = TOMsimilarityFromExpr(datExpr,networkType = 'signed',TOMType = 'signed',power = power) 
# colnames(TOM) =rownames(TOM) = rownames(isoform_TPM)
# saveRDS(TOM,file = "./TOM.Rds")
TOM <- readRDS("./TOM.Rds")
# rm(power)


## Recognition gene set
dissTOM=1-TOM
geneTree = hclust(as.dist(dissTOM),method='average') # 对加权的基因矩阵进行聚类。
minModuleSize = 100 # at least 50 genes in each gene set
dynamicMods = cutreeDynamic(dendro = geneTree,
                            distM = dissTOM,
                            method='hybrid',
                            deepSplit = 2,
                            pamRespectsDendro = FALSE,
                            minClusterSize = minModuleSize)
dynamicColors = labels2colors (dynamicMods) 
table(dynamicMods) 
table(dynamicColors)
# rm(TOM,dissTOM,minModuleSize)


## claculated Module Eigengenes
MEList = moduleEigengenes (datExpr, colors = dynamicColors)
MEs = MEList$eigengenes
MEDiss = 1-cor(MEs)
METree = hclust(as.dist(MEDiss), method = "average")

# 绘图
plotDendroAndColors(geneTree, dynamicColors,
                    "Dynamic Tree Cut", 
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05)



## Calculate the correlation  between disease type and gene set
design <- data.frame(row.names = colnames(fpkm_matrix),
                     SF3B6 = log2(fpkm_matrix["SF3B6",]+1) %>% as.numeric())
  MEs0 <-  moduleEigengenes(datExpr, dynamicColors)$eigengenes 
MEs <-  orderMEs(MEs0) 
moduleTraitCor <-  cor(MEs, design , use = "p") 
nSamples <-  nrow(datExpr)
moduleTraitPvalue <-  corPvalueStudent(moduleTraitCor, nSamples) 
textMatrix <-  paste(signif(moduleTraitCor, 2), "\n(",signif(moduleTraitPvalue, 1), ")", sep = "");
dim(textMatrix) = dim(moduleTraitCor)

# 绘图
tiff("研究基础图4B.tif",width = 12,height = 40,res = 300,units = "cm")
labeledHeatmap(Matrix = moduleTraitCor,
                    xLabels = colnames(design),
                    xLabelsAngle = 0,
                    yLabels = row.names(moduleTraitCor),
                    yLabelsPosition = "left",
                    ySymbols = row.names(moduleTraitCor),
                    yColorLabels=TRUE,
                    colors = blueWhiteRed(50),
                    textMatrix = textMatrix,
                    setStdMargins = TRUE,
                    cex.text = 1,
                    zlim = c(-1,1),
                    main = NULL)
dev.off()


## Extract the genes from each module
cl=c()
module_gene=c()
module_colors=unique(dynamicColors)
for (color in module_colors){
  module=rownames(isoform_TPM) [which(dynamicColors==color)]
  group1=rep(color,times=length(module))
  cl=c(cl,group1)
  module_gene=c(module_gene,module)
}
my_module = data.frame("colour"=cl,"gene"=module_gene)

rm(module_gene,module_colors,module,group1,cl,color)


## chose MEblue module
MEblue_genes <- my_module[my_module$colour %in% c("turquoise","blue"),]
colnames(MEblue_genes)[2] <- "isoform_id"

## gene ID转换
partial_isoform_annotion <-  read.table(file = "./partial_isoform_annotion.txt",sep = "\t")

SF3B6_related_genes <- merge(partial_isoform_annotion,MEblue_genes,
                             by="isoform_id")

## KEGG分析
source("./gll_KEGG.R")
data.frame(SYMBOL=unique(SF3B6_related_genes$gene_id)) %>% gll_KEGG(KEGG_top = 20)


## 相关性分析
df <-rbind(log2(fpkm_matrix["SF3B6",]+1),
           log2(isoform_TPM[SF3B6_related_genes$isoform_id,]+1)) 

gene_name1<-c()
gene_name2<-c()
cor_r<-c()
pvalue<-c()

# for循环
for (r in 2:nrow(df)){
  g1=rownames(df)[1]
  g2=rownames(df)[r]
  c_r=cor(as.numeric(df[1,]),as.numeric(df[r,]),method="spearman")
  p=cor.test(as.numeric(df[1,]),as.numeric(df[r,]),method ="spearman")[[3]]
  ##保存每一步的数据，而不可直接以空向量作为每一步运行的结果
  gene_name1=c(gene_name1,g1)
  gene_name2=c(gene_name2,g2)
  cor_r=c(cor_r,c_r)
  pvalue=c(pvalue,p)
}

# 导出结果文件
data_cor<-data.frame(gene_name1,gene_name2,cor_r,pvalue)
head(data_cor)
dim(data_cor)
data_cor_output <- data_cor[which(data_cor$pvalue<0.05 &
                                    (data_cor$cor_r>=0.8)),]
colnames(data_cor_output)[2] <-"isoform_id" 
data_cor_output <- merge(data_cor_output,partial_isoform_annotion,by="isoform_id")

# write.csv(data_cor_output,file = "./data_cor_output.csv")

## 相关性绘图
library(ggpubr)
df2 <- t(df) %>% as.data.frame()
df2 %>% 
  ggplot(aes(x=SF3B6,y=ENST00000237500))+
  geom_point(size=3,alpha=0.8,color="black",fill="black",shape=21,stroke =1.5)+
  geom_smooth(method = "lm",color="red",se=T,size=1.5)+
  theme_classic()+
  stat_cor(size=6,method = "spearman",
           # label.x = 1,
           # label.y = 10,
           r.accuracy = 0.001 ,
           p.accuracy = 0.001,
           cor.coef.name = "R")+
  labs(x="SF3B6 exp (FPKM)",
       y="ENST00000237500 exp (FPKM)",
       title = "GSE89408" ) +
  theme_classic(base_line_size = 1.5 # 坐标轴的粗细
  )+
  theme(plot.title = element_text(hjust = 0.5,vjust=1,size=20,
                                  color = "black",face = "bold"),
        axis.text.x=element_text(vjust=1,size=15,color = "black"),
        axis.text.y =element_text(vjust=1,size=15,color = "black"),
        axis.title.x = element_text(vjust=1,size=15,color = "black",face = "bold"),
        axis.title.y = element_text(vjust=1,size=15,color = "black",face = "bold"),
        text = element_text(family = "serif"))