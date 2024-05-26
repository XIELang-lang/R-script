library(IsoformSwitchAnalyzeR, quietly=TRUE)


## 导入Salmon 定量数据
isoformExpr <- importIsoformExpression(parentDir="./salmon_quant_sf", pattern="quant.sf")
str(isoformExpr)


## 构建 design matrix
samples <-list.files("./salmon_quant_sf")
condictions <- c(rep("Normal", 35),rep("RA", 34))
designM <- data.frame(sampleID=samples, condition=condictions)
designM


## 构建 switchAnalyzeRlist 对象
switchList1 <- importRdata(isoformCountMatrix=isoformExpr$counts,
                           isoformRepExpression=isoformExpr$abundance, 
                           designMatrix=designM, 
                           isoformExonAnnoation='./GRCh38.gtf', 
                           isoformNtFasta="./GRCh38.transcript.fa",
                           ignoreAfterPeriod =T)
# .gtf和.fa文件与构建Salmon 索引时一致


## 过滤
switchList2 <- preFilter(switchList1)
#默认过滤标准
# geneExpressionCutoff = 1 : 根据gene_overall_mean列过滤低表达基因
# isoformExpressionCutoff = 0 : 根据iso_overall_mean列过滤低表达基因
# IFcutoff=0.01: 根据IF_overall过滤
# dIFcutoff = 0.1: 根据dIF过滤（绝对值）
# removeSingleIsoformGenes = TRUE: 删除只有一个转录本的基因


## isoformSwitch分析
switchList3 <- isoformSwitchTestDEXSeq(switchList2)
# 增加了isoform_switch_q_value与gene_switch_q_value两列信息

# Extract top switching genes (by q-value)
switchingIso <- extractTopSwitches( 
  switchList3, 
  filterForConsequences = F, 
  n = NA,                  # n=NA: all features are returned
  extractGenes = FALSE,    # when FALSE isoforms are returned
  sortByQvals = TRUE
)


## 绘图
## 绘图
# 汇总图
switchPlot(switchList3,
           gene ='ELAC2',
           condition1="RA",
           condition2="Normal"
           # isoform_id = "ENST00000583371"
)

# 四个子图
switchPlotGeneExp(switchList3, gene = 'IL32',
                  condition1="Normal",
                  condition2="RA")
switchPlotIsoExp(switchList3, gene = 'IL32',
                 condition1="Normal",
                 condition2="RA")
switchPlotIsoUsage(switchList3, gene = 'IL32',
                   condition1="Normal",
                   condition2="RA")
switchPlotTranscript(switchList3, gene = 'IL32',
                     condition1="Normal",
                     condition2="RA")
# 火山图
ggplot(data=switchList3$isoformFeatures, aes(x=dIF, y=-log10(isoform_switch_q_value))) +
  geom_point(
    aes( color=abs(dIF) > 0.1 & isoform_switch_q_value < 0.05 ), # default cutoff
    size=1
  ) +
  geom_hline(yintercept = -log10(0.05), linetype='dashed') + # default cutoff
  geom_vline(xintercept = c(-0.1, 0.1), linetype='dashed') + # default cutoff
  scale_color_manual('Signficant\nIsoform Switch', values = c('black','red')) +
  labs(x='dIF', y='-Log10 ( Isoform Switch Q Value )') +
  theme_bw()