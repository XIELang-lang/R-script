## load the scrna-seq file (10x)
library(Seurat)
library (stringr)

data_dir <- getwd()
list.files(data_dir) # check path

# Read10X function  reads three files above and gets a counts matrix
count_matrix <- Read10X(data.dir = data_dir, 
                        gene.column = 2)

# Create a Seurat object
Seuratdata = CreateSeuratObject(counts = count_matrix,
                                min.cells = 3, 
                                min.features = 200)
CIA <- Seuratdata

## data QC
CIA[["percent.mt"]] <- PercentageFeatureSet(CIA, pattern = "^mt-") 

# Filter the cells 
CIA <- subset(CIA, subset = nFeature_RNA > 200 & nFeature_RNA < 7500 & percent.mt < 5)

# data Normalization
CIA <- NormalizeData(CIA, normalization.method = "LogNormalize",scale.factor = 10000)

# Identification of hypervariable genes
CIA <- FindVariableFeatures(CIA,selection.method = "vst",nfeatures = 2000)
top10 <- head(VariableFeatures(CIA), 10)

all.genes <- rownames(CIA)
CIA <- ScaleData(CIA, features = all.genes)

# Run PCA
CIA <- RunPCA(CIA,features = VariableFeatures(object = CIA))

# analysis
CIA <- FindNeighbors(CIA, dims = 1:10)
CIA <- FindClusters(CIA, resolution = 0.04)
CIA <- RunUMAP(CIA, dims = 1:10)


# cell type annotation
cluster.merged <- RenameIdents(object = CIA,
                               '0' = '1','1' = '1',
                               '2' = '2',
                               '3' = '3')

cluster.merged <- RenameIdents(object = cluster.merged,
                               '1' = 'Hematopoietic cells', 
                               '2' = 'Fibroblasts',
                               '3' = 'Perivascular cells')



# devtools::install_github(repo = "samuel-marsh/scCustomize")
library(scCustomize)
FeaturePlot_scCustom(cluster.merged, 
                     features = "Sf3b6",
                     label.size = 40)