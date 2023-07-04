# setwd
setwd('/home/user/Downloads/info_tud/quantitative_regional_economics')

# packages
library(readxl)
library(dplyr)

# clean data and join
ls_files <- list.files(paste(getwd(),'final_dataset', sep = '/'))

data_cleaning <- function(wd, ls_files_name) {
  file <- read_excel(paste(wd,'final_dataset',ls_files_name, sep = '/'))
  old.colname <- colnames(file)
  old.colname <- gsub("\\.\\.\\..*", "", old.colname)
  first.row.year <- as.character(file[c(1),])
  first.row.year[is.na(first.row.year)] <- ""
  new.colname <- paste(old.colname, first.row.year, sep = "_")
  new.colname[1:3] <- gsub("_", "", new.colname[1:3])
  new.colname <- gsub(" ", "_", new.colname )
  new.colname <- tolower(new.colname)
  colnames(file) <- new.colname
  file <- file[c(2:nrow(file)),]
  colnames(file)[1:3] <- c('id', 'space_unit', 'aggregate')
  num.part <- as.data.frame(apply(file[,c(4:ncol(file))], 2, as.numeric))
  str.part <- file[,c(1:3)]
  output.ds <- cbind(str.part, num.part)
}

# upload all files
ls_results_ds <- sapply(ls_files, function(i) data_cleaning(getwd(), i))

# merge datasets
merge.ds <- merged_dataset <- Reduce(function(x, y) merge(x, y, by = c('id', 'space_unit', 'aggregate')), ls_results_ds)

# estimate nan values per column
colSums(is.na(merge.ds))

# filter dataset based on year and specific features
col.selected <- colnames(merge.ds)[grep("id|space_unit|aggregate|2016|2017|2018|2019|2020", colnames(merge.ds))]
# y ~ Xs
features.names.filtered <- tolower(c("Bruttoinlandsprodukt", 
                                     "Beschäftigte", 
                                     "Kleinstbetriebe", "Kleinbetriebe", "Mittlere", "Großunternehmen",
                                     "Einwohner_von",
                                     "Haushalte mit",
                                     "Siedlungs-",
                                     "Ausgaben") )

col.selected.w.year.name <- col.selected[grep(
  paste( "id|space_unit|aggregate|",paste(features.names.filtered, collapse = "|"),  collapse = "|"), 
  col.selected)]

# final dataset
ds.final <- merge.ds[,col.selected.w.year.name]
dim(ds.final)


##-------------------------------
# make geofile
##-------------------------------

library(rgdal)
library(broom)
    