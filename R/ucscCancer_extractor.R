library(magrittr)
library(readr)
library(data.table)
library(plyr)

#===============================================================================
#   NAME: read.ucscCancer
#  USAGE: read ucsc cancer data and form list object
#  INPUT: 
#		f.gm: file name of genomicMatrix data
#		f.cd: file name of clinical data
#		f.ann: file name of annotation data
# OUTPUT:
#		ucscCancer object. a list, with component name of gm, cd, ann which means genomicMatrix,
#			clinical data, annotation
#===============================================================================
read.ucscCancer <- function(f.gm, f.cd, f.ann){
	if (missing(f.gm)){
		gm <- NA
	} else {
		gm <- read_tsv(f.gm)
	}
	if (missing(f.cd)){
		cd <- NA
	} else {
		cd <- read.table(f.cd, sep = "\t", header = T)
	}
	if (missing(f.ann)){
		ann <- NA
	} else {
		ann <- read_tsv(f.ann)
	}
	names(gm)[1] <- "probeID"
	dat.origin <- list(gm = gm, cd = cd, ann = ann)
	class(dat.origin) <- c("ucscCancer", class(dat.origin))
	dat.origin
}



read.ucscCancer.dir <- function(dir){
	#
	# NAME: read.ucscCancer.dir
	#
	# INPUT: 
	#   dir: directory of ucsc Cancer dataaset
	#
	# OUTPUT:
	#   ucscCancer object.
	#
	f.gm <- paste0(dir, "/genomicMatrix")
	f.cd <- paste0(dir, "/clinical_data")
	f.ann <- paste0(dir, "/probe")
	for (i.object in c("f.gm", "f.cd", "f.ann")){
		comm <- sprintf('if (!file.exists(%s)) %s <- NA', i.object, i.object)
		eval(parse(text = comm))
	}
	read.ucscCancer(f.gm, f.cd, f.ann)
}



#===============================================================================
#   NAME: extractProbe.ucscCancer
#  USAGE: extract typical probe from ucscCancer object
#  INPUT: 
#		probes: the probes to extract
#		datset: a ucscCancer object
# OUTPUT:
#		a ucscCancer object
#===============================================================================
extractProbe.ucscCancer <- function(probes, datset){
	datset$gm <- datset$gm %>% data.table(key = "probeID")
	datset$gm[probes] %>%
		data.frame(check.names = F) -> datset$gm
	datset
}


#===============================================================================
#   NAME: extractSample.ucscCancer
#  USAGE: extract given samples from ucscCancer object
#  INPUT: 
#		datset: a ucscCancer object
#		samples: the samples to be extract
# OUTPUT:
#		a ucscCancer object
#===============================================================================
extractSample.ucscCancer <- function(datset, samples){
	datset$cd <- datset$cd %>% data.table(key = "sampleID")
	if (!missing(samples)){
		datset$cd <- datset$cd[samples]
	}
	i.cd <- datset$cd$sampleID
	i.gm <- names(datset$gm)
	i.com <- intersect(i.cd, i.gm)
	datset$cd <- data.table(datset$cd, key = "sampleID")
	datset$cd <- datset$cd[i.com]
	datset$cd <- datset$cd %>% data.frame
	datset$gm <- datset$gm[, c("probeID", i.com)]
	datset
}

#=================================================
# cange list to data.frame by combind gm, cd and annotation?
#=================================================
#===============================================================================
#   NAME: l2df.ucscCancer
#  USAGE: combind genomicMatrix data and clinical data into a data.frame
#  INPUT: 
#		datset: a ucscCancer object
#		cd.vars: variables in clinical data need be extract
# OUTPUT:
#		a data.frame with the first lines are probe signal data and rest of clinical data
#===============================================================================
l2df.ucscCancer <- function(datset, cd.vars){
	gm.sub <- datset$gm %>% t
	gmCd.df <- data.frame(rownames(gm.sub)[-1], data.matrix(data.frame(gm.sub[-1, ])), datset$cd[[cd.vars]])
	names(gmCd.df)[1:ncol(gmCd.df)] <- c(rownames(gm.sub)[1], gm.sub[1, ], cd.vars)
	gmCd.df
}



## example not run
# f.gm <- "/home/bob/work_dir/Wu/UCSC_cancer/LUNG/TCGA_LUNG_exp_HiSeqV2-2015-02-24/genomicMatrix"
# f.cd <- "/home/bob/work_dir/Wu/UCSC_cancer/LUNG/TCGA_LUNG_exp_HiSeqV2-2015-02-24/clinical_data"
# dat.origin <- read.ucscCancer(f.gm, f.cd)

# probes <- c("SPRY1", "SPRY2")
# dat1 <- extractProbe.ucscCancer(probes, dat.origin)
# dat2 <- extractSample.ucscCancer(dat1)
# dat3 <- l2df.ucscCancer(dat2, "pathologic_stage")



