# ### setup:
# install.packages(c("ggplot2", "HMM", "zoo", "reshape", "plyr", "foreach", "Kendall", "XML", "stringr"))
# install.packages('snippets',,'http://www.rforge.net/')
# source("http://bioconductor.org/biocLite.R")
# biocLite("Biostrings")
# biocLite("VennDiagram")

# baseDirectory = "/Volumes/bioinformatics/projects/alberti/"


library(snippets)
library(HMM)
library(zoo)
library(reshape)
library(plyr)
library(VennDiagram)
library(Biostrings)
library(Kendall)
library(stringr)
library(ggplot2)

readFastaIntoDF <- function(fileName){
	## read a fasta file as data.fram

	fastaData <- read.BStringSet(fileName, "fasta")
	fastaDataDF <- as.data.frame(as.character(fastaData), stringsAsFactors=FALSE)
	fastaDataDF <- transform(fastaDataDF, GeneDesc=rownames(fastaDataDF))
	rownames(fastaDataDF) <- NULL
	names(fastaDataDF)[1] <- "Sequence"
	return(fastaDataDF)
}


# DEBUG asSequences<- dict_disc_proteome$Sequence

# DEBUG asSequences<- subset(dict_disc_proteome, str_detect(GeneDesc, "0001590"))$Sequence
calcASFreqDist <- function(asSequences, useSeqLetters=FALSE){
	## calclate the relative AS frequency distribution

	sequenceSeq <- BStringSet(asSequences)
	
	if(useSeqLetters){
		foundAS <- uniqueLetters(sequenceSeq);
	}else{
		foundAS <- c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y")
	}
	
	letFreqs <- letterFrequency(sequenceSeq, foundAS)
	letFreqs <- letFreqs + 1  ## add 1 offset as Ian did
	relativeLetFreqs <- letFreqs/rowSums(letFreqs) ## calcuate relative frequencies
	asFreqDist <- colSums(100*relativeLetFreqs)/nrow(relativeLetFreqs) ## do the normalization
	
	return(data.frame(amacid=names(asFreqDist), frequency=asFreqDist, row.names=NULL))
}


runHMM <- function(GeneDesc, Sequence, hmm, ...){
	# print(GeneDesc)
	t1 <- unlist(strsplit(Sequence, split=""))
	# browser()
	## do the viterbi alignment
	o1<-viterbi(hmm, t1)
	output <- paste(o1, collapse="")
	
	## calculate the score
	o2<-posterior(hmm, t1)
	dfo2 <- melt(o2)                            #convert matrix to data frame
	o2p <- subset(dfo2, states=="P")$value    #Split to get just Prion probabilities
	# o2b <- subset(dfo2, states=="B")$value    #Split to get just Prion probabilities
	# browser()
	ggplot(dfo2, aes(index, value, color=states)) + geom_line()
	coreScore=as.numeric(NA)
	if(nchar(Sequence)>=60) coreScore = max(rollmean(o2p,60))  #calculate maximum mean of posterior prion probabilities for a sliding window of 60
	# if(nchar(Sequence)>=60) coreScore = max(rollmean(o2p,60)/rollmean(o2b,60))  #Paper method: calculate max ratio of the moving average mean in both  states
	
	return(c(Segmentation=output, maxRollPScore=coreScore))	
}


# calcScore <- function(GeneDesc, Sequence, hmm, ...){
# 	t1 <- unlist(strsplit(Sequence, split=""))
# 	o2<-posterior(hmm, t1)
# 	dfo2 <- melt(o2)                            #convert matrix to data frame
# 	o2p <- subset(dfo2, states=="P")$value    #Split to get just Prion probabilities
# 	# 	ggplot(dfo2, aes(index, value, color=states)) + geom_line()
# 	coreScore=0
# 	if(nchar(Sequence)>=60) coreScore = max(rollmean(o2p,60))  #calculate maximum mean of posterior prion probabilities for a sliding window of 60
# 	return(c(maxRollPScore=coreScore))
# }


createPyTable <- function(cmd,...){
## runs a python script that prints a result table to stout. This table then is parsed back into a data.frame
	
	tFile = tempfile()
	system(paste("source ~/.bash_profile; python ", cmd, " > ", tFile))
	# browser()
	result <- read.table(tFile, ...)
	unlink(tFile)
	return(result)
}


write.fasta <- function(seq_names, sequences, file){
	fastaData <- AAStringSet(sequences)
	names(fastaData) <- seq_names;
	write.XStringSet(fastaData, file=file, format="fasta", width=80)
}

extractDomainSegs <- function(viterbi_paths){
## extracts all the prion segments with at least 60 consecutive Psviterbi_paths
	domain_segments <- str_locate_all(viterbi_paths$Segmentation, "P{60,}")
	rownames(viterbi_paths) <- 1:nrow(viterbi_paths)
	names(domain_segments) <- 1:length(domain_segments)
	domain_segments <- ldply(domain_segments, as.data.frame)
	subset(merge(viterbi_paths, domain_segments, by.x=0, by.y=".id", all.x=TRUE), select=-Row.names)
}


xout_pdomains <- function(df_with_segs_and_seq){
	adply(df_with_segs_and_seq, 1, splat(function(Sequence, Segmentation, ...){
		# browser()
		split_sequence <- unlist(strsplit(Sequence, split=""))
		split_align <- unlist(strsplit(Segmentation, split=""))
	
		split_sequence[which(split_align=="P")] = "X"
	
		return(c(prion_xout_sequence=paste(split_sequence, collapse="")))	
	}), .progress="text")
}

# ## use this to enable multiple cores for ddply
# library(plyr)
# library(foreach)
# library(doMC)
# registerDoMC(cores=8)


prion_model= data.frame(amacid=c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y"),
	prion_frequency=c(4.2501322, 0.8495353, 2.0713379, 1.7073955, 3.3644566, 10.8440664, 1.0713270, 1.5822792, 2.7584645, 3.4096907, 2.0704712, 19.3603931, 3.5267232, 16.2506059, 2.3562638, 10.5433046, 3.2207167, 2.1705145, 0.8495353, 7.7427865))

## J O U  encoding AS which could be sequenced properly
dict_specific_amacids=data.frame(amacid=c("X", "*", "J", "O", "U"), prion_frequency=0.000001, frequency=0.000001)

createPrionHMM <- function(state_model){
	States        <- c("P", "B")
	StartProbs    <- c(0.05,0.95)
	TransProbs    <- matrix(c(0.98,0.02,  0.001,0.999),2, byrow=TRUE)
	
	Symbols <- state_model$amacid
	EmissionProbs <- matrix(c(state_model$prion_frequency, state_model$frequency), 2, byrow=TRUE)
	# EmissionProbs <- EmissionProbs/rowSums(EmissionProbs)
	
	prion_hmm <- initHMM(States, Symbols, StartProbs,TransProbs, EmissionProbs)
	
	return(prion_hmm)
}

write.tsv <- function(df, ...){
	write.table(df, row.names=FALSE, sep="\t", ...)
}

## set the base directory for the analysis
baseDirectory = "~/DropBox/prion_domains/"


#####################################################################
### S. Cerevisiae
#####################################################################

setwd(paste(baseDirectory, "S_cerevisiae", sep=""))


## read the protein data
# scer_proteome <- createPyTable("../code/fasta2table.py orf_trans.fasta", stringsAsFactors=FALSE, col.names=c("GeneDesc","Sequence"))
scer_proteome <- readFastaIntoDF("orf_trans.fasta")
scer_proteome <- transform(scer_proteome, GeneDesc = str_split_fixed(GeneDesc, " ", 2)[,1])
# write.fasta(Scer_proteome$GeneDesc, Scer_proteome$Sequence, "scer_proteome.fasta") ## write it to disk for cdd analysis
arrange(as.data.frame(table(scer_proteome$GeneDesc)), Freq)


## Build the HMM
# scer_bcknd_freqs <- createPyTable("../code/count_frequencies.py orf_trans.fasta", stringsAsFactors=FALSE, col.names=c("amacid","frequency"))
scer_bcknd_freqs <- calcASFreqDist(scer_proteome$Sequence)
scer_state_model <- merge(prion_model, scer_bcknd_freqs, by="amacid")
prion_s_cer_hmm <- createPrionHMM(scer_state_model)

ggplot(rename(melt(scer_state_model), c(variable="state")), aes(amacid, value, fill=state)) + geom_bar(stat="identity", position="dodge") + opts(title="S. Cerivisae HMM state opdfs")


## Run the HMM predcition of domains
scer_proteome_path <- adply(scer_proteome, 1, splat(runHMM), hmm=prion_s_cer_hmm, .progress="text")
save(scer_proteome_path, file="scer_proteome_path.RData")
# print(load("scer_proteome_path.RData"))

scer_proteins_with_pdomain <- subset(scer_proteome_path, str_detect(Segmentation, "P{60,}"))
write.csv(scer_proteins_with_pdomain, file="scer_proteins_with_pdomain.csv")


### overlap our results with the results from the paper
paper_prots_with_pdomain <- read.csv(file = "../Original_paper/Oliver's list new version1.csv", header = TRUE, stringsAsFactors = FALSE)
paper_prots_with_pdomain <- paper_prots_with_pdomain[1:179,]
paper_prots_with_pdomain <- subset(paper_prots_with_pdomain, CORE=as.numeric(CORE), select=c(geneID, geneName, CORE))## keep just the important columns 

scer_proteins_with_pdomain <- transform(scer_proteins_with_pdomain, locus_tag=str_split_fixed(GeneDesc, "_", 2)[,1], gene_symbol=str_split_fixed(GeneDesc, "_", 2)[,2])


#### is there any overlap between disc and purp?

## overlap by gene symbol
protein_overlap <- merge(scer_proteins_with_pdomain, paper_prots_with_pdomain, by.x="gene_symbol", by.y="geneName", all=TRUE)
protein_overlap <- subset(protein_overlap, select=-c(Sequence, Segmentation))
notMappedRev <-  subset(protein_overlap, !is.na(GeneDesc) & is.na(CORE));
subset(notMappedRev[order(notMappedRev$gene_symbol),],select=c(gene_symbol, locus_tag))
notMappedPaper <-  subset(protein_overlap, is.na(GeneDesc) & !is.na(CORE));  
subset(notMappedPaper[order(notMappedPaper$gene_symbol),], select=c(gene_symbol, geneID))

scer_hits_combined <- list(new=scer_proteins_with_pdomain$gene_symbol, paper=paper_prots_with_pdomain$geneName)
# dictHits <- dlply(dict_proteins_with_pdomain, .(species), function(x) x$GeneDesc)
vPlot <- venn.diagram(x = scer_hits_combined, filename = NULL, fill = c("cornflowerblue", "darkorchid1"))#
grid.newpage(); grid.draw(vPlot)

## plot the correlation between the scores
ggplot(protein_overlap[complete.cases(protein_overlap),], aes(as.numeric(maxRollPScore), CORE)) + geom_point() + opts(title="ratio score correlation")
with(protein_overlap[complete.cases(protein_overlap),], Kendall(as.numeric(maxRollPScore), CORE))

## overlap by locus tag
protein_overlap <- merge(scer_proteins_with_pdomain, paper_prots_with_pdomain, by.x="locus_tag", by.y="geneID", all=TRUE)
# protein_overlap <- subset(protein_overlap, select=-c(Sequence, Segmentation))
notMappedRev <-  subset(protein_overlap, !is.na(GeneDesc) & is.na(CORE));
subset(notMappedRev[order(notMappedRev$gene_symbol),],select=c(locus_tag, gene_symbol))
notMappedPaper <-  subset(protein_overlap, is.na(GeneDesc) & !is.na(CORE));  
subset(notMappedPaper[order(notMappedPaper$gene_symbol),], select=c(locus_tag, geneName))

scer_hits_combined <- list(new=scer_proteins_with_pdomain$locus_tag, paper=paper_prots_with_pdomain$geneID)
# dictHits <- dlply(dict_proteins_with_pdomain, .(species), function(x) x$GeneDesc)
vPlot <- venn.diagram(x = scer_hits_combined, filename = NULL, fill = c("cornflowerblue", "darkorchid1"))#
grid.newpage(); grid.draw(vPlot)


#####################################################################
### Dictyostelium discoideum
#####################################################################

setwd(paste(baseDirectory, "Dicty/discoideum", sep=""))

## load the complete protoeme
# dict_disc_proteome <- createPyTable("../../code/fasta2table.py dicty_primary_protein_created_07-05-2011.fasta", stringsAsFactors=FALSE, col.names=c("GeneDesc","Sequence"))
dict_disc_proteome <- readFastaIntoDF("dicty_primary_protein_created_07-05-2011.fasta")

## fix the ids
dict_disc_proteome <- transform(dict_disc_proteome, GeneDesc = str_split_fixed(GeneDesc, "[|]", 3)[,2])
dict_disc_proteome <- transform(dict_disc_proteome, GeneDesc=str_trim(GeneDesc))
# arrange(as.data.frame(table(dict_disc_proteome$GeneDesc)), Freq)


## remove the shorter transcripts for the ~30 genes with multiple isoforms
selectTranscripts <- function(gene){
	if(nrow(gene)==1){ return(gene) }
	
	seqLens <- nchar(gene$Sequence)
	
	# DEBUG  gene <- subset(dict_disc_proteome, GeneDesc=="DDB_G0293378")
	if(gene$GeneDesc[1]=="DDB_G0293378"){
		return(gene[which(seqLens == min(seqLens)),])
	}
	
	# return the longer transcript as default 
	return(gene[which(seqLens == max(seqLens)),])
}
dict_disc_proteome <- ddply(dict_disc_proteome, .(GeneDesc), selectTranscripts, .progress="text")

# write.fasta(dict_disc_proteome$GeneDesc, dict_disc_proteome$Sequence, "dict_disc_proteome.fasta") ## dump proteome with fixed ids for later cdd analysis


# Build the HMM
# ddisc_bcknd_freqs <- createPyTable("../../code/count_frequencies.py dicty_primary_protein_created_07-05-2011.fasta", stringsAsFactors=FALSE, col.names=c("amacid","frequency"))
ddisc_bcknd_freqs <- calcASFreqDist(dict_disc_proteome$Sequence)


# ##### build a custom prion model
# seqs_for_custom_prion_model <- readFastaIntoDF("N-richPrD2.txt")
# prion_model_new <- calcASFreqDist(seqs_for_custom_prion_model$Sequence)
# 
# prion_model= data.frame(amacid=c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y"),
# prion_frequency=c(2.868827, 1.371914, 2.748457, 1.498457, 2.248457, 6.861111, 1.998457, 2.373457, 2.121914, 1.496914, 4.123457, 37.956790, 3.496914, 5.240741, 2.745370, 8.612654, 3.118827, 2.123457, 1.373457, 5.620370))
# 
# 	
# ## change the working directory to avoid that we override existing results
# newResultDirectory <- "newmodel3"
# dir.create(newResultDirectory)
# setwd(newResultDirectory)
# 
# #seqs_for_custom_prion_model <- readFastaIntoDF("/Users/malinovs/Desktop/N-richPrD.txt")
# #prion_model_new <- calcASFreqDist(seqs_for_custom_prion_model$Sequence)
# #
# ### change the working directory to avoid that we override existing results
# newResultDirectory <- "newmodel"
# dir.create(newResultDirectory)
# setwd(newResultDirectory)
# ######

ddisc_state_model <- merge(prion_model, ddisc_bcknd_freqs, by="amacid")
ddisc_state_model <- rbind(ddisc_state_model, dict_specific_amacids)
prion_d_disc_hmm <- createPrionHMM(ddisc_state_model)


# visualize the state opdfs
ggplot(rename(melt(ddisc_state_model), c(variable="state")), aes(amacid, value, fill=state)) + geom_bar(stat="identity", position="dodge") + opts(title="Dictyostelium discoideum HMM state opdfs")
ggsave("dict_disc_hmm.png")


## run the hmm analysis
dict_disc_paths <- adply(dict_disc_proteome, 1, splat(runHMM), hmm=prion_d_disc_hmm, .progress="text")
save(dict_disc_paths, file="dict_disc_paths.RData")

##if using prion_model_new :)
# dict_disc_new_paths <- adply(dict_disc_proteome, 1, splat(runHMM), hmm=prion_model_new, .progress="text")
# save(dict_disc_new_paths, file="dict_disc_new_paths.RData")


# # expand the gene description column
# dict_disc_paths <- transform(dict_disc_paths, dicty_id = str_split_fixed(GeneDesc, "[|]",3)[,1], dicty_gene_symbol = str_split_fixed(GeneDesc, "[|]",3)[,2])
	
## filter for proteins which have at least 60 consecutive Ps
dict_disc_proteins_with_pdomain <- subset(dict_disc_paths, str_detect(Segmentation, "P{60,}"))
write.csv(dict_disc_proteins_with_pdomain, file="dict_disc_proteins_with_pdomain.csv")


# ## extend the table by adding available gene information from ncbi
# idFile=tempfile()
# write.table(str_split_fixed(dict_disc_proteins_with_pdomain$GeneDesc, "[|]",3)[1:100,2], file=idFile, row.names=FALSE, col.names=FALSE)
# ddic_mapped_ids <- createPyTable(paste("../../code/map_dictybase_ids.py ", idFile), stringsAsFactors=FALSE, sep = "\t")
# unlink(idFile)
# save(ddic_mapped_ids, file="ddic_mapped_ids.RData")


# ## xout prion domains and write a fasta file
# dict_disc_proteins_with_xout_pdomain <- xout_pdomains(dict_disc_proteins_with_pdomain)
# # dd_prot_chunks <- split(dict_disc_proteins_with_xout_pdomain, factor(sort(floor(1:nrow(dict_disc_proteins_with_xout_pdomain)/400)))
# xout_seqs <- AAStringSet(dict_disc_proteins_with_xout_pdomain$prion_xout_sequence)
# names(xout_seqs) <- dict_disc_proteins_with_xout_pdomain$dicty_gene_symbol;
# write.XStringSet(xout_seqs, file=paste("xout_p_domains_all.fasta", sep=""), format="fasta", width=80)
 


#####################################################################
### Dictyostelium purpureum
#####################################################################

setwd(paste(baseDirectory, "Dicty/purpureum", sep=""))


## load the proteome
# dict_purp_proteome <- createPyTable("../../code/fasta2table.py purpureum_protein_created_02-04-2010.fas", stringsAsFactors=FALSE, col.names=c("GeneDesc","Sequence"))
dict_purp_proteome <- readFastaIntoDF("purpureum_protein_created_02-04-2010.fas")
dict_purp_proteome <- transform(dict_purp_proteome, GeneDesc = str_split_fixed(GeneDesc, "[|]", 3)[,2])
dict_purp_proteome <- transform(dict_purp_proteome, GeneDesc=str_trim(GeneDesc))
# write.fasta(dict_purp_proteome$GeneDesc, dict_purp_proteome$Sequence, "dict_purp_proteome.fasta") ## dump proteome with fixed ids for later cdd analysis
arrange(as.data.frame(table(dict_purp_proteome$GeneDesc)), Freq)


## estimate the background amino acid distribution and create the hmm
# dpurp_bcknd_freqs <- createPyTable("../../code/count_frequencies.py purpureum_protein_created_02-04-2010.fas", stringsAsFactors=FALSE, col.names=c("amacid","frequency"))
dpurp_bcknd_freqs <- calcASFreqDist(dict_purp_proteome$Sequence)

dpurp_state_model <- merge(prion_model_new, dpurp_bcknd_freqs, by="amacid")
dpurp_state_model <- rbind(dpurp_state_model, dict_specific_amacids)
prion_d_purp_hmm <- createPrionHMM(dpurp_state_model)

# visualize the state opdfs
ggplot(rename(melt(dpurp_state_model), c(variable="state")), aes(amacid, value, fill=state)) + geom_bar(stat="identity", position="dodge") + opts(title="Dictyostelium purpureum HMM state opdfs")


# tt <- subset(dict_purp_proteome, GeneDesc=="DPU0053091|DPU_G0053090_|Protein|gene:")

## run the analysis (paths + scores)
dict_purp_paths  <- adply(dict_purp_proteome, 1, splat(runHMM), hmm=prion_d_purp_hmm, .progress="text")
save(dict_purp_paths, file="dict_purp_paths.RData")

## filter for proteins which have at least 60 consecutive Ps
purp_disc_proteins_with_pdomain <- subset(purp_disc_paths, str_detect(Segmentation, "P{60,}"))
write.csv(purp_disc_proteins_with_pdomain, file="purp_disc_proteins_with_pdomain.csv")


#####################################################################
### Postprocess all HMM results together
#####################################################################

setwd(baseDirectory)

print(load("Dicty/purpureum/dict_purp_paths.RData"))
print(load("Dicty/discoideum/dict_disc_paths.RData"))
print(load("S_cerevisiae/scer_proteome_path.RData"))
# print(load("Dicty/discoideum/newmodel3/dict_disc_new_paths.RData"))

setwd("results")

# # prepare proper gene ids
# dict_purp_paths <- transform(dict_purp_paths, gene_id=str_split_fixed(GeneDesc, "[|]", 3)[,2])
# dict_purp_paths <- transform(dict_purp_paths, gene_id=substr(gene_id, 1, nchar(gene_id)-1))
# dict_disc_paths <- transform(dict_disc_paths, gene_id=str_split_fixed(GeneDesc, "[|]", 3)[,2])
# dict_disc_paths <- transform(dict_disc_paths, gene_id=substr(gene_id, 1, nchar(gene_id)-1))
# scer_proteome_path <- transform(scer_proteome_path, gene_id=str_split_fixed(GeneDesc, "_",2)[,1])

##comparing prion_model and prion_model_new in D.discoideum
all_species_paths <- rbind(transform(dict_disc_paths, species="d. discoideum"),transform(dict_purp_paths, species="d. purpureum"), transform(scer_proteome_path, species="s. cerivisae"))

##for newmodel
#all_species_paths <- rbind(transform(dict_disc_new_paths, species="d. discoideum N-rich"), transform(dict_disc_paths, species="d. discoideum"),transform(dict_purp_paths, species="d. purpureum"), transform(scer_proteome_path, species="s. cerivisae"))

table(all_species_paths$species)

# visualize the length distributions of all 3 proteomes
all_species_paths <- transform(all_species_paths, prot_length=nchar(Sequence))
ggplot(subset(all_species_paths, prot_length< 5000), aes(prot_length, fill=str_detect(Segmentation, "P{60,}"))) + geom_histogram(binwidth=50) + facet_wrap(~species, nrow=3) + opts(title="Protein length distributions")
ggsave("Protein length distributions.png")

## save the merged data-set (protein sequences and classification for all species)
save(all_species_paths, file=".all_species_paths.RData")


## filter for proteins which have at least 60 consecutive Ps
all_species_proteins_with_pdomain <- subset(all_species_paths, str_detect(Segmentation, "P{60,}"))
table(all_species_proteins_with_pdomain$species)
write.csv(all_species_proteins_with_pdomain, file="all_species_proteins_with_pdomain.tsv", row.names=FALSE)
## write fasta for later geneious import
write.fasta(all_species_proteins_with_pdomain$GeneDesc, all_species_proteins_with_pdomain$Sequence, "all_species_proteins_with_pdomain.fasta") 


## visualize the score distribution
ggplot(all_species_proteins_with_pdomain, aes(as.numeric(maxRollPScore))) + geom_histogram() + facet_wrap(~species, scales="free_y", nrow=3) +  opts(title="Maximum moving average score distribution (w=60) of proteins with at least one prion domain")
ggsave("hmm score distributions of prion domain proteins.png")
	
## count and visualize how many domains have been predicted per gene
prots_with_pdomsegs <- extractDomainSegs(all_species_proteins_with_pdomain)

## add the domain sequnece
prots_with_pdomsegs <- transform(prots_with_pdomsegs, domain_seq=substr(Sequence, start, end))
table(as.data.frame(table(prots_with_pdomsegs$GeneDesc))$Freq)
ggplot(ddply(prots_with_pdomsegs, .(GeneDesc, species), summarize, num_domains=length(GeneDesc)), aes(factor(num_domains))) + geom_bar() + facet_wrap(~species, ncol=4, scales="free") +  scale_y_continuous(name="# genes") + opts(title="Prion domain counts per gene")
ggsave("prion domains counts per gene.png")

## get the folding probability from foldindex for each domain
# foldIndexDoc <- xmlTreeParse("http://bioportal.weizmann.ac.il/fldbin/findex?m=xml&sq=SFALYVSAFALGFPLNLLAIRGAVSHAKLRLTPSL", getDTD = F, isURL=TRUE)

library(XML)
prots_with_pdomsegs <- adply(prots_with_pdomsegs, 1, splat(function(domain_seq, ...){
	foldIndexDoc <- xmlTreeParse(paste("http://bioportal.weizmann.ac.il/fldbin/findex?m=xml&sq=", domain_seq,sep=""), getDTD = F, isURL=TRUE)
	return( c(foldIndex=as.numeric(xmlValue(foldIndexDoc[[1]][[1]]))))
}), .progress="text")

write.tsv(prots_with_pdomsegs, file="predicted_prion_domains_all_species.tsv")
# prots_with_pdomsegs <- read.table("predicted_prion_domains_all_species.tsv", header=TRUE)

prots_with_pdomsegs_unfolded <- subset(prots_with_pdomsegs, foldIndex <0)
ggplot(prots_with_pdomsegs, aes(as.numeric(foldIndex))) + geom_histogram() + facet_wrap(~species, ncol=3)
ggsave("fold index distribution per species.png")

#####################################################################
### Domain prediction and enrichment analysis
#####################################################################


setwd(paste(baseDirectory, "results", sep=""))


domains_dpurp <- read.csv(paste(baseDirectory, "Dicty/purpureum/cdd_hitdata_concise_dpurp.txt", sep=""), sep = '\t', fill=TRUE, header=TRUE, skip=7)
domains_ddict <- read.csv(paste(baseDirectory, "Dicty/discoideum/cdd_hitdata_concise_ddict.txt", sep=""), sep = '\t', fill=TRUE, header=TRUE, skip=7)
domains_scerv <- read.csv(paste(baseDirectory, "S_cerevisiae/cdd_hitdata_concise_scerv.txt", sep=""), sep = '\t', fill=TRUE, header=TRUE, skip=7)

## combine the data sets and extract the ids
domains_all <- rbind(transform(domains_scerv, species="s. cerivisae"), transform(domains_ddict, species="d. discoideum"), transform(domains_dpurp, species="d. purpureum"))
##for newmodel
#domains_all <- rbind(transform(domains_scerv, species="s. cerivisae"), transform(domains_ddict, species="d. discoideum"),transform(domains_ddict, species="d. discoideum N-rich"), transform(domains_dpurp, species="d. purpureum"))

domains_all <- transform(domains_all, gene_id=str_match(Query, ".*>(.*)")[,2])

## load all protoemes and the sets of proteins for each organism that have at least one prion domain
print(load(".all_species_paths.RData"))
all_species_proteins_with_pdomain <- read.csv( file="all_species_proteins_with_pdomain.tsv")


## keep just the super families but remove the word superfamiliy from the (short) name
domains_all_sf <- subset(domains_all, Hit.type=="superfamily")
domains_all_sf <- transform(domains_all_sf, Short.name = str_trim(str_replace(Short.name, "superfamily", "")))

write.tsv(domains_all_sf, file="cdd_superfamily_domains_all_species.csv")


## prepare the enrichment analysis by counting proteins in total and in hitlist	
proteome_counts <- rename(as.data.frame(with(all_species_paths, table(species))), c(Freq="num_proteins"))
domain_counts <- rename(as.data.frame(with(domains_all_sf, table(species))), c(Freq="num_domains")) 
norm_domain_counts <- transform(merge(proteome_counts, domain_counts, by="species"), doms_per_prot = num_domains/ num_proteins)
ggplot(domains_all_sf, aes(species)) + geom_bar() + opts(title="Numbers of predicted domains per organism")
ggsave("enrichment analysis - Numbers of predicted domains per organism.png")
ggplot(norm_domain_counts, aes(species, doms_per_prot)) + geom_bar(stat="identity") + opts(title="Normalized number of domains per protein in each organsim")
ggsave("enrichment analysis - Normalized number of domains per protein in each organsim.png")


## create a tag cloud of frequent domains
ccd_mult_counts <- table(subset(domains_all_sf)$Short.name)
ccd_mult_counts <- log(ccd_mult_counts[ccd_mult_counts>25]) 
cloud(ccd_mult_counts, col = col.br(ccd_mult_counts, fit=TRUE))


## collect the different statistics necessary to do the enrichment analysis
familiy_counts <- ddply(domains_all_sf, .(species, Short.name), summarize, num_domains=length(Short.name), .progress="text")
familiy_total_counts <- ddply(domains_all_sf, .(species), summarize, tot_num_domains=length(species), .progress="text")


all_priprots_with_domains <- merge(all_species_proteins_with_pdomain, domains_all_sf, by.x=c("GeneDesc", "species"), by.y=c("gene_id", "species"))
all_priprots_domain_counts <- ddply(all_priprots_with_domains, .(species, Short.name), summarize, num_domains=length(Short.name), .progress="text")
all_priprots_total_counts <- ddply(all_priprots_with_domains, .(species), summarize, tot_num_domains=length(species), .progress="text")

## prepare the data for the enrichemn
urn <- merge(familiy_counts, familiy_total_counts)
drawn_sample <- merge(all_priprots_domain_counts, all_priprots_total_counts)
urn_model <- merge(urn, drawn_sample, suffixes = c("_proteome","_prionprots"), by=c("species", "Short.name"))

# Note: why q-1, because we want to have Pr(X>=x) instead of Pr(X>x)
calcDomainEnrichemnt <- function(num_domains_proteome, tot_num_domains_proteome, num_domains_prionprots, tot_num_domains_prionprots, ...)
 		phyper(num_domains_prionprots-1, num_domains_proteome, tot_num_domains_proteome-num_domains_proteome, tot_num_domains_prionprots, lower.tail=FALSE)

domain_enrichment <- rename(adply(urn_model, 1,  splat(calcDomainEnrichemnt)), c(V1="p_value"))
write.csv(domain_enrichment, file="domain_super_familiy_enrichment.csv")
# domain_enrichment = read.csv("results/domain_super_familiy_enrichment.csv")

# keep just the overrepresented terms
sig_domain_enrichment <- subset(domain_enrichment, p_value < 0.01)
write.csv(sig_domain_enrichment, file="overrep_domain_super_families_cutoff0.01.csv", row.names=FALSE)

ggplot(sig_domain_enrichment, aes(Short.name, p_value, width=0.3)) + geom_bar(stat="identity") + facet_wrap(~ species, ncol=4, scale="free") + coord_flip() + scale_y_continuous(formatter = "comma", trans="log10")
ggsave("enrichment analysis - enriched superfamilies overview.png", width=11, height=9)

## see if there are any terms shared between the different species
png("enrichment analysis - overlap between enriched superfamilies.png")
enrTermLists <- dlply(sig_domain_enrichment, .(species), function(x) x$Short.name)
# vPlot <- venn.diagram(x = enrTermLists, filename = NULL, fill = c("cornflowerblue", "darkorchid1", "darkgreen", "yellow"))
vPlot <- venn.diagram(x = enrTermLists, filename = NULL, fill = c("cornflowerblue", "darkorchid1", "darkgreen"))
grid.newpage(); grid.draw(vPlot)
dev.off()

## export the domains of an enriched super-family and built a phylogeny in Geneious for it
#commonEnrDomains <- intersect(intersect(enrTermLists[[1]], enrTermLists[[2]]), enrTermLists[[3]])
#subset(sig_domsin_enrichment, Short.name %in% commonEnrDomains)

#protsWithEnrDomain <- subset(all_priprots_with_domains, Short.name=="SH3")
#protsWithEnrDomain <- transform(protsWithEnrDomain, domain=substr(Sequence, From, To))

# add orthologue information
#merge(protsWithEnrDomain, slim_ortho_mapping)

## remove domains that occur more than once in a single domain
#protsWithEnrDomainJustOnce <- subset(protsWithEnrDomain, !duplicated(paste(GeneDesc, Short.name, species, sep="|")))

# oneSupFam <- transform(oneSupFam, tagsequence = as.character(subseq(DNAStringSet(geneseq_strand_corr), start=tag_window_start, end=tag_window_end)))
#with(protsWithEnrDomainJustOnce, write.fasta(paste(GeneDesc, Short.name, species, sep="|"), domain, "results/SH3domains.fasta")) 


#####################################################################
### AS frequency bias analysis for proteins with prion domains
###
### The analysis is done by grouping based on other predicted domains 
### for these proteins and by subsequent selection of overrepresented
### superfamilies in the set of proteins with prion domains
#####################################################################
setwd(baseDirectory)
setwd("results")

## load the results from the last sections 
# print(load("results/.all_species_paths.RData"))
# all_species_proteins_with_pdomain <- read.csv(file="results/all_species_proteins_with_pdomain.tsv")
prots_with_pdomsegs <- read.table("predicted_prion_domains_all_species.tsv", header=TRUE)
# prots_with_pdomsegs <- subset(prots_with_pdomsegs, select=-c(Segmentation, Sequence))
prots_with_pdomsegs <- rename(prots_with_pdomsegs, c(GeneDesc ="gene_id"))


## calculate the amino acid distribution for all domain in each species to get a background distribution
prionBckndASDist <- ddply(prots_with_pdomsegs, .(species), function(x) calcASFreqDist(x$domain_seq))
names(prionBckndASDist) <- paste(names(prionBckndASDist), "bcknd", sep="_")


## pick the most signficant terms in each group
sig_domain_enrichment <- read.csv("overrep_domain_super_families_cutoff0.01.csv")
sig_domain_enrichment <- arrange(sig_domain_enrichment, p_value) 

## filter away domains that don't occur frequently 
sig_domain_enrichment <- subset(sig_domain_enrichment, num_domains_prionprots > 6)
#ggplot(sig_domain_enrichment, aes(num_domains_prionprots)) + geom_histogram(binwidth=1) + facet_wrap(~species, nrow=3)

## pick the top ten superfamilies from each species
top10 <- ddply(sig_domain_enrichment, .(species), function(x)x[1:min(nrow(x),10),])
table(top10$species) ## just to make sure that the subsetinng worked properly
	
shared_terms <- as.character(subset(as.data.frame(table(sig_domain_enrichment$Short.name)), Freq > 2)$Var1)
supfamsOfInterest <- union(shared_terms, top10$Short.name)


## Visualize the bcknd-frequency distribution
ggplot(prionBckndASDist, aes(amacid_bcknd, frequency_bcknd, fill=species_bcknd)) + geom_bar(stat="identity", position="dodge") + opts(title="Background AS frequencies per species")
ggsave("as freq bias - Background AS frequencies per species.png")

## just keep the domain with superfamilies of interest in the same protein
domains_all_sf <- read.table(file="cdd_superfamily_domains_all_species.csv", header=TRUE)


# ## Options 1: keep all the terms for ALL species
# domains_of_interest <- subset(domains_all_sf, Short.name %in% supfamsOfInterest)
# domains_of_interest <- subset(domains_all_sf, Short.name %in% shared_terms)

## Options 2: keep only the top 10 terms for all species
domains_of_interest <- merge(domains_all_sf,  top10, by=c("species", "Short.name"))

# export this superfamilies of interest to allow an import into Geneious
write.tsv(domains_of_interest, file="domains_of_interest.csv")
ddply(subset(domains_of_interest, !str_detect(species, "cervi")), .(Short.name), nrow)

## combine superfamily domains and domains
pDomsWithSupDoms <- merge(domains_of_interest, prots_with_pdomsegs, by.x=c("species", "gene_id"))
# pDomsWithSupDoms4Pyhlogeney <- pDomsWithSupDoms
pDomsWithSupDoms <- subset(pDomsWithSupDoms, select=-c(Segmentation, Sequence))


## calculate the AS freq-dists per superfamily and species 
asFreqDists <- ddply(pDomsWithSupDoms, .(species, Short.name), function(x) calcASFreqDist(x$domain_seq), .progress="text")

## normalize them against the bcknd-AS-freq dist per species
asFreqDists <- merge(asFreqDists, prionBckndASDist, by.x=c("species", "amacid"), by.y=c("species_bcknd", "amacid_bcknd"))
asFreqDists <- transform(asFreqDists, norm_frequency=frequency/frequency_bcknd)

## start of categorization of AS into groups
asCategoryMapping <- list(
	positive_charged=c("R","H", "K"), 
	negative_charged=c("D", "E"),
	Serine="S",	Threonine="T", Asparagine="N", Gluatime="Q", 
	Cysteine="C", Selenocysteine="U", Glycine="G", Proline="P",
	hydrophobic=c("A", "V", "I", "L", "M", "F", "Y", "W"))

asCategories <- transform(ldply(asCategoryMapping, function(x)data.frame(amacid=x)), as_type=.id, .id=NULL)

asFreqDists <- merge(asFreqDists, asCategories, by="amacid")
asFreqDists <- ddply(asFreqDists, .(species, as_type, Short.name), transform, norm_frequency=mean(norm_frequency), amacid=as_type)
## end of categorization

ggplot(asFreqDists, aes(amacid, log2(norm_frequency), fill=species)) + geom_bar(stat="identity", position="dodge") + opts(title="AS frequency bias") + facet_wrap(~Short.name, ncol=3) + opts(axis.text.x=theme_text(angle=90))
ggsave("as freq bias.png", width=10, height=9)




##################################################################################################
# prepare the list of protein for which we want to align the pictorgrams

# protsOfInterest <- subset(pDomsWithSupDoms, Short.name %in% c("PKc_like", "RRM") & !str_detect(species, "cerivisae"))
protsOfInterest <- subset(pDomsWithSupDoms, !str_detect(species, "cerivisae"))

protsOfInterest <- subset(protsOfInterest, select=c(species, gene_id))
protsOfInterest <- subset(protsOfInterest, !duplicated(gene_id))

dicty_orthologues <- read.csv("../Dicty/dicty_orthologues.csv")

ddisc_mapped  <-merge(subset(protsOfInterest, species=="d. discoideum"), dicty_orthologues, by.x="gene_id", by.y="ddisc_gene_id", all.x=TRUE) 
dpurp_mapped  <-merge(subset(protsOfInterest, species=="d. purpureum"), dicty_orthologues, by.x="gene_id", by.y="dpurp_gene_id", all.x=TRUE) 
dpurp_mapped <- dpurp_mapped[,ncol(dpurp_mapped):1]

names(dpurp_mapped)[3] <- "dpurp_gene_id"
names(ddisc_mapped)[1] <- "ddisc_gene_id"

orthologuesEnrSF <- rbind(dpurp_mapped, ddisc_mapped)
orthologuesEnrSF <- subset(orthologuesEnrSF, !duplicated(paste(ddisc_gene_id, dpurp_gene_id)))

## todo put into slide
table(complete.cases(orthologuesEnrSF))

orthologuesEnrSF <- orthologuesEnrSF[complete.cases(orthologuesEnrSF),]
# export the sequences of both sets into a fasta file
print(load(".all_species_paths.RData"))


with(subset(all_species_paths, !duplicated(GeneDesc) & (GeneDesc %in% as.character(orthologuesEnrSF$ddisc_gene_id))), write.fasta(GeneDesc, Sequence, file="domain_orths_ddisc.fasta"))
with(subset(all_species_paths, !duplicated(GeneDesc) & (GeneDesc %in% as.character(orthologuesEnrSF$dpurp_gene_id))), write.fasta(GeneDesc, Sequence, file="domain_orths_dpurp.fasta"))

## without the duplicate removal hack# 
# with(subset(all_species_paths, GeneDesc %in% as.character(orthologuesEnrSF$ddisc_gene_id)), write.fasta(GeneDesc, Sequence, file="domain_orths_ddisc.fasta"))
# with(subset(all_species_paths, GeneDesc %in% as.character(orthologuesEnrSF$dpurp_gene_id)), write.fasta(GeneDesc, Sequence, file="domain_orths_dpurp.fasta"))


write.tsv(transform(orthologuesEnrSF, orth_index=1:nrow(orthologuesEnrSF)), file="domain_structure_orth_mapping.csv")


## Next in geneious
# 1) import both lists into a sequence list
# 2) annotate segments from file /Volumes/bioinformatics/projects/alberti/results/predicted_prion_domains_all_species.tsv
# 3) annoate enriched superfamily domains of interest from /Volumes/bioinformatics/projects/alberti/results/domains_of_interest.csv
# 4) import the orthologue mapping from domain_structure_orth_mapping.csv using the meta-data importer into both sequence listsdomains_of_interest.csv

subset(orthologuesEnrSF, dpurp_gene_id=="DPU_G0074906")




# ############ 
# #### extract the data that is necessary to build the pyhlogeny
# 
# protsOfInterest <- subset(pDomsWithSupDoms4Pyhlogeney, Short.name %in% c("PKc_like", "RRM"))

# protsOfInterest <- subset(protsOfInterest, select=-c(Segmentation, Query, Hit.type, PSSM.ID, Bitscore))
# 
# 
# # remove duplicates because of multiple predictions of the same domain in a sinlge protein
# protsOfInterest <- subset(protsOfInterest, !duplicated(paste(species, gene_id, Short.name, domain_seq)))
# 
# # even after removing duplicates of prion domains we might have several superfamiliy domains of the same type in a single protein, e.g.
# arrange(as.data.frame(table(as.character(protsOfInterest$gene_id))), Freq)
# 
# subset(protsOfInterest2, gene_id=="DDB_G0274523")
# 
# ## chop away the prion domains
# tail(arrange(as.data.frame(with(protsOfInterest, table(gene_id, From))), Freq))
# 
# prionsProtsWithoutPrions <- ddply(protsOfInterest, .(gene_id), function(oneProt){
# 	# print(paste("chopping gene with id", oneProt$gene_id[1]))
# 	# oneProt = subset(protsOfInterest, gene_id=="DDB_G0282627")
# 	protein = oneProt$Sequence[1]
# 	
# 	for(prion_domain_seq in oneProt$domain_seq){
# 		# print(domain)
# 		protein <- str_replace(protein, prion_domain_seq, "")
# 	}
# 	
# 	return(with(oneProt, data.frame(gene_id=gene_id[1], species=species[1], Short.name=Short.name[1], prionfree_protein=protein)))
# 	
# }, .progress="text")
# 
# ddply(prionsProtsWithoutPrions, .(gene_id), nrow)
# 
# ## write one fasta file per term
# d_ply(prionsProtsWithoutPrions, .(Short.name), function(df){
# 	
# 	with(df, write.fasta(gene_id, prionfree_protein, file=paste(Short.name[1], ".fasta", sep="")))
# }, .progress="text")

## save the meta data-table to be imported into Geneious
# write.tsv(subset(prionsProtsWithoutPrions, select=c(, file="prionsProtsWithoutPrions.tsv")


##############################
## Create aligned pictorgrams of dicty proteins and their orthologues


#####################################################################
### Orthologue analysis
#####################################################################

setwd(baseDirectory)

jgi2dpu_mapping <- read.table("Dicty/purpureum/GeneID-JGI.txt", header=TRUE)
orthologues <- read.csv("Dicty/purpureum/orthologues_with_ddict.csv", header=TRUE)
names(orthologues) <- c("source", "JGI", "dictybase_id", "gene_name", "description")
orthologues_mapping <- merge(orthologues, jgi2dpu_mapping)

slim_ortho_mapping <- subset(orthologues_mapping, select=c(GeneID, dictybase_id))
slim_ortho_mapping <- rename(slim_ortho_mapping, c(GeneID="dpurp_gene_id", dictybase_id="ddisc_gene_id"))
write.csv(slim_ortho_mapping, "Dicty/dicty_orthologues.csv", row.names=FALSE)

all_species_proteins_with_pdomain <- read.csv("results/all_species_proteins_with_pdomain.csv")

## add the orthologue info
prots_with_pdom_purp <- subset(all_species_proteins_with_pdomain, str_detect(species, "purp")); nrow(prots_with_pdom_purp)
prots_with_pdom_purp <- merge(prots_with_pdom_purp, slim_ortho_mapping, by.x="GeneDesc", by.y="dpurp_gene_id"); nrow(prots_with_pdom_purp)

# constrain ddict also to just those genes for which there are orthologues
prots_with_pdom_ddisc <- subset(all_species_proteins_with_pdomain, str_detect(species, "disc")); nrow(prots_with_pdom_ddisc)
prots_with_pdom_ddisc <- subset(prots_with_pdom_ddisc, GeneDesc %in% slim_ortho_mapping$ddisc_gene_id); nrow(prots_with_pdom_ddisc)

# ddply(prots_with_pdom_ddisc, .(GeneDesc), subset, length(GeneDesc)>1)

## is there any overlap between discoideum and purpureum?
gene_with_poverlap <- list(ddisc=prots_with_pdom_ddisc$GeneDesc, dpurp=prots_with_pdom_purp$dictybase_id)
vPlot <- venn.diagram(x = gene_with_poverlap, filename = NULL, fill = c("cornflowerblue", "darkorchid1"))#
grid.newpage(); grid.draw(vPlot)
protein_overlap <- merge(prots_with_pdom_ddisc, rename(prots_with_pdom_purp, c(GeneDesc="GeneDesc_dpurp")), by.x="GeneDesc", by.y="dictybase_id", suffixes = c("_ddisc","_dpurp"))
protein_overlap_noseqs <- subset(protein_overlap, select=!str_detect(colnames(protein_overlap), "Segmentation|Sequence|species"))
write.csv(protein_overlap_noseqs , file="results/disc_dpurp_ortho_overlap.csv", row.names=FALSE)

# protein_overlap[3,]
# subset(extractDomainSegs(prots_with_pdom_ddisc), gene_id=="DDB_G0267422")
# subset(extractDomainSegs(prots_with_pdom_purp), dictybase_id=="DDB_G0267422")


