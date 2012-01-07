xout_pdomains <- function(df_with_segs_and_seq){
	adply(df_with_segs_and_seq, 1, splat(function(Sequence, Segmentation, ...){
		return(c(prion_xout_sequence=paste(split_sequence)))
	}), .progress="text")
}

