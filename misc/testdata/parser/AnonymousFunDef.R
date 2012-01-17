for(curTagType in tagTypes ){




calcConsensus <- function(tagWindows, struct_col="simple_sec_struct"){
	allexExpandStructs <- ddply(tagWindows, .(experiment), function(subTagWindows){
		# browser()
		secStructs <- BStringSet(eval(parse(text=paste("subTagWindows$", struct_col, sep="")))) # Sample set of DNA fragments.
		pwm <- consensusMatrix(secStructs, as.prob=TRUE)

		expandStructs <-as.data.frame(pwm)
		expandStructs$struct_element <- rownames(expandStructs)

		## Do the actual reshaping
		expandStructs <- melt(expandStructs)
		expandStructs <- transform(expandStructs, win_pos = as.numeric(str_replace(variable, "V","")))
		expandStructs$variable=NULL

		expandStructs$experiment=subTagWindows$experiment[1]
		expandStructs$clip_replicate=subTagWindows$clip_replicate[1]
		expandStructs$clip_protein=subTagWindows$clip_protein[1]

		return(expandStructs)
	})

	return(allexExpandStructs);
}



}
 # end for-loop
