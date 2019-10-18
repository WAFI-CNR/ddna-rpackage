# global reference to scipy (will be initialized in .onLoad)
ddna <- NULL


.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This package needs Python3.x in order to work")
  library(TraMineR)
  library(ggplot2)
  library(gtable)
  library(scales)
  library(grid)
  library(tools)
  #python.path <- choose.files(caption = "Select Python 3.7 exe/bin")
  os.info <- Sys.info()

  if(os.info['sysname']=="Windows"){
    python.path <- file.choose()
    reticulate::use_python(python.path, required = T)
  }
}

.onLoad <- function(libname, pkgname) {

  # use superassignment to update global reference to ddna
  ddna <<- reticulate::import("digitaldna", delay_load = TRUE)
}

install_ddna <- function(method = "auto", conda = "auto") {
  reticulate::py_install("digitaldna", method = method, conda = conda)
}

# definisco funzioni di utilità
vectorize = function(seq) {
  seqvector <- rep(NA, length(seq))
  for (i in 1:length(seq)) {
    seqvector[i] <- seq[i]
  }
  return(seqvector)
}

longest_common_subsequence <- function(df, overwrite_flag = F, threshold='auto', window=10){
  filename1 <- paste0(getwd(), "/glcr_cache", collapse = NULL)
  lcs <- ddna$LongestCommonSubsequence(out_path = filename1, overwrite=overwrite_flag, threshold=threshold, window=as.integer(window))
  lcs$fit_predict(X=df$dna)
  return(lcs)

}

#' Title
#'
#' @param input_file
#' @param alphabet
#'
#' @return stt
#' @export
#'
#' @examples
#'
sequence_tweets_from_file <- function(input_file, alphabet="b3_type"){
  ddna <- reticulate::import("digitaldna")
  dnasequencer <- ddna$TwitterDDNASequencer(input_file = input_file, alphabet = alphabet )
  result <- dnasequencer$fit_transform()
  flat_result <- array(result) # flat matrix, chr [1:12(1d)]  - odd indexes contains user_id, even ones dnasequence
  df_result <- data.frame(user_id = result[seq(1,length(result),2)], seq = result[seq(2,length(result),2)]) # create a df
  return(df_result)
}



#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
setup_df <- function(df, bases, dnacol = 2){
  seq <- seqdef(df, var = dnacol, format = "STS", stsep = "")

  bases <- alphabet(seq)
  labels <- stlab(seq)
  ids <- rownames(seq)
  num_seq <- nrow(seq)
  brackets <- rep(")", length(bases))
  full_labels <- paste(bases, " (", labels, ")", sep = "")

  seqwide <- unname(apply(seq, 2, vectorize))
  dummyrows <- unname(t(sapply(c(bases, "%"), function(x) rep(x, ncol(seqwide)))))
  seqwide <- as.data.frame(rbind(seqwide, dummyrows))
  seqwide <- seqwide[-c((nrow(seqwide) - length(bases)):nrow(seqwide)),]
  seqwide <- cbind(seqid = rownames(seqwide), seqwide)
  return(seqwide)
}

# calcola la probabilità empirica delle varie basi in una sequenza
#' Title
#'
#' @param seq
#' @param seqalphabet
#'
#' @return
#' @export
#'
#' @examples
seqprob = function(seq, seqalphabet) {
	seq <- unname(unlist(seq))
	# elimino le wildcards dalla sequenza
	if (length(which(seq == '%')) > 0) seq <- seq[-which(seq == '%')]
	seq <- factor(seq, levels = seqalphabet)
	# calcolo le probabilità
	return(table(seq) / length(seq))
}

# calcola l'entropia di Shannon dato un vettore di probabilità
#' Title
#'
#' @param seqpdf
#'
#' @return
#' @export
#'
#' @examples
seqentropy = function(seqpdf) {
	seqpdf <- unname(seqpdf)
	if (length(which(seqpdf == 0)) > 0) seqpdf <- seqpdf[-which(seqpdf == 0)]
	return(-sum(seqpdf * log2(seqpdf)))
}

# Execution example

#choose.files()
#library(rstudioapi)
#file_path <- selectFile(caption = "Select Python 3.7 exe", label = "Select", path = NULL,
#                        filter = NULL, existing = TRUE)

#use_python("C:/Program Files/Python37/python.exe", required = T)


# df <- sequence_tweets_from_file("tests/timelines.json")
# or...
# df1 <- read.csv2("tests/italian_retweets_users_sequences_new.csv", sep = ",")
# plot_interseq(df)
# # plot_intraseq(df)
# # plot_bases_distribution(df)
# plot_sequences_by_color(df)
# lcs_plot(df)
#
#
# py_versions_windows()
# py_discover_config()

