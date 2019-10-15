require(TraMineR)
require(ggplot2)
require(scales)
require(grid)

# definisco funzioni di utilità
vectorize = function(seq) {
	seqvector <- rep(NA, length(seq))
	for (i in 1:length(seq)) {
		seqvector[i] <- seq[i]
	}
	return(seqvector)
}

# calcola la probabilità empirica delle varie basi in una sequenza
seqprob = function(seq, seqalphabet) {
	seq <- unname(unlist(seq))
	# elimino le wildcards dalla sequenza
	if (length(which(seq == '%')) > 0) seq <- seq[-which(seq == '%')]
	seq <- factor(seq, levels = seqalphabet)
	# calcolo le probabilità
	return(table(seq) / length(seq))
}

# calcola l'entropia di Shannon dato un vettore di probabilità
seqentropy = function(seqpdf) {
	seqpdf <- unname(seqpdf)
	if (length(which(seqpdf == 0)) > 0) seqpdf <- seqpdf[-which(seqpdf == 0)]
	return(-sum(seqpdf * log2(seqpdf)))
}

# configurazione
groupname <- "Human"
input <- "tweettype_3_human"
bases <- c("A", "C", "T")
labels <- c("tweet", "reply", "retweet")

# carico una sequenza
#rawseq <- read.csv(paste("plots/fake/dna properties/data/sequences/", input, ".csv", sep = ""))
rawseq <- read.csv("R/100_sintetici_genbot.csv")
seq <- seqdef(rawseq, var = 3, format = "STS", stsep = "", alphabet = bases, labels = labels)
seqwide <- unname(apply(seq, 2, vectorize))
dummyrows <- unname(t(sapply(c(bases, "%"), function(x) rep(x, ncol(seqwide)))))
seqwide <- as.data.frame(rbind(seqwide, dummyrows))
seqwide <- seqwide[-c((nrow(seqwide) - length(bases)):nrow(seqwide)),]
seqwide <- cbind(seqid = rownames(seqwide), seqwide)

# calcolo la distribuzione di probabilità delle basi per ogni sequenza
intraseq_prob <- as.data.frame(t(apply(seqwide[,-1], 1, seqprob, bases)))

# calcolo l'entropia (di Shannon) per ogni sequenza (entropia intra-sequenza)
intraseq_entropy <- as.data.frame(apply(intraseq_prob, 1, seqentropy))
colnames(intraseq_entropy) <- "entropy"
mean_intraseq_entropy <- mean(intraseq_entropy$entropy)

# una qualche visualizzazione...
