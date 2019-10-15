require(TraMineR)
require(RColorBrewer)
require(reshape2)
require(ggplot2)
require(grid)
require(scales)
require(tools)

# percorso completo al file RDS con le sequenze
filename <- "args[1]"
plotname <- paste(file_path_sans_ext(file_path_sans_ext(basename(filename))), ".seqplot.png", sep = "")

# carico l'oggetto contenente le sequenze
seq <- readRDS(filename)
# estraggo informazioni sulle sequenze
bases <- alphabet(seq)
labels <- stlab(seq)
ids <- rownames(seq)
num_seq <- nrow(seq)
max_len <- length(seq[1,])
brackets <- rep(")", length(bases))
full_labels <- paste(bases, " (", labels, ")", sep = "")

# genero il data frame delle sequenze
seqwide <- unname(apply(seq, 2, vectorize))
dummyrows <- unname(t(sapply(c(bases, "%"), function(x) rep(x, ncol(seqwide)))))
seqwide <- as.data.frame(rbind(seqwide, dummyrows))
seqwide <- seqwide[-c((nrow(seqwide) - length(bases)):nrow(seqwide)),]
seqwide <- cbind(seqid = seq(from = 1, to = num_seq, by = 1), seqwide[,2:ncol(seqwide)])
# trasformo da formato wide a formato long
seqlong <- melt(seqwide, id.vars = "seqid")
colnames(seqlong)[2:3] <- c("position", "bases")
seqlong <- seqlong[which(seqlong$bases != "%"),]
seqlong[,3] <- as.factor(seqlong[,3])
seqlong <- cbind(seqlong, rep(1, nrow(seqlong)))
colnames(seqlong)[4] <- "i"

# genero il bar chart
plotlabels <- paste(bases, " (", labels, ")", sep = "")
ybreaks <- c(0, pretty(c(0, max_len), n = 4)[2:(length(pretty(c(0, max_len), n = 4)) - 1)], max_len)
xbreaks <- c(1, pretty(c(1, num_seq), n = 4)[2:(length(pretty(c(1, num_seq), n = 4)) - 1)], num_seq)

# realizzo il plot delle sequenze
png(paste(dna_sequencer_Rconfig$plot_dir, "/", plotname, sep = ""), width = 10, height = 7, units = "in", res = 300)
ggplot(seqlong, aes(x = seqid, y = i, fill = bases)) +
	geom_bar(stat = "identity", width = 1) +
	coord_flip() +
	scale_y_continuous("Sequence length (# bases)", breaks = pretty_breaks(n = 4), limits = c(0, max_len)) +
	scale_x_continuous("Sequence index", breaks = xbreaks, limits = c(1, num_seq)) +
	scale_fill_manual(labels = plotlabels, values = brewer.pal(length(bases), "Set1")) +
	theme_bw(base_family = "Helvetica") +
	theme(axis.text.x = element_text(size=14), axis.title.x = element_text(size=18, vjust=-0.75), axis.text.y = element_text(size=14), axis.title.y = element_text(size=18, vjust=0.30)) +
	theme(plot.title = element_text(lineheight=.8, face="bold", size=18), legend.text = element_text(size=14), legend.title = element_text(size=18))
garbage <- dev.off()

print(plotname)
