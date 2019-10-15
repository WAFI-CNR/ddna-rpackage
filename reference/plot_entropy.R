require(TraMineR)
require(ggplot2)
require(gtable)
require(scales)
require(grid)
require(tools)

# carico il file di configurazione
dna_sequencer_Rconfig <- readRDS("/var/www/fake/evolution/dna/sequencer/sequencer_Rconfig.inc.rds")
# carico la libreria contenente le funzioni di utilità
source(dna_sequencer_Rconfig$lib_file)

# leggo gli argomenti passati allo script
args <- commandArgs(TRUE)
if (length(args) != 1) stop("ERROR!!! Invalid arguments!", call. = T)
# percorso completo al file RDS con le sequenze
filename <- args[1]
plotname_intra <- paste(file_path_sans_ext(file_path_sans_ext(basename(filename))), ".intraseqent.png", sep = "")
plotname_inter <- paste(file_path_sans_ext(file_path_sans_ext(basename(filename))), ".interseqent.png", sep = "")

# carico l'oggetto contenente le sequenze
seq <- readRDS(filename)
# estraggo informazioni sulle sequenze
bases <- alphabet(seq)
labels <- stlab(seq)
ids <- rownames(seq)
brackets <- rep(")", length(bases))
full_labels <- paste(bases, " (", labels, ")", sep = "")

# genero il data frame delle sequenze
seqwide <- unname(apply(seq, 2, vectorize))
dummyrows <- unname(t(sapply(c(bases, "%"), function(x) rep(x, ncol(seqwide)))))
seqwide <- as.data.frame(rbind(seqwide, dummyrows))
seqwide <- seqwide[-c((nrow(seqwide) - length(bases)):nrow(seqwide)),]
seqwide <- cbind(seqid = ids, seqwide)

# calcolo la distribuzione di probabilità delle basi per ogni sequenza
intraseq_prob <- as.data.frame(t(apply(seqwide[,-1], 1, seqprob, bases)))

# calcolo l'entropia (di Shannon) per ogni sequenza (entropia intra-sequenza)
intraseq_entropy <- as.data.frame(apply(intraseq_prob, 1, seqentropy))
colnames(intraseq_entropy) <- "entropy"
mean_intraseq_entropy <- mean(intraseq_entropy$entropy)

# qui farebbe comodo eseguire un MSA, prima di calcolare le probabilità per colonna

# calcolo la distribuzione di probabilità delle basi per ogni posizione nelle sequenze
interseq_prob <- as.data.frame(t(apply(seqwide[,-1], 2, seqprob, bases)))

# calcolo l'entropia (di Shannon) per ogni posizione nelle sequenze (entropia inter-sequenza)
interseq_entropy <- as.data.frame(apply(interseq_prob, 1, seqentropy))
colnames(interseq_entropy) <- "entropy"
mean_interseq_entropy <- mean(interseq_entropy$entropy)

max_entropy = max(rbind(interseq_entropy[,1],intraseq_entropy))

# realizzo il boxplot dell'entropia intra-sequenza
png(paste(dna_sequencer_Rconfig$plot_dir, "/", plotname_intra, sep = ""), width = 3, height = 8, units = "in", res = 300)
ggplot(intraseq_entropy, aes(x = "dummy", y = entropy)) +
	geom_boxplot(outlier.shape=NA) +
	geom_jitter(position = position_jitter(width = .25, height = 0), alpha = .45, color = "dodgerblue3") +
	geom_hline(yintercept = mean_intraseq_entropy, color = "red") +
	scale_y_continuous(limits = c(0, max_entropy)) +
	labs(x = "", y = "Intra-sequence Entropy") +
	theme_bw(base_family = "Helvetica") +
	theme(axis.text.x = element_text(size=14, color="white"), axis.title.x = element_text(size=18, vjust=-0.75, color="white"), axis.ticks.x = element_blank(), axis.text.y = element_text(size=14), axis.title.y = element_text(size=18, vjust=0.30)) +
	theme(plot.title = element_text(lineheight=.8, face="bold", size=18), legend.text = element_text(size=14), legend.title = element_text(size=18), legend.key.height=unit(2,"line"), legend.key.width=unit(2,"line"), legend.key.size = unit(1.5, "lines"))
garbage <- dev.off()

# realizzo il boxplot dell'entropia inter-sequenza
interseq_boxplot <- ggplot(interseq_entropy, aes(x = "dummy", y = entropy)) +
	geom_boxplot(outlier.shape=NA) +
	geom_jitter(position = position_jitter(width = .25, height = 0), alpha = .45, color = "dodgerblue3") +
	geom_hline(yintercept = mean_interseq_entropy, color = "red") +
	scale_y_continuous(limits = c(0, max_entropy)) +
	labs(x = "", y = "Inter-sequence Entropy") +
	theme_bw(base_family = "Helvetica") +
	theme(axis.text.x = element_text(size=14, color="white"), axis.title.x = element_text(size=18, vjust=-0.75, color="white"), axis.ticks.x = element_blank(), axis.text.y = element_text(size=14), axis.title.y = element_text(size=18, vjust=0.30)) +
	theme(plot.title = element_text(lineheight=.8, face="bold", size=18), legend.text = element_text(size=14), legend.title = element_text(size=18), legend.key.height=unit(2,"line"), legend.key.width=unit(2,"line"), legend.key.size = unit(1.5, "lines"))

# realizzo il line plot dell'entropia inter-sequenza
interseq_entropy <- cbind(interseq_entropy, seq(1, nrow(interseq_entropy), 1))
colnames(interseq_entropy)[2] <- "position"
interseq_lineplot <- ggplot(interseq_entropy, aes(position, entropy)) +
	geom_line(color = "dodgerblue3") +
	geom_point(color = "dodgerblue3", shape = 4, size = 4) +
	geom_hline(yintercept = mean_interseq_entropy, color = "red") +
	scale_x_continuous(breaks = pretty_breaks(n = 5), limits = c(0, nrow(interseq_entropy))) +
	scale_y_continuous(limits = c(0, max_entropy)) +
	labs(x = "Position in sequence", y = "") +
	theme_bw(base_family = "Helvetica") +
	theme(axis.text.x = element_text(size=14), axis.title.x = element_text(size=18, vjust=-0.75), axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank()) +
	theme(plot.title = element_text(lineheight=.8, face="bold", size=18), legend.text = element_text(size=14), legend.title = element_text(size=18))

# unisco i 2 plot
interseq_linegrob <- ggplotGrob(interseq_lineplot)
combined_plot <- ggplotGrob(interseq_boxplot)
combined_plot <- gtable_add_cols(combined_plot, unit(4, "null"))
# per vedere la struttura della tabella: gtable_show_layout(combined_plot)
combined_plot <- gtable_add_grob(combined_plot, interseq_linegrob, t = 1, l = 6, b = 6, r = 6)

# li visualizzo
png(paste(dna_sequencer_Rconfig$plot_dir, "/", plotname_inter, sep = ""), width = 10, height = 8, units = "in", res = 300)
grid.newpage()
grid.draw(combined_plot)
garbage <- dev.off()

print(plotname_intra)
print(plotname_inter)