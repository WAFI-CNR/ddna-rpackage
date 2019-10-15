require(TraMineR)
require(reshape2)
require(ggplot2)
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
plotname <- paste(file_path_sans_ext(file_path_sans_ext(basename(filename))), ".basedist.png", sep = "")

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

# calcolo la distribuzione di probabilità delle basi per ogni posizione nelle sequenze
pdf_wide <- as.data.frame(t(apply(seqwide[,-1], 1, seqprob, bases)))
colnames(pdf_wide) <- bases
pdf_wide <- cbind(seqid = ids, pdf_wide)
# trasformo da formato wide a formato long
pdf_long <- melt(pdf_wide, id.vars = c("seqid"))
levels(pdf_long$variable) <- bases

# realizzo il boxplot sulla distribuzione delle basi
png(paste(dna_sequencer_Rconfig$plot_dir, "/", plotname, sep = ""), width = 10, height = 10, units = "in", res = 300)
ggplot(pdf_long, aes(x = variable, y = value)) +
	geom_boxplot(outlier.shape=NA) +
	geom_jitter(position = position_jitter(width = .25, height = 0), alpha = .45, color = "dodgerblue3") +
	scale_x_discrete(labels = full_labels) +
	labs(x = "Bases", y = "Frequency") +
	theme_bw(base_family = "Helvetica") +
	theme(axis.text.x = element_text(size=14), axis.title.x = element_text(size=18, vjust=-0.75), axis.text.y = element_text(size=14), axis.title.y = element_text(size=18, vjust=0.30)) +
	theme(plot.title = element_text(lineheight=.8, face="bold", size=18), legend.text = element_text(size=14), legend.title = element_text(size=18), legend.key.height=unit(2,"line"), legend.key.width=unit(2,"line"), legend.key.size = unit(1.5, "lines"))
garbage <- dev.off()

print(plotname)