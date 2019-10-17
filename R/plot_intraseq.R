
#' Title
#'
#' @param df
#' @param filename_path
#'
#' @return
#' @export
#'
#' @examples
plot_intraseq <-  function(df, dnacol = 2){
  require(TraMineR)
  require(ggplot2)
  require(gtable)
  require(scales)
  require(grid)
  require(tools)
  seqwide <- setup_df(df, dnacol=dnacol)
  seq <- seqdef(df, var = dnacol, format = "STS", stsep = "")
  bases <- alphabet(seq)
  labels <- stlab(seq)
  ids <- rownames(seq)
  num_seq <- nrow(seq)
  max_len <- length(seq[1,])
  brackets <- rep(")", length(bases))
  full_labels <- paste(bases, " (", labels, ")", sep = "")
  seqwide <- setup_df(df)

    # calcolo la distribuzione di probabilità delle basi per ogni sequenza
  intraseq_prob <- as.data.frame(t(apply(seqwide[,-1], 1, seqprob, bases)))

  # calcolo l'entropia (di Shannon) per ogni sequenza (entropia intra-sequenza)
  intraseq_entropy <- as.data.frame(apply(intraseq_prob, 1, seqentropy))
  colnames(intraseq_entropy) <- "entropy"
  mean_intraseq_entropy <- mean(intraseq_entropy$entropy)

  # calcolo la distribuzione di probabilità delle basi per ogni posizione nelle sequenze
  interseq_prob <- as.data.frame(t(apply(seqwide[, -1], 2, seqprob, bases)))

  # calcolo l'entropia (di Shannon) per ogni posizione nelle sequenze (entropia inter-sequenza)
  interseq_entropy <- as.data.frame(apply(interseq_prob, 1, seqentropy))
  colnames(interseq_entropy)<- "entropy"
  mean_interseq_entropy <- mean(interseq_entropy$entropy)
  max_entropy = max(rbind(interseq_entropy[, 1], intraseq_entropy))

  # realizzo il boxplot dell'entropia intra-sequenza
  #png(paste(dna_sequencer_Rconfig$plot_dir, "/", plotname_intra, sep = ""), width = 3, height = 8, units = "in", res = 300)
  intraseq_plot <- ggplot(intraseq_entropy, aes(x = "dummy", y = entropy))+
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(position = position_jitter(width = .25, height = 0), alpha = .45, color = "dodgerblue3")+
  geom_hline(yintercept = mean_intraseq_entropy, color = "red")+
  scale_y_continuous(limits = c(0, max_entropy))+
  labs(x = "", y = "Intra-sequence Entropy")+
  theme_bw(base_family = "Helvetica")+
  theme(axis.text.x = element_text(size=14, color="white"), axis.title.x = element_text(size=18, vjust=-0.75, color="white"), axis.ticks.x = element_blank(), axis.text.y = element_text(size=14), axis.title.y = element_text(size=18, vjust=0.30))+
  theme(plot.title = element_text(lineheight=.8, face="bold", size=18), legend.text = element_text(size=14), legend.title = element_text(size=18), legend.key.height=unit(2, "line"), legend.key.width=unit(2, "line"), legend.key.size = unit(1.5, "lines"))

  return(intraseq_plot)

}
