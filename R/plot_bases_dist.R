library(ggplot2)


#' Title
#'
#' @param df
#' @param dnacol
#'
#' @return
#' @export
#'
#' @examples
plot_bases_distribution <-  function(df, dnacol = 2){
  require(TraMineR)
  require(ggplot2)
  require(gtable)
  require(scales)
  require(grid)
  require(tools)
  # calcolo la distribuzione di probabilità delle basi per ogni posizione nelle sequenze
  seqwide <- setup_df(df, dnacol = dnacol)
  seq <- seqdef(df, var = dnacol, format = "STS", stsep = "")

  # estraggo informazioni sulle sequenze
  bases <- alphabet(seq)
  labels <- stlab(seq)
  ids <- rownames(seq)
  brackets <- rep(")", length(bases))
  full_labels <- paste(bases, " (", labels, ")", sep = "")
  pdf_wide <- as.data.frame(t(apply(seqwide[,-1], 1, seqprob, bases)))
  colnames(pdf_wide) <- bases
  pdf_wide <- cbind(seqid = ids, pdf_wide)

  # trasformo da formato wide a formato long
  pdf_long <- reshape2::melt(pdf_wide, id.vars = c("seqid"))
  levels(pdf_long$variable) <- bases

  # realizzo il boxplot sulla distribuzione delle basi
  # png(paste("imgs", "/", "plotname.png", sep = ""), width = 10, height = 10, units = "in", res = 300)
  ret_value <- ggplot(pdf_long, aes(x = variable, y = value)) +
    geom_boxplot(outlier.shape=NA) +
    geom_jitter(position = position_jitter(width = .25, height = 0), alpha = .45, color = "dodgerblue3") +
    scale_x_discrete(labels = full_labels) +
    labs(x = "Bases", y = "Frequency") +
    theme_bw(base_family = "Helvetica") +
    theme(axis.text.x = element_text(size=14), axis.title.x = element_text(size=18, vjust=-0.75), axis.text.y = element_text(size=14), axis.title.y = element_text(size=18, vjust=0.30)) +
    theme(plot.title = element_text(lineheight=.8, face="bold", size=18), legend.text = element_text(size=14), legend.title = element_text(size=18), legend.key.height=unit(2,"line"), legend.key.width=unit(2,"line"), legend.key.size = unit(1.5, "lines"))

  return(ret_value)
}
