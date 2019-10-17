
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
plot_sequences_by_color <-  function(df, dnacol = 2){
  seqwide <- setup_df(df, dnacol = dnacol)
  seq <- seqdef(df, var = dnacol, format = "STS", stsep = "")

  bases <- alphabet(seq)
  #labels <- stlab(seq)
  labels <- c("tweet","retweet","reply")
  ids <- rownames(seq)
  num_seq <- nrow(seq)
  max_len <- length(seq[1,])
  brackets <- rep(")", length(bases))
  full_labels <- paste(bases, " (", labels, ")", sep = "")

  # trasformo da formato wide a formato long
  seqlong <- reshape2::melt(seqwide, id.vars = "seqid")

  colnames(seqlong)[2:3] <- c("position", "bases")
  seqlong <- seqlong[which(seqlong$bases != "%"),]
  seqlong[,3] <- factor(seqlong[,3], ordered = F)
  seqlong <- cbind(seqlong, rep(1, nrow(seqlong)))
  colnames(seqlong)[4] <- "i"


  # genero il bar chart
  plotlabels <- paste(bases, " (", labels, ")", sep = "")
  ybreaks <- c(0, pretty(c(0, max_len), n = 4)[2:(length(pretty(c(0, max_len), n = 4)) - 1)], max_len)
  xbreaks <- c(1, pretty(c(1, num_seq), n = 4)[2:(length(pretty(c(1, num_seq), n = 4)) - 1)], num_seq)
  #### Table to order timeline ###
  id.count <- as.data.frame(table(seqlong$seqid))

  timeline_orderded_by_length <- factor(seqlong$seqid, levels = id.count$Var1[order(id.count$Freq, decreasing = F)])


#### Funzione originale ###################
  ret_value <- ggplot(seqlong, aes(x = timeline_orderded_by_length, y = i, fill = bases, group = position)) +
    geom_bar(stat = "identity", width = 1) +
    coord_flip() +
    #scale_y_continuous("Sequence length (# bases)", breaks = pretty_breaks(n = 4), limits = c(0, max_len)) +
    scale_x_continuous("Sequence index", breaks = xbreaks, limits = c(1, num_seq)) +
    scale_x_discrete("Sequence index") +
    #scale_fill_manual(labels = plotlabels, values = RColorBrewer::brewer.pal(length(bases), "Set1")) +
    scale_fill_manual(labels = plotlabels, values = c("orange", "forestgreen", "red"))
    #theme_bw(base_family = "Helvetica") +
    theme(axis.text.x = element_text(size=14), axis.title.x = element_text(size=18, vjust=-0.75), axis.text.y = element_text(size=14), axis.title.y = element_text(size=18, vjust=0.30)) +
    theme(plot.title = element_text(lineheight=.8, face="bold", size=18), legend.text = element_text(size=14), legend.title = element_text(size=18))

    return(ret_value)
    #### =========================================#####

}

#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
color_sequence_plot <- function(df){
  plotter <- ddna$SequencePlots()
  ret_value <- plotter$plot_sequences_color(df)
  return(ret_value)
}
