
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
#'
lcs_plot <- function(df){
  library(ggplot2)
  library(gridExtra)
  library(grid)
  library(scales)

  longest_common_subsequence(df)
  # carico i dati
  filename <- paste0(getwd(), "/glcr_cache.mat", collapse = NULL)
  filename <- "D:/Google Drive/Europee2019 CNR/SISP 2019/timeline utenti/leader following/matrici/1122076402432528384_glcr_cache.mat"
  lcs1 <- read.csv(filename, header=T, sep=",")
  colnames(lcs1) <- c("Accounts", "LCS", "x", "y")
  end1 <- lcs1$Accounts[length(lcs1$Accounts)]
  LCS_max <- max(lcs1$LCS)

  decision_function <- function(lcs1, w_size = 10){
    lunghezza <- length(lcs1$LCS)
    w_size <- w_size
    X_avg_diff = diff(running_mean(lcs1, w_size))
    i = 0
    max_index = lunghezza - w_size
    for (j in 1:max_index){
      print(j)
      print(paste(X_avg_diff[j], mean(X_avg_diff[j:j+w_size])))
      if(X_avg_diff[j] < mean(X_avg_diff[j:j+w_size])) break
      i <- i+1
    }
    print(i)
    t = which.max(X_avg_diff[i:length(X_avg_diff)])
    print(t)
    return(t + i)

  }

  running_mean <- function(x, winsize){
    run_mean_vector <- numeric()
    for(i in 1:length(x$LCS)){
      run_mean_vector[i] <- round(mean(x$LCS[max(1, i):min(i + winsize, length(x$LCS))]),2)
    }
  return(run_mean_vector)
  }

  # plot lineare
  lcs_plot1_lin <- ggplot(lcs1, aes(Accounts, LCS)) +
    geom_line(color="dodgerblue4") +
    geom_point(color="dodgerblue3", shape=1, size=5, alpha=0.25) +
    scale_x_continuous(breaks = pretty_breaks(n = 5), limits = c(0, end1)) +
    scale_y_continuous(limits = c(0, LCS_max)) +
    #theme_bw(base_family = "Helvetica") +
    theme(axis.text.x = element_text(size=14), axis.title.x = element_text(size=18, vjust=-0.75), axis.text.y = element_text(size=14), axis.title.y = element_text(size=18, vjust=0.30)) +
    theme(plot.title = element_text(lineheight=.8, face="bold", size=18), legend.text = element_text(size=14), legend.title = element_text(size=18), legend.key.height=unit(2,"line"), legend.key.width=unit(2,"line"), legend.key.size = unit(1.5, "lines")) +
    geom_vline(xintercept = decision_function(lcs1), color="orangered", linetype="dashed", size = 1.5)

  # preparo breaks e minor breaks per il plot log-log
  breaks <- 10^(-10:10)
  minor_breaks <- rep(1:9, 21) * (10^rep(-10:10, each = 9))

  # plot logaritmico
  lcs_plot1_log <- ggplot(lcs1, aes(Accounts, LCS)) +
    geom_line(color="dodgerblue4") +
    geom_point(color="dodgerblue3", shape=1, size=1.75, alpha=0.25) +
    scale_x_log10(breaks = breaks, minor_breaks = minor_breaks) +
    scale_y_log10(breaks = breaks, minor_breaks = minor_breaks) +
    annotation_logticks(sides='trbl') +
    #xlab("") +
    #ylab("") +
    #theme_bw(base_family = "Helvetica") +
    geom_vline(xintercept = decision_function(lcs1), color="orangered", linetype="dashed", size = 1.5) +
    theme(axis.text.x = element_text(size=14), axis.title.x = element_text(size=18, vjust=-0.75), axis.text.y = element_text(size=14), axis.title.y = element_text(size=18, vjust=0.30)) +
    theme(plot.title = element_text(lineheight=.8, face="bold", size=18), legend.text = element_text(size=14), legend.title = element_text(size=18), legend.key.height=unit(2,"line"), legend.key.width=unit(2,"line"), legend.key.size = unit(1.5, "lines"))
    #theme(plot.margin = unit(c(0,0,0,0), "cm"))

  # unisco i due plot

  # Definisco la posizione dell'inset come percentuale rispetto al plot principale
  xleft   <- 1 - 0.03
  xright  <- 1 - 0.78
  ybottom <- 1 - 0.03
  ytop    <- 1 - 0.78

  # Calcolo la posizione dell'inset nelle coordinate del plot principale
  main <- ggplot_build(lcs_plot1_lin)
  x1 = main$layout$panel_params[[1]]$x.range[1]
  x2 = main$layout$panel_params[[1]]$x.range[2]
  y1 = main$layout$panel_params[[1]]$y.range[1]
  y2 = main$layout$panel_params[[1]]$y.range[2]
  xdif <- x2 - x1
  ydif <- y2 - y1
  xmin  <- x1 + (xleft * xdif)
  xmax  <- x1 + (xright * xdif)
  ymin  <- y1 + (ybottom * ydif)
  ymax  <- y1 + (ytop * ydif)

  # inserisco l'inset nel plot principale
  log_grob = ggplotGrob(lcs_plot1_log)
  fullPlot <- lcs_plot1_lin + annotation_custom(grob = log_grob, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)

  return(list(lcs_plot1_lin, lcs_plot1_log))

}
