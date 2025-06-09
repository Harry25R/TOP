#' Pairwise regularised CCA on a list of expression matrices
#' ---------------------------------------------------------
#' @param x_list list of matrices/data.frames (rows = genes, cols = samples)
#' @param scale  logi – centre & scale genes first?  (default TRUE)
#' @return list(cca_mat = symmetric matrix, plot = ggplot object)
#' @details Uses PMA::CCA with a √p sparsity rule and plots the
#'          lower-triangle heat-map (L-shape) with text labels.
#' @import PMA ggplot2 reshape2 dplyr
#' Pairwise regularised CCA on a list of expression matrices
#' ---------------------------------------------------------
#' Returns a list with the CCA matrix and a ggplot heat-map.
#' No console chatter.
# --------------------------------------------------------------------
# Pairwise regularised CCA → (matrix, ggplot)
# --------------------------------------------------------------------
cca_pairwise <- function(x_list, scale = TRUE) {
  
  stopifnot(requireNamespace("PMA",      quietly = TRUE),
            requireNamespace("reshape2", quietly = TRUE),
            requireNamespace("dplyr",    quietly = TRUE),
            requireNamespace("ggplot2",  quietly = TRUE))
  
  library(PMA)
  library(reshape2)
  library(dplyr)
  library(ggplot2)
  
  common  <- Reduce(intersect, lapply(x_list, rownames))
  x_list  <- lapply(x_list, `[`, i = common, j = TRUE)
  if (scale) x_list <- lapply(x_list, scale, center = TRUE, scale = TRUE)
  
  pick_pen <- function(p) min(0.7, sqrt(p) / p)        
  first_r  <- function(out) {
    out$cor[1]
  }
  
  studies <- names(x_list)
  S       <- length(studies)
  cca_mat <- matrix(NA_real_, S, S, dimnames = list(studies, studies))
  
  for (i in seq_len(S - 1)) {
    for (j in (i + 1):S) {
      cca_res <- {
        tmp <- NULL
        junk <- capture.output(                       
          tmp <- CCA(x        = x_list[[i]],
                     z        = x_list[[j]],
                     typex    = "standard",
                     typez    = "standard",
                     K        = 1,
                     penaltyx = pick_pen(ncol(x_list[[i]])),
                     penaltyz = pick_pen(ncol(x_list[[j]])),
                     trace    = FALSE),               
          file = NULL)
        tmp                                     
      }
      cca_mat[i, j] <- cca_mat[j, i] <- first_r(cca_res)
    }
  }
  diag(cca_mat) <- 1
  
  cca_df <- melt(cca_mat) %>%
    mutate(Var1 = factor(Var1, levels = studies),
           Var2 = factor(Var2, levels = studies)) %>%
    filter(as.numeric(Var2) >= as.numeric(Var1))  
  
  p <- ggplot(cca_df, aes(Var1, Var2, fill = value)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = sprintf("%.2f", value)), size = 3) +
    scale_y_discrete(limits = rev(levels(cca_df$Var2))) +
    scale_fill_gradient(low = "white", high = "firebrick",
                        limits = c(0, 1), name = "Canonical\ncorr") +
    coord_fixed() +
    theme_minimal(base_size = 12) +
    theme(axis.title  = element_blank(),
          axis.ticks  = element_blank(),
          panel.grid  = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  invisible(list(cca_mat = cca_mat, plot = p))
}


#' @title CCA vs AUC Plot
#' @description Plot canonical correlation coefficients against AUC values to assess model transferability.
#'
#' @param cca Numeric vector of canonical correlation coefficients.
#' @param auc Numeric vector of AUC values associated with the same models.
#' @param labels Optional character vector of labels for each point.
#'
#' @return A ggplot object displaying the relationship between CCA and AUC.
#' @export
#'
#' @examples
#' cca <- c(0.8, 0.6, 0.75)
#' auc <- c(0.9, 0.7, 0.82)
#' CCA_AUC_plot(cca, auc)
#'
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @importFrom stats lm
CCA_AUC_plot <- function(cca, auc, labels = NULL) {
  if (length(cca) != length(auc)) {
    stop("cca and auc must have the same length")
  }
  df <- data.frame(CCA = cca, AUC = auc, label = labels)
  p <- ggplot(df, aes(x = CCA, y = AUC)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
    theme_bw() +
    xlab("Canonical Correlation (CCA)") +
    ylab("Area Under Curve (AUC)")
  if (!is.null(labels)) {
    p <- p + ggrepel::geom_text_repel(aes(label = label))
  }
  return(p)
}
