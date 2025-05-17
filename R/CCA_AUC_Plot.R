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
