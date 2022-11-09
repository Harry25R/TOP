% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ROC_Plot.R
\name{ROC_Plot}
\alias{ROC_Plot}
\title{ROC_Plot}
\usage{
ROC_Plot(roc_list)
}
\arguments{
\item{roc_list}{A list of roc object from the pROC package}
}
\description{
ROC_Plot
}
\examples{
data(cpop_data_binary, package = "CPOP")

x1 <- cpop_data_binary$x1
x2 <- cpop_data_binary$x2
x3 <- cpop_data_binary$x3
y1 <- cpop_data_binary$y1
y2 <- cpop_data_binary$y2
y3 <- cpop_data_binary$y3

set.seed(23)
x_list <- list(x1, x2)
y_list <- list(factor(y1), factor(y2))

fCPOP_model <- Frankenstein_CPOP(x_list, y_list)
pred <- predict_cpop2(fCPOP_model$models, newx = x3)
roc_fCPOP <- roc(y3, pred)
ROC_Plot(list(roc_fCPOP))

}