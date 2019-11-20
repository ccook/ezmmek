\name{p_std_curve}
\alias{p_std_curve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to plot standard curve
}
\description{
'p_std_curve' plots spectral data versus standard concentration. It asks the user to specify the axis labels with the appropriate units. It reports linear model summary statistics. It also creates a list output containing the summary statistics and standard curve plot.
}
\usage{
p_std_curve(d_std)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d_std}{
d_std must be a dataframe that contains the column headers 'std.conc'(standard concentration) and 'spec' (spectral data).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{fit_object}{regression model used to calculate standard curve}
\item{plot_object }{plot of standard curve}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Christopher Cook
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
p_std_curve(d_std)

# 'd_std' is standard curve data

## The function is currently defined as
function (d_std)
{
    if (!"std.conc" \%in\% names(d_std)) {
        stop("A column named 'std.conc' was expected but not provided")
    }
    if (!"spec" \%in\% names(d_std)) {
        stop("A column named 'spec' was expected but not provided")
    }
    x.units.vec <- c("(M)", "(mM)", "(μM)", "(nM)")
    x.s <- readline(prompt = "Standard type: ")
    x.index.units <- menu(x.units.vec, graphics = FALSE, title = "x-axis: What are the units of concentration?")
    plot.x.label <- paste(x.s, x.units.vec[x.index.units], sep = " ")
    y.d <- readline(prompt = "y-axis: Detection unit?:")
    plot.y.label <- paste(y.d)
    p_std_curve_1 <- ggplot2::ggplot(data = d_std, mapping = ggplot2::aes(x = std.conc,
        y = spec)) + ggplot2::geom_point(size = 2.5) + ggplot2::theme_bw() +
        ggplot2::xlab(plot.x.label) + ggplot2::ylab(plot.y.label) +
        ggplot2::scale_y_continuous(labels = scales::scientific) +
        ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
            axis.title = ggplot2::element_text(size = 18)) +
        ggplot2::geom_smooth(method = "lm")
    print(p_std_curve_1)
    lm_std_curve <- lm(formula = spec ~ std.conc, data = d_std)
    print(summary(lm_std_curve))
    out_list <- list(fit_object = lm_std_curve, plot_object = p_std_curve_1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
