\name{p_sat_raw}
\alias{p_sat_raw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to plot raw saturation data
}
\description{
'p_sat_raw' plots raw saturation curve data into separate facets based on substrate concentration ('sub.conc'). It asks the user to specify the axis labels with the appropriate units. It also creates a list output containing the raw data plot.
}
\usage{
p_sat_raw(d_sat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d_sat}{
d_sat must be a dataframe that contains the column headers
'time', 'sub.conc' (substrate concentration), 'replicate', and 'spec' (spectral data).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{plot_object }{plot of raw saturation data}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
p_sat_raw(d_sat)
p_sat_raw(d_sat_n)

# 'd_sat' is saturation curve data
# 'd_sat_n' is saturation curve data with a normalization factor

## The function is currently defined as
function (d_sat)
{
    if (!"time" \%in\% names(d_sat)) {
        stop("A column named 'time' was expected but not provided")
    }
    if (!"spec" \%in\% names(d_sat)) {
        stop("A column named 'spec' was expected but not provided")
    }
    if (!"replicate" \%in\% names(d_sat)) {
        stop("A column named 'replicate' was expected but not provided")
    }
    if (!"sub.conc" \%in\% names(d_sat)) {
        stop("A column named 'sub.conc' was expected but not provided")
    }
    x.units.vec <- c("(sec)", "(min)", "(hr)", "(day)")
    x.index.units <- menu(x.units.vec, graphics = FALSE, title = "x-axis: What are the units of time?")
    plot.x.label <- paste("Time", x.units.vec[x.index.units],
        sep = " ")
    y.d <- readline(prompt = "y-axis: Detection unit?:")
    plot.y.label <- paste(y.d)
    p_sat_raw_1 <- ggplot2::ggplot(data = d_sat, mapping = ggplot2::aes(x = time,
        y = spec, color = as.factor(replicate))) + ggplot2::geom_point() +
        ggplot2::geom_smooth(method = "lm") + ggplot2::facet_wrap(~sub.conc,
        scales = "fixed") + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7)) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 6)) +
        ggplot2::scale_y_continuous(labels = scales::scientific) +
        ggplot2::xlab(plot.x.label) + ggplot2::ylab(plot.y.label) +
        ggplot2::scale_color_discrete(name = "Replicate") + ggplot2::theme_bw()
    print(p_sat_raw_1)
    out_list <- list(plot_object = p_sat_raw_1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
