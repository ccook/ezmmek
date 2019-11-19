\name{p_std_curve}
\alias{p_std_curve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot and statistics for standard curve
}
\description{
'p_std_curve' plots spectral data versus standard concentration. It asks the user to specify the axis labels with the appropriate units. It reports linear model summary statistics. It also creates a list output containing the summary statistics and standard curve plot.
}
\usage{
p_std_curve(d_std)
}

\arguments{
  \item{d_std}{
d_std must be a dataframe that contains the column headers 'std.conc'(standard concentration) and 'spec' (spectral data).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \out_list{lm_stats: }{object that contains linear model summary statistics of standard curve}
    {plot_object: }{object that contains standard curve plot}
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (d_std)
{
    if (!"std_conc" \%in\% names(d_std)) {
        stop("A column named 'std_conc' was expected but not provided")
    }
    if (!"spec" \%in\% names(d_std)) {
        stop("A column named 'spec' was expected but not provided")
    }
    x.units.vec <- c("(M)", "(mM)", "(μM)", "(nM)")
    s <- readline(prompt = "Standard type: ")
    x.index.units <- menu(x.units.vec, graphics = FALSE, title = "x-axis: What are the units of concentration?")
    plot.x.label <- paste(s, x.units.vec[x.index.units], sep = " ")
    y.units.vec <- c("FSU", "AU")
    u <- readline(prompt = "y-axis: Detection unit?:")
    plot.y.label <- paste(u)
    p_std_curve_1 <- ggplot(data = d_std, mapping = aes(x = std_conc,
        y = spec)) + geom_point(size = 2.5) + theme_bw() + xlab(plot.x.label) +
        ylab(plot.y.label) + theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 18)) + geom_smooth(method = "lm")
    print(p_std_curve_1)
    lm_std_curve <- lm(formula = spec ~ std_conc, data = d_std)
    print(summary(lm_std_curve))
    print(coef(lm_std_curve))
    out_list <- list(lm_stats = lm_std_curve)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
