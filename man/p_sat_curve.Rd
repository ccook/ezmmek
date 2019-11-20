\name{p_sat_curve}
\alias{p_sat_curve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Dataframes, plot, and statistics for saturation curve
}
\description{
'p_sat_curve' creates a new dataframe by applying the standard curve coefficients to the raw saturation data. The spectral data is converted to concentration of standard. The new dataframe contains the average slope (rate of reaction) and standard deviation for each replicate at each substrate concentration. 'p_sat_curve' plots the new dataframe with substrate concentration on the x-axis, and rate of reaction on the y-axis. It asks the user to specify axis labels with the appropriate units. It calculates and reports Vmax and Km values. It also creates a list output containing the new dataframe, regression model, and saturation curve plot.
}
\usage{
p_sat_curve(d_std, d_sat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d_std}{
  d_std must be a dataframe that contains the column headers 'std.conc' and 'spec'.
}
  \item{d_sat}{
d_sat must be a dataframe that contains the column headers 'time', 'sub.conc', 'replicate', and 'spec'. If d_sat contains a fifth column, that fifth column will be assumed to be a normalization factor. The rate of reacation will be divided by the values present in the fifth column. The user will be prompted to name the unit of normalization, which will appear on the y-axis, if a fifth column is present.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{sat_data }{new dataframe containing average reaction rates and corresponding standard deviations}
\item{fit_object }{regression model used to calculate saturation curve}
\item{curve_data }{new dataframe containing predicted values for saturation curve fit}
\item{plot_object }{plot of saturation curve}
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
p_sat_curve(d_std, d_sat)
p_sat_curve(d_std, d_sat_n)

# 'd_std' is standard curve data
# 'd_sat' is saturation curve data
# 'd_sat_n' is saturation curve data with normalization factor

## The function is currently defined as
function (d_std, d_sat)
{
    if (!"std.conc" \%in\% names(d_std)) {
        stop("A column named 'std.conc' was expected but not provided")
    }
    if (!"spec" \%in\% names(d_std)) {
        stop("A column named 'spec' was expected but not provided")
    }
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
    lm_fit <- lm(spec ~ std.conc, data = d_std)
    lm.intercept <- coef(lm_fit)[1]
    lm.slope <- coef(lm_fit)[2]
    d_sat$spec.to.std <- (d_sat$spec - lm.intercept)/lm.slope
    if (ncol(d_sat) == 6) {
        d_sat <- dplyr::mutate(d_sat, activity.norm = spec.to.std/d_sat[,
            5])
        d_sat_2 <- d_sat \%>\% dplyr::group_by(sub.conc, replicate) \%>\%
            tidyr::nest() \%>\% dplyr::mutate(std.slope = purrr::map_dbl(data,
            function(df) coef(lm(activity.norm ~ time, data = df))[2])) \%>\%
            dplyr::group_by(sub.conc) \%>\% dplyr::mutate(slope.m = mean(std.slope),
            slope.sd = sd(std.slope))
    }
    else {
        d_sat_2 <- d_sat \%>\% dplyr::group_by(sub.conc, replicate) \%>\%
            tidyr::nest() \%>\% dplyr::mutate(std.slope = purrr::map_dbl(data,
            function(df) coef(lm(spec.to.std ~ time, data = df))[2])) \%>\%
            dplyr::group_by(sub.conc) \%>\% dplyr::mutate(slope.m = mean(std.slope),
            slope.sd = sd(std.slope))
    }
    x.units.vec <- c("(M)", "(mM)", "(μM)", "(nM)")
    x.s <- readline(prompt = "Substrate name: ")
    x.index.units <- menu(x.units.vec, graphics = FALSE, title = "x-axis: What are the units of substrate concentration?")
    plot.x.label <- paste(x.s, x.units.vec[x.index.units], sep = " ")
    sup.s <- "<U+207B>?"
    y.units.vec.conc <- c("M", "mM", "μM", "nM")
    y.units.vec.time <- c(paste("sec", sup.s, sep = ""), paste("min",
        sup.s, sep = ""), paste("hr", sup.s, sep = ""), paste("day",
        sup.s, sep = ""))
    y.index.units.conc <- menu(y.units.vec.conc, graphics = FALSE,
        title = "y-axis: What are the units of concentration?")
    y.index.units.time <- menu(y.units.vec.time, graphics = FALSE,
        title = "y-axis: What are the units of time?")
    if ("activity.norm" \%in\% names(d_sat)) {
        norm.name <- readline(prompt = "Normalization unit: ")
        y.units.vec.norm <- c(paste(norm.name, sup.s, sep = ""))
        plot.y.label <- paste("Reaction Rate", paste("(", y.units.vec.conc[y.index.units.conc],
            sep = ""), y.units.vec.time[y.index.units.time],
            paste(y.units.vec.norm, ")", sep = ""), sep = " ")
    }
    else {
        plot.y.label <- paste("Reaction Rate", paste("(", y.units.vec.conc[y.index.units.conc],
            sep = ""), paste(y.units.vec.time[y.index.units.time],
            ")", sep = ""), sep = " ")
    }
    p_sat_curve_1 <- ggplot2::ggplot(data = d_sat_2, mapping = ggplot2::aes(x = sub.conc,
        y = slope.m)) + ggplot2::geom_point(size = 1.5) + ggplot2::theme_bw() +
        ggplot2::xlab(plot.x.label) + ggplot2::ylab(plot.y.label) +
        ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
            axis.title = ggplot2::element_text(size = 18)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = slope.m -
            slope.sd, ymax = slope.m + slope.sd, width = 15)) +
        ggplot2::scale_y_continuous(labels = scales::scientific)
    max.slope <- max(d_sat_2$slope.m)
    half.conc <- median(d_sat_2$sub.conc)
    mm_form <- formula(slope.m ~ (Vmax * sub.conc)/(Km + sub.conc))
    mm_fit <- nls2::nls2(formula = mm_form, data = d_sat_2, start = list(Vmax = max.slope,
        Km = half.conc))
    print(summary(mm_fit))
    min.sub.conc <- min(d_sat_2$sub.conc)
    max.sub.conc <- max(d_sat_2$sub.conc)
    pred_grid <- data.frame(sub.conc = min.sub.conc:max.sub.conc)
    predictions <- predict(mm_fit, newdata = pred_grid)
    pred_df <- data.frame(sub.conc = pred_grid$sub.conc, slope.m = predictions)
    p_sat_fit <- p_sat_curve_1 + ggplot2::geom_line(data = pred_df,
        ggplot2::aes(x = sub.conc, y = slope.m))
    plot(p_sat_fit)
    out_list <- list(sat_data = d_sat_2, curve_data = pred_df, fit_object = mm_fit,
        plot_object = p_sat_fit)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
