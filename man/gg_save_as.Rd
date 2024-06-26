% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zz-api-file-output-utils.R
\name{gg_save_as}
\alias{gg_save_as}
\title{Save a plot to multiple formats}
\usage{
gg_save_as(...)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[=.gg_save_as]{.gg_save_as}}
  \describe{
    \item{\code{filename}}{base of target filename (excluding extension).}
    \item{\code{plot}}{a ggplot}
    \item{\code{size}}{a standard size see `std_size`}
    \item{\code{maxWidth}}{maximum width in inches}
    \item{\code{maxHeight}}{maximum height in inches}
    \item{\code{aspectRatio}}{defaults to maxWidth/maxHeight}
    \item{\code{formats}}{some of svg, png, pdf, Rdata, eps, ...}
    \item{\code{webfontFinder}}{a function that takes a plot and returns a properly
formatted css specification for webfonts in the plot. This is for internal
use and does not need to be changed.}
  }}
}
\value{
the output is an sensible default object that can be displayed given
  the context it is called in, for example if knitting an RMarkdown document
  a link to the png file for embedding, if latex a link to the pdf file.
}
\description{
Saves a ggplot object to disk at a set physical size. Allows specific maximum dimensions
with an optional target aspect ratio to fit into specific configurations for publication.
e.g. a half page plot or a third of a 2 column page. Allows output in pdf for journal
publication or png for inclusion in documents, and makes sure that the outputs are
near identical.
}
\details{
For maximum cross platform reproducibility we are using the combination of
`systemfonts` for font management, `svglite` to render the canonical output
`rsvg` to convert that to pdf, and `ragg` to for bitmap formats.
In some situations `rsvg` fails in which case we fall back to rendering in a
headless chrome instance. This rather complicated pipeline ensures modern
webfont support, and editable SVG or PDF.
}
\examples{

try({
.gg_pedantic(fontSize = 6)
p = ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt, colour=as.factor(cyl))) +
  ggplot2::geom_point()
# p \%>\% .gg_save_as(filename="~/tmp/plot_example",maxWidth=4,maxHeight=4)
p \%>\% .gg_save_as(filename=tempfile(),maxWidth=2,maxHeight=1.5)

plot = ggplot2::ggplot(ggplot2::diamonds, ggplot2::aes(x=carat,y=price,color = color))+
  ggplot2::geom_point()+
  ggplot2::annotate("label",x=2,y=10000,label="Hello \u2014 world", family="Kings")+
  ggplot2::labs(tag = "A")+
  ggplot2::xlab("Carat\u2082")+
  ggplot2::ylab("price\u2265")

# plot \%>\% .gg_save_as(filename="~/tmp/plot_example_2")
res = plot \%>\% .gg_save_as(filename=tempfile(), formats=c("png","eps"))
as.character(res)
res
})
}
\concept{output}
