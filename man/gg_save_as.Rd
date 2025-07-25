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
  Named arguments passed on to \code{\link[=.gg_save_as]{.gg_save_as}}\describe{
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
\item{\code{...}}{
  Named arguments passed on to \code{\link[svglite:svglite]{svglite::svglite}}\describe{
    \item{\code{height,width}}{Height and width in inches.}
\item{\code{bg}}{Default background color for the plot (defaults to "white").}
\item{\code{pointsize}}{Default point size.}
\item{\code{standalone}}{Produce a standalone svg file? If \code{FALSE}, omits
xml header and default namespace.}
\item{\code{web_fonts}}{A list containing web fonts to use in the SVG. The fonts
will still need to be available locally on the computer running the code,
but viewers of the final SVG will not need the font if specified as a web
font. Web fonts can either be specified using \code{\link[svglite:font_face]{font_face()}} or given as a
single string in which case they are taken to be URL's for an \verb{@import}
directive to e.g. Google Fonts. For the latter, you can use
\code{\link[svglite:fonts_as_import]{fonts_as_import()}} to automatically generate the string, optionally
embedding the font data in it. If the passed in string is not in the form
of a URL or \verb{@import} statement then it is considered a font family name
and \code{\link[svglite:fonts_as_import]{fonts_as_import()}} will be called to convert it to an import
automatically, using the default arguments.}
\item{\code{id}}{A character vector of ids to assign to the generated SVG's. If
creating more SVG files than supplied ids the exceeding SVG's will not have
an id tag and a warning will be thrown.}
\item{\code{fix_text_size}}{Should the width of strings be fixed so that it doesn't
change between svg renderers depending on their font rendering? Defaults to
\code{TRUE}. If \code{TRUE} each string will have the \code{textLength} CSS property set
to the width calculated by systemfonts and
\code{lengthAdjust='spacingAndGlyphs'}. Setting this to \code{FALSE} can be
beneficial for heavy post-processing that may change content or style of
strings, but may lead to inconsistencies between strings and graphic
elements that depend on the dimensions of the string (e.g. label borders
and background).}
\item{\code{scaling}}{A scaling factor to apply to the rendered line width and text
size. Useful for getting the right sizing at the dimension that you
need.}
\item{\code{always_valid}}{Should the svgfile be a valid svg file while it is being
written to? Setting this to \code{TRUE} will incur a considerable performance
hit (>50\% additional rendering time) so this should only be set to \code{TRUE}
if the file is being parsed while it is still being written to.}
\item{\code{file}}{Identical to \code{filename}. Provided for backward compatibility.}
\item{\code{system_fonts}}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}} \emph{Consider using
\code{\link[systemfonts:register_font]{systemfonts::register_font()}} instead}. Named list of font
names to be aliased with fonts installed on your system. If unspecified,
the R default families \code{sans}, \code{serif}, \code{mono} and
\code{symbol} are aliased to the family returned by
\code{\link[systemfonts]{font_info}()}.}
\item{\code{user_fonts}}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}} \emph{Consider using
\code{\link[systemfonts:register_font]{systemfonts::register_font()}} instead}. Named list of fonts to
be aliased with font files provided by the user rather than fonts properly
installed on the system. The aliases can be fonts from the fontquiver
package, strings containing a path to a font file, or a list containing
\code{name} and \code{file} elements with \code{name} indicating
the font alias in the SVG output and \code{file} the path to a
font file.}
}}
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
