#' @title King Gizzard and the Lizard Wizard palettes
#' @export
kgatlw_pal <- list(
## 12 Bar Bruise (2012)
twelvebar = c("#f53728", "#524847", "#00baac", "#ffc700", "#00bd32", "#ff6a75", "#9f789b"),
## Eyes Like the Sky (2013)
elts = c("#fbaf54", "#000000", "#ffdfb5", "#0073ff"),
## Float Along-- Fill Your Lungs (2013)
floatalong = c("#00b83d", "#b5d945", "#f8ea10", "#ff7a0e", "#b2d3ed"),
## Oddments (2014)
oddments = c("#0092a1", "#f4c00f", "#bd8c9b", "#e25657", "#32826c", "#a0b137", "#221916"),
## I'm in Your Mind Fuzz (2014)
mindfuzz = c("#004316", "#00ba41", "#6bcb78", "#56c5c5", "#ff2d23", "#fad752", "#ff157d"),
## Quarters! (2015)
quarters1 = c("#00b76a", "#ff2825", "#22231c", "#ffc100", "#825087", "#92b7e0", "#0077c0", "#bdb6da"),
## Paper Mâché Dream Balloon (2015)
papermache = c("#3275bd", "#ff5345", "#ffef00", "#ff4792", "#8fbbdc", "#8ea955", "#515818"),
## Nonagon Infinity (2016)
nonagon = c("#a43a25", "#d81f25", "#f0e05a", "#627f63", "#24140d"),
## Flying Microtonal Banana (2017)
microbanana = c("#5354b8", "#d8c426", "#232222"),
## Murder of the Universe (2017)
motu = c("#758046", "#bea959", "#0c6263", "#574f32", "#1a1c19"),
## Sketches of Brunswick East (2017)
sketches = c("#00c6de", "#f9b200", "#ff7700", "#007386", "#594226", "#78706b"),
## Polygondwanaland (2017)
polygondwana = c("#0078ca", "#007e17", "#f60301", "#f6ef00", "#e17000", "#5b3d25", "#282321"),
## Gumboot Soup (2017)
gumboot = c("#ff381c", "#ff7300", "#faf0d2", "#000000"),
## Gizzfest 2017 tour poster
gizzfest2017 = c("#e63930", "#7b437e", "#dfd840", "#007829", "#3b9940", "#005ac8", "#198bbf", "#8f5f19"),
## Gizzfest 2018 tour poster
gizzfest2018 = c("#6b7d15", "#da5697", "#da7d4a", "#ff3b23"),
## Mexico 2018 tour poster
mex2018 = c("#0074c1", "#ff4943", "#f2df3a", "#00943d"),
# Fishing for Fishies (2019)
fff = c("#00adc4", "#ec2022", "#1d9a5f", "#68615f", "#c99353", "#784d41", "#ff861b", "#0070ac"),
# Infest the Rats' Nest + Self-immolate and Organ Farmer (2019)
itrn = c("#7a883a", "#b6a74a", "#010102", "#003e31", "#ff641f", "#ed4820", "#00fa34", "#ff10ff")
)

#' @title King Gizzard and the Lizard Wizard palette generator
#'
#' @description Allows you to use palettes inspired by King Gizzard's artwork by Jason Galea
#'
#' @param n Number of colors. Number of colors available depends on palette, ranging from three colors
#' (microbanana) to eight (quarters1, gizzfest2017, fff, and itrn).
#'
#' @param name Name of palette. List of available palettes:
#' \code{twelvebar}, \code{elts}, \code{floatalong}, \code{oddments},
#' \code{mindfuzz}, \code{quarters1}, \code{papermache}, \code{nonagon},
#' \code{microbanana}, \code{motu}, \code{sketches}, \code{polygondwana},
#' \code{gumboot}, \code{gizzfest2017}, \code{gizzfest2018}, \code{mex2018},
#' \code{fff}, \code{itrn}
#'
#' @param type Choose between "discrete" or "continuous"
#'
#' @return A vector of colors.
#'
#' @examples
#' kgatlw_palette("microbanana")
#' kgatlw_palette("oddments", 4)
#' kgatlw_palette("polygondwana", n = 20, type = "continuous")
#'
#' @export
#'
kgatlw_palette <- function(name, n, type = c("discrete", "continuous")) {

  if (missing(type)){
    type <- "discrete"
  }

  type <- match.arg(type)

  pal <- kgatlw_pal[[name]]
  if (is.null(pal))
    stop("There is no Palette B")

  if (missing(n)) {
    n <- length(pal)
  }

  if (type == "discrete" && n > length(pal)) {
    stop("It's a glitch in the matrix! You chose too many colors, mate")
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}

#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)

  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 2, family = "mono")
}
