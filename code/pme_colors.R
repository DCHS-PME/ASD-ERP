# ---- Palette Registry -----------------------------------------------------------

# Registry of palettes, grouped by type in display order. Note that wrappers accept "grey"
# as an alternative.

.brand_palettes <- list(
  qualitative = list(
    primary    = c("#1E295A", "#C0277A", "#FFE067", "#4A7BA7", "#636569"),
    secondary  = c("#A6533C", "#73CAE1", "#BFBFBF"),
    background = c("#FFFFFF", "#F8F4EA")
  ),
  sequential = list(
    navy   = c("#C5CAE0", "#98A2C8", "#707DAF", "#4E5F8F", "#1E295A", "#151D3D", "#0D1220"),
    gray   = c("#E5E5E6", "#CBCCCD", "#9A9B9D", "#7E7F82", "#636569", "#4A4B4E", "#323335"),
    blue   = c("#C4D9E8", "#8FB7D5", "#6B9EC7", "#578DB7", "#4A7BA7", "#3A6186", "#2C4A66"),
    pink   = c("#EDA8CB", "#E22F90", "#C0277A", "#9E1F64", "#7D184F"),
    yellow = c("#FFF2B8", "#FFE067", "#E6C640")
  ),
  diverging = list(
    blue_pink = c("#2C4A66", "#4A7BA7", "#8FB7D5", "#E5E5E6", "#EDA8CB", "#E22F90", "#7D184F"),
    navy_pink = c("#374576", "#707DAF", "#A9B1D1", "#E5E5E6", "#EDA8CB", "#E22F90", "#7D184F")
  )
)

# ---- Validators -----------------------------------------------------------

.check_pal <- function(pal, choices) {
    
  if (!rlang::is_string(pal) || !(pal %in% choices)) {

    cli::cli_abort(c(
      "{.arg pal} must be one of {.val {choices}}.",
      "x" = "You supplied {.val {pal}}."
    ))

  }

}
 
.check_alpha <- function(a) {

  if (!(is.numeric(a) && length(a) == 1L && !is.na(a) && a >= 0 && a <= 1)) {
    cli::cli_abort("{.arg a} must be a single number between 0 and 1.")
  }

}
 
.check_color_code <- function(color_code) {
    
  if (!(rlang::is_string(color_code) && color_code %in% c("hex", "rgb"))) {
    cli::cli_abort("{.arg color_code} must be either {.val hex} or {.val rgb}.")
  }

}
 
.check_n <- function(n) {

  if (!(is.numeric(n) && length(n) == 1L && !is.na(n) && n >= 0 && n %% 1 == 0)) {
    cli::cli_abort("{.arg n} must be a single whole number that is zero or greater.")

  }
}
 
.check_ramp <- function(ramp) {

  if (!(is.logical(ramp) && length(ramp) == 1L && !is.na(ramp))) {
    cli::cli_abort("{.arg ramp} must be a single {.code TRUE} or {.code FALSE} value.")
  }

}

# ---- Private Internal Functions -----------------------------------------------------------

# Convenience function to interpolate a palette, with n returning the palette unchanged.
.interpolate <- function(cols, n) {

  if (n > 0) {
    cols <- grDevices::colorRampPalette(cols)(n)
  }

  cols

}

# Apply transparency and format the conversion
# Alpha is only applied if less than 1

.finalize_colors <- function(cols, a, color_code) {

  if (a < 1) {
    cols <- grDevices::adjustcolor(cols, alpha.f = a)
  }

  if (color_code == "rgb") {

    cols <- as.list(as.data.frame(grDevices::col2rgb(cols, alpha = a < 1)))
  }

  cols

}

# ----- Wrappers --------------------------------------------------------------------------------------------

#' Qualitative brand colors
#'
#' Returns a qualitative (categorical) brand palette. 
#'
#' Note: The "rusty orange" (`#A6533C`, in the secondary palette) is reserved for
#' accents.
#'
#' @param pal One of `"primary"`, `"secondary"`, or `"background"`.
#' @param a Transparency, a single number in `[0, 1]`. Default `1` (opaque).
#' @param color_code Either `"hex"` or `"rgb"`. Default `"hex"`.
#' @return For `"hex"`, a character vector of colors. For `"rgb"`, a list with
#'   one element per color, each a numeric vector of rgb values (an alpha
#'   value is included only when `a < 1`).
#' @export
#' @examples
#' load_qual_colors()
#' load_qual_colors(pal = "secondary")
#' load_qual_colors(pal = "background", a = 0.4, color_code = "rgb")

load_qual_colors <- function(pal = "primary", a = 1, color_code = "hex") {

  .check_alpha(a)
  .check_color_code(color_code)
  .check_pal(pal, names(.brand_palettes$qualitative))
 
  cols <- .brand_palettes$qualitative[[pal]]
  .finalize_colors(cols, a, color_code)

}
 
#' Sequential brand colors
#'
#' Returns a sequential brand gradient. By default the palette is returned as
#' defined in the brand guide; set `n` to interpolate to a different number of
#' grades, or `ramp = TRUE` to return a palette-generating function.
#'
#' @param pal One of `"navy"`, `"gray"` (or `"grey"`), `"blue"`, `"pink"`, or
#'   `"yellow"`.
#' @param n Number of tonal grades. `0` (the default) returns the palette as
#'   defined in the brand guide; a positive integer interpolates to `n` colors.
#' @param a Transparency, a single value in `[0, 1]`. Default `1` (opaque).
#' @param ramp Logical. If `TRUE`, returns a `colorRampPalette()` function over
#'   the base palette (producing opaque hex); `a` and `color_code` are ignored.
#'   Default `FALSE`.
#' @param color_code Either `"hex"` or `"rgb"`. Default `"hex"`.
#' @return If `ramp = TRUE`, a function of one argument. Otherwise a list of
#' colors
#' @export
#' @examples
#' load_sequential_colors("navy")
#' load_sequential_colors("blue", n = 10)
#' brand_ramp <- load_sequential_colors("pink", ramp = TRUE)
#' brand_ramp(4)

load_sequential_colors <- function(pal = "navy", n = 0, a = 1,
                                   ramp = FALSE, color_code = "hex") {

  # accept British spelling (because the function creator always forgets the American version)
  if (identical(pal, "grey")) pal <- "gray"   
 
  .check_alpha(a)
  .check_color_code(color_code)
  .check_n(n)
  .check_ramp(ramp)
  .check_pal(pal, names(.brand_palettes$sequential))
 
  cols <- .brand_palettes$sequential[[pal]]
 
  if (isTRUE(ramp)) {
    return(grDevices::colorRampPalette(cols))
  }
 
  cols <- .interpolate(cols, n)
  .finalize_colors(cols, a, color_code)

}
 
#' Diverging brand colors
#'
#' Returns a diverging brand palette that runs through a neutral midpoint.
#' Interpolation (`n`) and `ramp` run across the range, preserving the same
#' midpoint.
#'
#' @param pal One of `"blue_pink"` or `"navy_pink"`.
#' @param n Number of grades. `0` (the default) returns the palette as defined
#; a positive integer interpolates to `n` colors.
#' @param a Transparency, a single value in `[0, 1]`. Default `1` (opaque).
#' @param ramp Logical. If `TRUE`, returns a `colorRampPalette()` function over
#'   the base palette (producing opaque hex); `a` and `color_code` are ignored.
#'   Default `FALSE`.
#' @param color_code Either `"hex"` or `"rgb"`. Default `"hex"`.
#' @return If `ramp = TRUE`, a function of one argument. Otherwise, a list of colors
#' @export
#' @examples
#' load_diverging_colors()
#' load_diverging_colors("navy_pink", n = 11)
#' 
load_diverging_colors <- function(pal = "blue_pink", n = 0, a = 1,
                                  ramp = FALSE, color_code = "hex") {

  .check_alpha(a)
  .check_color_code(color_code)
  .check_n(n)
  .check_ramp(ramp)
  .check_pal(pal, names(.brand_palettes$diverging))
 
  cols <- .brand_palettes$diverging[[pal]]
 
  if (isTRUE(ramp)) {
    return(grDevices::colorRampPalette(cols))
  }
 
  cols <- .interpolate(cols, n)
  .finalize_colors(cols, a, color_code)

}

#' Preview brand palettes
#'
#' Draws a reference chart of the brand palettes grouped by type
#' (qualitative, sequential, diverging). Each type is shown as a 
#' header, with individual palette names indented beneath it and that
#' palette's colors drawn as a row of evenly spaced swatches.
#'
#' @param types Character vector; any of `"qualitative"`, `"sequential"`,
#'   `"diverging"`. Defaults to all three, always drawn in that order.
#' @param show_hex Logical; if `TRUE`, prints each hex code beneath its swatch.
#'   Defaults to `FALSE`.
#' @param swatch_border Border color for swatches, so near-white colors stay
#'   visible on a white background. Defaults to `"grey70"`.
#' @return Invisibly returns `NULL`; called for its plotting side effect.
#' @export
#' @examples
#' preview_brand_palettes()
#' preview_brand_palettes(types = "sequential", show_hex = TRUE)

preview_brand_palettes <- function(types = c("qualitative", "sequential", "diverging"),
                                   show_hex = FALSE,
                                   swatch_border = "grey70") {
 
  # I've chosen to use base R to spare the package another dependency. Adding ggplot2 
  # would not make creation of this figure any easier, given the nested labels.

  all_types <- c("qualitative", "sequential", "diverging")

  if (!all(types %in% all_types)) {
    cli::cli_abort("{.arg types} must be one or more of {.val {all_types}}.")
  }

  # Make sure to present types in the same order regardless of how many are chosen to be displayed
  types <- intersect(all_types, types)      
  reg   <- .brand_palettes[types]
 
  # Flatten into an ordered list of rows for header (type) and palette

  rows <- list()

  for (ty in names(reg)) {
    rows[[length(rows) + 1L]] <- list(kind = "header", label = ty)

    for (nm in names(reg[[ty]])) {
      rows[[length(rows) + 1L]] <- list(kind = "pal", label = nm,
                                        colors = reg[[ty]][[nm]])
    }

  }

  n_rows <- length(rows)

  # Ensure there's atleast 1 column
  max_k  <- max(vapply(reg, function(g) max(vapply(g, length, integer(1))), integer(1)))
 
  # Layout

  # Add breathing room
  swatch_x0 <- 0.28

  # Evenly space the swatches
  cell_w    <- (0.99 - swatch_x0) / max_k

  # Add some whitespace between swatches     
  swatch_w  <- cell_w * 0.88                  
  swatch_h  <- 0.60

  row_h     <- if (show_hex) 1.25 else 1.0
 
  op <- graphics::par(mar = c(0.5, 0.5, 2.0, 0.5))
  on.exit(graphics::par(op), add = TRUE)
 
  graphics::plot.new()

  # Turn off the default padding for yaxs and decrement the vertical range
  graphics::plot.window(xlim = c(0, 1),
                        ylim = c(-n_rows * row_h, 0), yaxs = "i")

  graphics::title(main = "Brand Palettes", adj = 0, cex.main = 1.3, font.main = 2)
 
  for (i in seq_len(n_rows)) {

    r  <- rows[[i]]
    yc <- -(i - 0.5) * row_h
 
    if (r$kind == "header") {
      graphics::text(0.005, yc, labels = toupper(r$label),
                     adj = c(0, 0.5), font = 2, cex = 1.02, col = "grey15")
    } else {
      graphics::text(0.04, yc, labels = r$label,
                     adj = c(0, 0.5), font = 1, cex = 0.92, col = "grey25")

      for (j in seq_along(r$colors)) {
        x0 <- swatch_x0 + (j - 1) * cell_w

        graphics::rect(x0, yc - swatch_h / 2,
                       x0 + swatch_w, yc + swatch_h / 2,
                       col = r$colors[j], border = swatch_border, lwd = 0.7)

        if (show_hex) {

          graphics::text(x0 + swatch_w / 2, yc - swatch_h / 2 - 0.10,
                         labels = r$colors[j], adj = c(0.5, 1),
                         cex = 0.42, col = "grey40")
                         
        }
      }
    }
  }

  invisible(NULL)

}