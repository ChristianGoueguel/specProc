# Draws the package logo: a laser spark whose beam carries the median
# forageLIBS spectrum around the Ca II H and K lines (383-404 nm). Writes
# man/figures/logo.png (README and pkgdown site), the pkgdown favicons, and
# inst/figures/pkg_sticker.png, the same logo with the repository URL for
# printed stickers. All images are 518 x 600 px or smaller, drawn in pixels
# at 96 dpi. Run from the package root: source("data-raw/logo.R")

library(ggplot2)

systemfonts::register_font(
  "Aller", system.file("fonts/Aller/Aller_Rg.ttf", package = "hexSticker")
)

colors <- c(border = "#F2AA4C", fill = "#000000", title = "#FDD600",
            spark = "#96D76A", line = "#FFFFFF")
width <- 518
height <- 600

# line width of n pixels at 96 dpi
px <- function(n) n / .pt

hexagon <- function(r) {
  a <- pi / 2 + (0:5) * pi / 3
  data.frame(x = width / 2 + r * cos(a), y = height / 2 + r * sin(a))
}

circle <- function(x0, y0, r) {
  a <- seq(0, 2 * pi, length.out = 120)
  data.frame(x = x0 + r * cos(a), y = y0 + r * sin(a))
}

glow <- function(colors, stops) {
  grid::radialGradient(colors, stops = stops, r2 = 0.5)
}

# Spectrum ----------------------------------------------------------------

load("data/forageLIBS.rda")
wl <- suppressWarnings(as.numeric(names(forageLIBS)))
in_window <- !is.na(wl) & wl >= 383 & wl <= 404
intensity <- apply(as.matrix(forageLIBS[in_window]), 2, stats::median)
intensity <- (intensity - min(intensity)) / diff(range(intensity))

baseline <- 186
spectrum <- data.frame(
  x = 140 + 325 * (wl[in_window] - 383) / 21,
  y = baseline + 165 * intensity
)

# Spark -------------------------------------------------------------------

spark <- c(x = 100, y = baseline)
# 16 rays: long on the axes, medium on the diagonals, short in between
angles <- (0:15) * pi / 8
lengths <- ifelse(0:15 %% 4 == 0, 72, ifelse(0:15 %% 2 == 0, 50, 32))
rays <- do.call(rbind, lapply(seq_along(angles), function(k) {
  a <- angles[k]
  w <- 3.2
  data.frame(
    ray = k,
    x = spark[["x"]] + c(-sin(a) * w, sin(a) * w, cos(a) * lengths[k]),
    y = spark[["y"]] + c(cos(a) * w, -cos(a) * w, sin(a) * lengths[k])
  )
}))

# Logo --------------------------------------------------------------------

draw_logo <- function(url = NULL) {
  r <- width / sqrt(3)
  border <- 9
  p <- ggplot() +
    geom_polygon(aes(x, y), data = hexagon(r), fill = colors[["border"]]) +
    geom_polygon(aes(x, y), data = hexagon(r - border / cos(pi / 6)),
                 fill = colors[["fill"]]) +
    # halo of the spark
    geom_polygon(aes(x, y), data = circle(spark[["x"]], spark[["y"]], 95),
                 fill = glow(scales::alpha(colors[["spark"]], c(0.55, 0.15, 0)),
                             c(0, 0.35, 1))) +
    # beam
    annotate("rect", xmin = spark[["x"]], xmax = 475,
             ymin = baseline - 6, ymax = baseline + 6,
             fill = grid::linearGradient(
               scales::alpha(colors[["spark"]], c(0.35, 0.15, 0)),
               stops = c(0, 0.8, 1), x1 = 0, y1 = 0.5, x2 = 1, y2 = 0.5
             )) +
    annotate("rect", xmin = spark[["x"]], xmax = 475,
             ymin = baseline - 1.2, ymax = baseline + 1.2,
             fill = grid::linearGradient(
               c(colors[["line"]], colors[["spark"]], scales::alpha(colors[["spark"]], 0)),
               stops = c(0, 0.8, 1), x1 = 0, y1 = 0.5, x2 = 1, y2 = 0.5
             )) +
    # spectrum, with a green glow
    geom_path(aes(x, y), data = spectrum, colour = colors[["spark"]],
              alpha = 0.12, linewidth = px(11), linejoin = "round") +
    geom_path(aes(x, y), data = spectrum, colour = colors[["spark"]],
              alpha = 0.3, linewidth = px(5.5), linejoin = "round") +
    geom_path(aes(x, y), data = spectrum, colour = colors[["line"]],
              linewidth = px(2.4), linejoin = "round") +
    # spark
    geom_polygon(aes(x, y, group = ray), data = rays,
                 fill = colors[["spark"]], alpha = 0.9) +
    geom_polygon(aes(x, y), data = circle(spark[["x"]], spark[["y"]], 20),
                 fill = glow(c(colors[["line"]], colors[["line"]],
                               scales::alpha(colors[["spark"]], c(0.8, 0))),
                             c(0, 0.2, 0.5, 1))) +
    annotate("text", x = width / 2, y = 424, label = "specProc",
             family = "Aller", colour = colors[["title"]],
             size = 58, size.unit = "pt") +
    coord_fixed(xlim = c(0, width), ylim = c(0, height), expand = FALSE) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
  if (!is.null(url)) {
    # centered on the lower right edge, inside the border
    a <- pi / 6
    p <- p + annotate("text", x = 0.75 * width - 19 * sin(a),
                      y = height / 2 - 0.75 * r + 19 * cos(a), label = url, angle = 30,
                      family = "Aller", colour = colors[["border"]],
                      size = 9, size.unit = "pt")
  }
  p
}

save_logo <- function(plot, file) {
  ggsave(file, plot, device = ragg::agg_png, width = width, height = height,
         units = "px", dpi = 96, bg = "transparent")
}

save_logo(draw_logo(), "man/figures/logo.png")
save_logo(draw_logo(url = "github.com/ChristianGoueguel/specProc"),
          "inst/figures/pkg_sticker.png")

# Favicons ----------------------------------------------------------------

logo <- magick::image_read("man/figures/logo.png")
icon <- function(n) {
  magick::image_extent(magick::image_scale(logo, paste0("x", n)),
                       paste0(n, "x", n), color = "none")
}
for (n in c(60, 76, 120, 152, 180)) {
  magick::image_write(icon(n), sprintf("pkgdown/favicon/apple-touch-icon-%dx%d.png", n, n))
}
magick::image_write(icon(180), "pkgdown/favicon/apple-touch-icon.png")
for (n in c(16, 32)) {
  magick::image_write(icon(n), sprintf("pkgdown/favicon/favicon-%dx%d.png", n, n))
}
magick::image_write(c(icon(48), icon(32), icon(16)), "pkgdown/favicon/favicon.ico",
                    format = "ico")
