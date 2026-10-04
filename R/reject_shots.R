#' @title Rejection of Outlying Laser Shots
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Flags the outlying shots (spectra) of each sample of a LIBS dataset, such
#' as shots with a weak or missed plasma, or shots on an inclusion or a
#' contaminated spot, before the shots are averaged.
#'
#' @details
#' Each shot is compared with the other shots of its sample, by robust
#' z-scores of one or more criteria:
#'  - `"intensity"`: the total intensity of the spectrum, on a log scale (its
#'    deviation is relative to the median shot of the sample). Both weak and
#'    unusually strong shots are flagged.
#'  - `"correlation"`: the Pearson correlation \eqn{r} of the spectrum with
#'    the median spectrum of the sample, which detects changes of the shape of
#'    the spectrum (other lines, other line ratios) whatever its intensity.
#'    The z-scores are those of Fisher's \eqn{z = \tanh^{-1}(r)}: correlations
#'    near 1 are bounded and skewed, so that small differences such as 0.998
#'    and 0.9995 would otherwise look extreme. Only shots with a low
#'    correlation are flagged.
#'  - `"distance"`: the Euclidean distance of the spectrum to the median
#'    spectrum of the sample, relative to the norm of the median spectrum, on
#'    a log scale, which combines both. Only far shots are flagged.
#'
#' **Scale.** A z-score is the deviation of a shot from the median of its
#' sample divided by a robust scale (the MAD). With the few shots of a sample
#' (often 5 to 10), the MAD of the sample is unstable: when its shots happen
#' to be very similar, a shot that differs by little gets a large z-score.
#' With `scale = "floor"` (default), the scale of a sample is its own MAD, but
#' not less than the pooled MAD of the deviations of all the samples (the
#' typical shot-to-shot variability of the data set); `"sample"` uses the MAD
#' of each sample alone, and `"pooled"` the pooled MAD for all samples, which
#' suits samples of similar variability. In simulations of 8 shots per sample
#' with noise varying between samples, `"floor"` with Fisher's \eqn{z}
#' rejected 0.1% to 0.5% of the regular shots (against 2.5% to 5% with the
#' MAD of each sample and the raw correlations), and still detected the shots
#' of a plasma weakened by a quarter or more, or with an extra line of 10% of
#' the strongest line. Lower `cutoff` to detect milder deviations.
#'
#' A shot is rejected when any criterion exceeds `cutoff` (3.5 by default,
#' following Iglewicz and Hoaglin, 1993). Samples with fewer than 3 shots are
#' not checked. When a MAD is zero, the mean absolute deviation is used.
#'
#' The result keeps the raw criteria (`.intensity`, the total intensity
#' relative to the median shot of the sample; `.correlation`; `.distance`),
#' easier to read than the z-scores, and the settings, which
#' [plot_shots()] uses to show the shots and the rejections. To average the
#' kept shots per sample, use [average()] on the result with `drop = TRUE`. In
#' a recipe, use [step_reject_shots()].
#'
#' @param data A data frame with one shot per row: a sample column and
#'   spectral columns named by their wavelengths (other columns are kept
#'   but not used).
#' @param sample The column identifying the sample of each shot, unquoted or
#'   as a string.
#' @param method The criteria: one or more of `"intensity"`,
#'   `"correlation"` (both by default) and `"distance"`.
#' @param cutoff The robust z-score above which a shot is rejected. Default
#'   is 3.5.
#' @param wavelength An optional wavelength range (nm) in which the criteria
#'   are computed, for example to avoid saturated lines.
#' @param drop A logical: return only the kept shots, without the added
#'   columns (`FALSE`, default).
#' @param shot An optional column (unquoted or as a string) giving the order
#'   of the shots in each sample, such as the shot number. Default is the
#'   order of the rows.
#' @param scale The robust scale of the z-scores: `"floor"` (default),
#'   `"sample"` or `"pooled"` (see Details).
#'
#' @return A tibble: `data` with the shot number in its sample `.shot`, a
#'   logical column `.rejected`, the criteria that rejected each shot in
#'   `.reason` (`NA` for kept shots), the raw criteria `.intensity`,
#'   `.correlation` and `.distance`, and the robust z-score of each criterion
#'   used (`.intensity_z`, `.correlation_z`, `.distance_z`). Its attribute
#'   `"reject_shots"` holds the settings. With `drop = TRUE`, the kept rows of
#'   `data`.
#'
#' @references
#'  - Iglewicz, B., Hoaglin, D.C. (1993). How to Detect and Handle
#'    Outliers. ASQC Quality Press, Milwaukee.
#'  - Fisher, R.A. (1915). Frequency distribution of the values of the
#'    correlation coefficient in samples from an indefinitely large
#'    population. Biometrika, 10(4):507-521.
#'
#' @seealso [plot_shots()], [step_reject_shots()], [average()],
#'   [saturation_summary()], [forageShots]
#' @export reject_shots
#'
#' @examples
#' data(forageShots)
#' # 8 shots of each of 20 forage samples
#' res <- reject_shots(forageShots, Measurement, shot = shot)
#' res[res$.rejected, c("Measurement", ".shot", ".reason", ".intensity", ".correlation")]
#' # the kept shots only
#' nrow(reject_shots(forageShots, Measurement, drop = TRUE))
#' # the former behavior, with the MAD of each sample alone
#' sum(reject_shots(forageShots, Measurement, scale = "sample")$.rejected)
reject_shots <- function(data, sample, method = c("intensity", "correlation"), cutoff = 3.5,
                         wavelength = NULL, drop = FALSE, shot = NULL,
                         scale = c("floor", "sample", "pooled")) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame with one shot per row.", call. = FALSE)
  }
  if (missing(sample)) {
    stop("'sample' must name the column identifying the sample of each shot.", call. = FALSE)
  }
  sample <- rlang::as_name(rlang::enquo(sample))
  if (!sample %in% names(data)) {
    stop("Column `", sample, "` not found in 'data'.", call. = FALSE)
  }
  shot_quo <- rlang::enquo(shot)
  shot <- if (rlang::quo_is_null(shot_quo)) NULL else rlang::as_name(shot_quo)
  if (!is.null(shot) && !shot %in% names(data)) {
    stop("Column `", shot, "` not found in 'data'.", call. = FALSE)
  }
  method <- match.arg(method, shot_criteria, several.ok = TRUE)
  scale <- match.arg(scale)
  check_number(cutoff, "cutoff", lower = 0, lower_open = TRUE)
  check_flag(drop, "drop")
  wl <- suppressWarnings(as.numeric(names(data)))
  channels <- !is.na(wl) & vapply(data, is.numeric, logical(1))
  if (!is.null(wavelength)) {
    check_wavelength_range(wavelength)
    channels <- channels & !is.na(wl) & wl >= wavelength[1] & wl <= wavelength[2]
  }
  if (sum(channels) < 2) {
    stop("'data' needs at least 2 spectral columns named by their wavelengths",
         if (!is.null(wavelength)) " in the wavelength range", ".", call. = FALSE)
  }
  x <- as.matrix(data[channels])
  storage.mode(x) <- "double"
  flags <- shot_flags(x, data[[sample]], method, cutoff, scale)
  if (drop) {
    return(tibble::as_tibble(data[!flags$rejected, , drop = FALSE]))
  }
  out <- tibble::as_tibble(data)
  out$.shot <- if (is.null(shot)) {
    stats::ave(seq_len(nrow(data)), as.character(data[[sample]]), FUN = seq_along)
  } else {
    data[[shot]]
  }
  out$.rejected <- flags$rejected
  out$.reason <- flags$reason
  out$.intensity <- flags$raw[, "intensity"]
  out$.correlation <- flags$raw[, "correlation"]
  out$.distance <- flags$raw[, "distance"]
  for (m in method) out[[paste0(".", m, "_z")]] <- flags$scores[, m]
  attr(out, "reject_shots") <- list(sample = sample, shot = shot, method = method,
                                    cutoff = cutoff, scale = scale,
                                    channels = names(data)[channels])
  out
}

# ---- internals ---------------------------------------------------------------

shot_criteria <- c("intensity", "correlation", "distance")

# Robust z-scores of `v` within the groups `group` (samples of at least 3
# shots; NA for the others): the deviation from the median of the group,
# divided by its MAD ("sample"), the pooled MAD of all the groups
# ("pooled"), or the larger of the two ("floor").
robust_z <- function(v, group, scale = "floor") {
  z <- rep(NA_real_, length(v))
  rows <- split(seq_along(v), as.character(group))
  rows <- rows[lengths(rows) >= 3]
  if (length(rows) == 0) return(z)
  mad_of <- function(e) {
    s <- 1.4826 * stats::median(abs(e))
    if (!is.finite(s) || s <= 0) s <- mean(abs(e)) * sqrt(pi / 2)
    s
  }
  deviation <- lapply(rows, function(r) v[r] - stats::median(v[r]))
  pooled <- mad_of(unlist(deviation))
  for (g in names(rows)) {
    e <- deviation[[g]]
    s <- switch(scale, sample = mad_of(e), pooled = pooled, floor = max(mad_of(e), pooled))
    z[rows[[g]]] <- if (is.finite(s) && s > 0) e / s else 0
  }
  z
}

# Flags the outlying shots of each sample (rows of x grouped by `group`):
# the raw criteria, their robust z-scores, and the rejected shots.
shot_flags <- function(x, group, method, cutoff, scale = "floor") {
  if (anyNA(group)) {
    stop("The sample column has missing values.", call. = FALSE)
  }
  if (anyNA(x)) {
    stop("The spectra have missing values; impute or remove them first.", call. = FALSE)
  }
  n <- nrow(x)
  raw <- matrix(NA_real_, n, length(shot_criteria), dimnames = list(NULL, shot_criteria))
  total <- rowSums(x)
  for (rows in split(seq_len(n), as.character(group))) {
    xs <- x[rows, , drop = FALSE]
    ref <- col_medians(xs)
    raw[rows, "intensity"] <- total[rows] / stats::median(total[rows])
    r <- suppressWarnings(as.vector(stats::cor(t(xs), ref)))
    r[is.na(r)] <- 0   # constant spectra
    raw[rows, "correlation"] <- r
    norm <- sqrt(sum(ref^2))
    raw[rows, "distance"] <- sqrt(rowSums(sweep(xs, 2, ref)^2)) / if (norm > 0) norm else 1
  }
  # comparable scales across samples: log intensity (relative deviations),
  # Fisher's z of the correlation, log distance
  value <- cbind(
    intensity = if (all(total > 0)) log(total) else raw[, "intensity"],
    correlation = atanh(pmin(pmax(raw[, "correlation"], -1 + 1e-12), 1 - 1e-12)),
    distance = log(raw[, "distance"] + 1e-12)
  )
  scores <- matrix(NA_real_, n, length(shot_criteria), dimnames = list(NULL, shot_criteria))
  for (m in method) scores[, m] <- robust_z(value[, m], group, scale)
  scores[, "correlation"] <- -scores[, "correlation"]   # low correlation = high score
  exceed <- cbind(
    intensity = abs(scores[, "intensity"]) > cutoff,
    correlation = scores[, "correlation"] > cutoff,
    distance = scores[, "distance"] > cutoff
  )[, method, drop = FALSE]
  exceed[is.na(exceed)] <- FALSE
  reason <- apply(exceed, 1, function(e) if (any(e)) paste(method[e], collapse = ", ") else NA_character_)
  list(rejected = rowSums(exceed) > 0, reason = reason, scores = scores, raw = raw)
}
