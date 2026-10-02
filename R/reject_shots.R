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
#' z-scores (deviation from the median of the sample divided by the MAD) of
#' one or more criteria:
#'  - `"intensity"`: the total intensity of the spectrum. Both weak and
#'    unusually strong shots are flagged.
#'  - `"correlation"`: the Pearson correlation of the spectrum with the
#'    median spectrum of the sample, which detects changes of the shape of
#'    the spectrum (other lines, other line ratios) whatever its intensity.
#'    Only shots with a low correlation are flagged.
#'  - `"distance"`: the Euclidean distance of the spectrum to the median
#'    spectrum of the sample, which combines both. Only far shots are
#'    flagged.
#'
#' A shot is rejected when any criterion exceeds `cutoff` (3.5 by default,
#' following Iglewicz and Hoaglin, 1993). Samples with fewer than 3 shots are
#' not checked. When all shots of a sample vary by the same amount, the MAD
#' can be zero; the mean absolute deviation is then used.
#'
#' To average the kept shots per sample, use [average()] on the result with
#' `drop = TRUE`. In a recipe, use [step_reject_shots()].
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
#'
#' @return A tibble: `data` with a logical column `.rejected`, the criteria
#'   that rejected each shot in `.reason` (`NA` for kept shots) and the
#'   robust z-score of each criterion (`.intensity_z`, `.correlation_z`,
#'   `.distance_z`). With `drop = TRUE`, the kept rows of `data`.
#'
#' @references
#'  - Iglewicz, B., Hoaglin, D.C. (1993). How to Detect and Handle
#'    Outliers. ASQC Quality Press, Milwaukee.
#'
#' @seealso [step_reject_shots()], [average()], [saturation_summary()]
#' @export reject_shots
#'
#' @examples
#' data(forageLIBS)
#' wl <- suppressWarnings(as.numeric(names(forageLIBS)))
#' spectra <- forageLIBS[which(wl > 760 & wl < 780)]  # the K I resonance lines
#' # five shots of each of four samples, one of them with a weak plasma
#' set.seed(1)
#' shots <- spectra[rep(1:4, each = 5), ] * stats::runif(20, 0.95, 1.05)
#' shots[7, ] <- 0.2 * shots[7, ]
#' shots <- cbind(sample = rep(c("A", "B", "C", "D"), each = 5), shots)
#' res <- reject_shots(shots, sample)
#' res[res$.rejected, c("sample", ".reason", ".intensity_z")]
#' # the kept shots only
#' nrow(reject_shots(shots, sample, drop = TRUE))
reject_shots <- function(data, sample, method = c("intensity", "correlation"), cutoff = 3.5,
                         wavelength = NULL, drop = FALSE) {
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
  method <- match.arg(method, shot_criteria, several.ok = TRUE)
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
  flags <- shot_flags(x, data[[sample]], method, cutoff)
  if (drop) {
    return(tibble::as_tibble(data[!flags$rejected, , drop = FALSE]))
  }
  out <- tibble::as_tibble(data)
  out$.rejected <- flags$rejected
  out$.reason <- flags$reason
  for (m in method) out[[paste0(".", m, "_z")]] <- flags$scores[, m]
  out
}

# ---- internals ---------------------------------------------------------------

shot_criteria <- c("intensity", "correlation", "distance")

# Robust z-scores within a sample; NA when the sample is too small.
robust_z <- function(v) {
  if (length(v) < 3) return(rep(NA_real_, length(v)))
  center <- stats::median(v)
  s <- stats::mad(v, center = center)
  if (!is.finite(s) || s <= 0) s <- mean(abs(v - center)) * sqrt(pi / 2)
  if (!is.finite(s) || s <= 0) return(rep(0, length(v)))
  (v - center) / s
}

# Flags the outlying shots of each sample (rows of x grouped by `group`).
shot_flags <- function(x, group, method, cutoff) {
  if (anyNA(group)) {
    stop("The sample column has missing values.", call. = FALSE)
  }
  if (anyNA(x)) {
    stop("The spectra have missing values; impute or remove them first.", call. = FALSE)
  }
  scores <- matrix(NA_real_, nrow(x), length(shot_criteria), dimnames = list(NULL, shot_criteria))
  for (rows in split(seq_len(nrow(x)), as.character(group))) {
    if (length(rows) < 3) next
    xs <- x[rows, , drop = FALSE]
    if ("intensity" %in% method) {
      scores[rows, "intensity"] <- robust_z(rowSums(xs))
    }
    if (any(c("correlation", "distance") %in% method)) {
      ref <- col_medians(xs)
      if ("correlation" %in% method) {
        r <- suppressWarnings(as.vector(stats::cor(t(xs), ref)))
        r[is.na(r)] <- 0   # constant spectra
        scores[rows, "correlation"] <- -robust_z(r)   # low correlation = high score
      }
      if ("distance" %in% method) {
        scores[rows, "distance"] <- robust_z(sqrt(rowSums(sweep(xs, 2, ref)^2)))
      }
    }
  }
  exceed <- cbind(
    intensity = abs(scores[, "intensity"]) > cutoff,
    correlation = scores[, "correlation"] > cutoff,
    distance = scores[, "distance"] > cutoff
  )[, method, drop = FALSE]
  exceed[is.na(exceed)] <- FALSE
  reason <- apply(exceed, 1, function(e) if (any(e)) paste(method[e], collapse = ", ") else NA_character_)
  list(rejected = rowSums(exceed) > 0, reason = reason, scores = scores)
}
