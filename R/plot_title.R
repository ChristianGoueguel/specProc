# Plot titles. Every plot of the package has a bold title, and a title longer
# than `title_width` characters is split, at a natural break, into a shorter
# title and a subtitle (placed above the subtitle the plot may already have).

title_width <- 45

# Splits a long title into a title and a subtitle. The break is, in this
# order of preference, a ": ", " (", " - " or ", " that leaves a title of at
# most `width` characters (the last such one of the preferred separator); if
# none does, the first of these separators; and without any, the last space
# that does. For " (", the matching closing parenthesis is dropped from the
# subtitle.
split_title <- function(title, subtitle = NULL, width = title_width) {
  if (!is.character(title) || length(title) != 1L || is.na(title) || nchar(title) <= width) {
    return(list(title = title, subtitle = subtitle))
  }
  separators <- c(": ", " (", " - ", ", ")
  breaks <- unlist(lapply(separators, function(s) {
    at <- gregexpr(s, title, fixed = TRUE)[[1]]
    if (at[1] > 1) stats::setNames(at, rep(s, length(at)))
  }))
  if (length(breaks) > 0) {
    fitting <- breaks[breaks - 1 <= width]
    cut <- if (length(fitting) > 0) {
      preferred <- fitting[names(fitting) == separators[separators %in% names(fitting)][1]]
      preferred[which.max(preferred)]
    } else {
      breaks[which.min(breaks)]
    }
    separator <- names(cut)
    head <- substr(title, 1, cut - 1)
    tail <- substr(title, cut + nchar(separator), nchar(title))
    if (separator == " (") {
      # drop the parenthesis that closes the one at the break
      close <- regexpr(")", tail, fixed = TRUE)
      if (close > 0 && !grepl("(", substr(tail, 1, close), fixed = TRUE)) {
        tail <- paste0(substr(tail, 1, close - 1), substr(tail, close + 1, nchar(tail)))
      }
    }
  } else {
    spaces <- gregexpr(" ", title, fixed = TRUE)[[1]]
    spaces <- spaces[spaces > 1 & spaces - 1 <= width]
    if (length(spaces) == 0) {
      return(list(title = title, subtitle = subtitle))
    }
    cut <- max(spaces)
    head <- substr(title, 1, cut - 1)
    tail <- substr(title, cut + 1, nchar(title))
  }
  has_subtitle <- !is.null(subtitle) && !(is.character(subtitle) && !any(nzchar(subtitle)))
  list(title = head, subtitle = if (has_subtitle) paste0(tail, "\n", subtitle) else tail)
}

bold_title <- function() {
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

# Splits the title of a finished ggplot if it is long, and makes it bold.
# Called last, after any complete theme (such as theme_bw()), which would
# otherwise reset the title style.
finish_title <- function(p) {
  lab <- split_title(p$labels$title, p$labels$subtitle)
  if (!is.null(lab$title)) {
    p <- p + ggplot2::labs(title = lab$title, subtitle = lab$subtitle)
  }
  p + bold_title()
}

# The title of a plotly figure: bold, with the subtitle below in a smaller
# font.
plotly_title <- function(title, subtitle = NULL) {
  lab <- split_title(title, subtitle)
  if (is.null(lab$title)) {
    return(NULL)
  }
  text <- paste0("<b>", lab$title, "</b>")
  if (!is.null(lab$subtitle)) {
    text <- paste0(text, "<br><sup>", gsub("\n", "<br>", lab$subtitle, fixed = TRUE), "</sup>")
  }
  list(text = text)
}
