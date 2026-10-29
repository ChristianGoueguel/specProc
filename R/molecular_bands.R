#' @title Band Heads of Diatomic Molecules
#'
#' @author Christian L. Goueguel
#'
#' @description
#' Lists the band heads of the electronic systems of diatomic molecules that
#' emit in laser-induced plasmas, with their Franck-Condon factors and
#' relative intensities at a vibrational temperature, to identify the
#' molecular bands of a spectrum: CN, C2, CH, OH, NH, N2, N2+, NO, CO, AlO,
#' CaF, CaCl, SrF, SrCl, MgO, TiO, BO and SiO. The bands are computed from
#' spectroscopic constants included in specProc, without a download.
#'
#' @details
#' The origin of the band from the vibrational level \eqn{v'} of the upper
#' state to \eqn{v''} of the lower state is
#' \deqn{\nu_0 = \nu_{00} + G'(v') - G'(0) - G''(v'') + G''(0)}
#' with the measured origin \eqn{\nu_{00}} of the 0-0 band and the
#' vibrational terms
#' \eqn{G(v) = \omega_e (v + 1/2) - \omega_e x_e (v + 1/2)^2 + \omega_e y_e (v + 1/2)^3}.
#' The head is where the P or R branch turns back,
#' \deqn{\nu_{head} = \nu_0 - \frac{(B_{v'} + B_{v''})^2}{4 (B_{v'} - B_{v''})}}
#' with the rotational constants \eqn{B_v = B_e - \alpha_e (v + 1/2)}. A band
#' is degraded to the red when \eqn{B_{v'} < B_{v''}} (its head is on the
#' violet side) and to the violet otherwise. When the head would form at a
#' rotational level above twice the most populated one at `temperature`, as
#' in the bands of hydrides such as CH and NH, the band has no head: its
#' strongest feature is the Q branch near the origin, and `wavelength` is
#' the origin. The spin-orbit components of \eqn{\Pi}, \eqn{\Delta} and
#' \eqn{\Phi} states are separate sub-bands when their origins are known;
#' other fine structure is ignored.
#'
#' The Franck-Condon factor \eqn{q_{v'v''}} is the squared overlap of the
#' vibrational wavefunctions of Morse potentials (harmonic when the
#' anharmonicity is not known). The relative intensity of a band is
#' \deqn{I \propto q_{v'v''}\, \nu^4 \exp\left(-\frac{hc\,[G'(v') - G'(0)]}{k_B T}\right)}
#' (in energy units, with a constant electronic transition moment),
#' normalized to the strongest band of each system. Intensities of
#' different systems or molecules are not comparable: that would need their
#' transition moments and the populations of their upper states. When the
#' rotational constants of a state are not known, only the bands with
#' \eqn{v' = v''} are listed, without Franck-Condon factor or head.
#'
#' The heads agree with measured heads to about 0.05 nm for well-studied
#' systems, such as the CN violet, C2 Swan, N2 second positive, N2+ first
#' negative and AlO B-X systems. They are less accurate for bands of high
#' \eqn{v}, perturbed states and hydrides, whose rotational structure is
#' open. [molecular_spectrum()] simulates the rotational structure of the
#' bands from a line list.
#'
#' The constants are those compiled by Huber and Herzberg (1979), from the
#' NIST Chemistry WebBook. Wavelengths are in air between 200 and 2000 nm
#' and in vacuum outside this range, as in [nist_lines()].
#'
#' @param molecule A character vector of molecules, such as `c("CN", "C2")`,
#'   or `NULL` (default) for all of them.
#' @param wavelength A numeric vector of length 2: the wavelength range, in
#'   nm. Default is 200 to 900 nm.
#' @param temperature The vibrational temperature, in K, for the relative
#'   intensities and the formation of the heads. Default is 5000.
#' @param min_relative The minimum relative intensity of the bands kept,
#'   between 0 and 1. Default is 0.01.
#'
#' @return A tibble with one row per band, sorted by wavelength:
#'   `molecule`, `system` (such as `"B-X"`), its `name`, the `upper` and
#'   `lower` electronic states, the vibrational quantum numbers `v_upper` and
#'   `v_lower`, the `wavelength` of the head (nm), the `origin` of the band
#'   (nm), the direction in which the band is `degraded` (`"red"` or
#'   `"violet"`), whether it has a `head`, the Franck-Condon factor `fcf` and
#'   the `relative_intensity`.
#'
#' @references
#'  - Huber, K.P., Herzberg, G. (1979). Molecular Spectra and Molecular
#'    Structure IV. Constants of Diatomic Molecules. Van Nostrand Reinhold,
#'    New York. Data from the NIST Chemistry WebBook, NIST Standard Reference
#'    Database 69, \url{https://webbook.nist.gov/chemistry/}.
#'  - Herzberg, G. (1950). Molecular Spectra and Molecular Structure I.
#'    Spectra of Diatomic Molecules, 2nd edition. Van Nostrand, Princeton.
#'
#' @seealso [plot_lines()] and [line_finder()] to mark the bands on a
#'   spectrum, [molecular_spectrum()], [libs_lines()]
#' @export molecular_bands
#'
#' @examples
#' # the CN violet and C2 Swan bands
#' molecular_bands(c("CN", "C2"), wavelength = c(380, 520), min_relative = 0.05)
#'
#' # the CN bands in the mean spectrum of the forage samples
#' data(forageLIBS)
#' spectrum <- colMeans(forageLIBS[-(1:14)])
#' wl <- as.numeric(names(spectrum))
#' plot_lines(spectrum[wl > 383 & wl < 390], bands = molecular_bands("CN"),
#'            interactive = FALSE)
molecular_bands <- function(molecule = NULL, wavelength = c(200, 900), temperature = 5000,
                            min_relative = 0.01) {
  constants <- molecular_constants()
  if (!is.null(molecule)) {
    if (!is.character(molecule) || length(molecule) == 0 || anyNA(molecule)) {
      stop("'molecule' must be a character vector, such as c(\"CN\", \"C2\").", call. = FALSE)
    }
    unknown <- setdiff(molecule, constants$molecule)
    if (length(unknown) > 0) {
      stop("Unknown molecule(s): ", paste(unknown, collapse = ", "), ". Available: ",
           paste(unique(constants$molecule), collapse = ", "), ".", call. = FALSE)
    }
    constants <- constants[constants$molecule %in% molecule, ]
  }
  if (!is.numeric(wavelength) || length(wavelength) != 2 || anyNA(wavelength) || any(wavelength <= 0)) {
    stop("'wavelength' must be a numeric vector of length 2 (nm).", call. = FALSE)
  }
  check_number(temperature, "temperature", lower = 0, lower_open = TRUE)
  check_number(min_relative, "min_relative", lower = 0, upper = 1)
  bands <- do.call(rbind, lapply(seq_len(nrow(constants)), function(i) {
    system_bands(constants[i, ], temperature)
  }))
  # relative intensities within each system (its sub-bands included)
  group <- paste(bands$molecule, bands$system)
  bands$relative_intensity <- bands$strength / stats::ave(bands$strength, group, FUN = max)
  bands$strength <- NULL
  keep <- bands$wavelength >= min(wavelength) & bands$wavelength <= max(wavelength) &
    bands$relative_intensity >= min_relative
  out <- tibble::as_tibble(bands[keep, ])
  out[order(out$wavelength), ]
}

# ---- internals ---------------------------------------------------------------

# Spectroscopic constants of the systems, one row per sub-band (Huber and
# Herzberg, 1979, from the NIST Chemistry WebBook): the origin of the 0-0
# band nu00 (cm-1), the shading of the bands (V or R) and, for the upper (_u)
# and lower (_l) states, we, wexe, weye, Be and ae (cm-1) and re (angstrom).
# Missing values are left empty. v_max is the highest upper level listed.
molecular_constants <- function() {
  if (is.null(molecular_cache$constants)) {
    molecular_cache$constants <- utils::read.csv(text = "
molecule,system,name,upper,lower,nu00,shading,we_u,wexe_u,weye_u,Be_u,ae_u,re_u,we_l,wexe_l,weye_l,Be_l,ae_l,re_l,v_max,atoms
CN,B-X,violet system,B 2Sigma+,X 2Sigma+,25797.84,V,2163.9,20.2,,1.973,0.023,1.15,2068.59,13.087,-0.009093,1.89974,0.017369,1.17182,5,C N
C2,d-a,Swan system,d 3Pi_g,a 3Pi_u,19378.44,V,1788.22,16.440,-0.5067,1.7527,0.01608,1.2661,1641.35,11.67,,1.63246,0.01661,1.31190,6,C C
C2,C-A,Deslandres-d'Azambuja system,C 1Pi_g,A 1Pi_u,25969.19,V,1809.1,15.81,,1.7834,0.018,1.2552,1608.35,12.078,-0.0119,1.6134,0.01686,1.31843,4,C C
CH,A-X,4300 A system,A 2Delta,X 2Pi,23217.5,V,2930.7,96.65,,14.934,0.697,1.1019,2858.5,63.02,,14.457,0.534,1.1199,3,C H
CH,B-X,3900 A system,B 2Sigma-,X 2Pi,25698.2,R,1794.9,,,12.645,,1.1975,2858.5,63.02,,14.457,0.534,1.1199,2,C H
CH,C-X,3150 A system,C 2Sigma+,X 2Pi,31778.1,V,2840.2,125.96,13.55,14.603,0.7185,1.1143,2858.5,63.02,,14.457,0.534,1.1199,3,C H
OH,A-X,ultraviolet system,A 2Sigma+,X 2Pi,32402.39,R,3178.86,92.917,,17.358,0.7868,1.0121,3737.761,84.8813,,18.9108,0.7242,0.96966,3,O H
NH,A-X,3360 A system,A 3Pi,X 3Sigma-,29776.76,,3231.2,98.6,,16.6745,0.7454,1.03698,3282.27,78.35,,16.6993,0.6490,1.03621,3,N H
N2,C-B,second positive system,C 3Pi_u,B 3Pi_g,29671.0,V,2047.178,28.4450,,1.82473,0.01868,1.14869,1733.39,14.122,-0.0569,1.63745,0.01791,1.21260,4,N N
N2,B-A,first positive system,B 3Pi_g,A 3Sigma_u+,9552.03,V,1733.39,14.122,-0.0569,1.63745,0.01791,1.21260,1460.64,13.872,0.0103,1.4546,0.0180,1.2866,12,N N
N2+,B-X,first negative system,B 2Sigma_u+,X 2Sigma_g+,25566.04,V,2419.84,23.189,-0.5375,2.07456,0.024,1.0742,2207.00,16.10,-0.040,1.93176,0.01881,1.11642,4,N N
NO,A-X,gamma system,A 2Sigma+,X 2Pi1/2,44200.2,V,2374.31,10.106,-0.0465,1.9965,0.01915,1.06434,1904.204,14.075,,1.67195,0.0171,1.15077,3,N O
NO,A-X,gamma system,A 2Sigma+,X 2Pi3/2,44080.5,V,2374.31,10.106,-0.0465,1.9965,0.01915,1.06434,1904.040,14.100,,1.72016,0.0182,1.15077,3,N O
CO,B-A,Angstrom system,B 1Sigma+,A 1Pi,22171.35,V,2112.7,15.22,,1.9612,0.0261,1.1197,1518.24,19.4,,1.6115,0.02325,1.2353,2,C O
AlO,B-X,blue-green system,B 2Sigma+,X 2Sigma+,20635.22,R,870.05,3.52,,0.60408,0.00447,1.6670,979.23,6.97,,0.64136,0.00580,1.6179,4,Al O
CaF,A-X,orange system,A 2Pi3/2,X 2Sigma+,16565.6,V,593.4,3.113,0.0051,0.3436,0.0028,1.952,581.1,2.74,,0.3385,0.0026,1.967,3,Ca F
CaF,A-X,orange system,A 2Pi1/2,X 2Sigma+,16493.1,V,586.8,3.427,0.0619,0.3436,0.0028,1.952,581.1,2.74,,0.3385,0.0026,1.967,3,Ca F
CaF,B-X,green system,B 2Sigma+,X 2Sigma+,18834.2,R,566.1,2.80,,0.3361,,,581.1,2.74,,0.3385,0.0026,1.967,3,Ca F
CaCl,A-X,orange system,A 2Pi3/2,X 2Sigma+,16164.3,V,372.3,1.2,-0.05,,,,367.53,1.31,,0.15195,0.000783,2.439,3,Ca Cl
CaCl,A-X,orange system,A 2Pi1/2,X 2Sigma+,16094.8,V,372.3,1.2,-0.05,,,,367.53,1.31,,0.15195,0.000783,2.439,3,Ca Cl
CaCl,B-X,orange system,B 2Sigma+,X 2Sigma+,16847.6,V,366.7,1.43,,,,,367.53,1.31,,0.15195,0.000783,2.439,3,Ca Cl
SrF,A-X,red system,A 2Pi3/2,X 2Sigma+,15352.0,V,507.3,2.18,,,,,502.4,2.27,,0.250533,0.001546,2.07537,3,Sr F
SrF,A-X,red system,A 2Pi1/2,X 2Sigma+,15071.6,V,507.9,2.21,,,,,502.4,2.27,,0.250533,0.001546,2.07537,3,Sr F
SrF,B-X,orange system,B 2Sigma+,X 2Sigma+,17264.10,R,495.8,2.34,,0.249396,0.001557,2.08010,502.4,2.27,,0.250533,0.001546,2.07537,3,Sr F
SrCl,A-X,red system,A 2Pi3/2,X 2Sigma+,15116.1,V,309.4,0.98,,,,,302.3,0.95,,,,,3,Sr Cl
SrCl,A-X,red system,A 2Pi1/2,X 2Sigma+,14821.9,V,309.4,0.98,,,,,302.3,0.95,,,,,3,Sr Cl
SrCl,B-X,red system,B 2Sigma+,X 2Sigma+,15721.5,V,306.4,0.98,,,,,302.3,0.95,,,,,3,Sr Cl
MgO,B-X,green system,B 1Sigma+,X 1Sigma+,20003.57,V,824.08,4.76,,0.5822,0.0045,1.7371,785.06,5.18,,0.5743,0.0050,1.7490,4,Mg O
TiO,A-X,gamma system,A 3Phi4,X 3Delta3,14163.00,R,867.78,3.942,,0.50739,0.00315,1.66436,1009.02,4.498,-0.0107,0.53541,0.00301,1.62022,4,Ti O
TiO,A-X,gamma system,A 3Phi3,X 3Delta2,14095.88,R,867.78,3.942,,0.50739,0.00315,1.66436,1009.02,4.498,-0.0107,0.53541,0.00301,1.62022,4,Ti O
TiO,A-X,gamma system,A 3Phi2,X 3Delta1,14019.43,R,867.78,3.942,,0.50739,0.00315,1.66436,1009.02,4.498,-0.0107,0.53541,0.00301,1.62022,4,Ti O
TiO,B-X,gamma prime system,B 3Pi2,X 3Delta3,16066.7,R,875,5,,0.50617,,1.66636,1009.02,4.498,-0.0107,0.53541,0.00301,1.62022,4,Ti O
TiO,B-X,gamma prime system,B 3Pi1,X 3Delta2,16151.6,R,875,5,,0.50617,,1.66636,1009.02,4.498,-0.0107,0.53541,0.00301,1.62022,4,Ti O
TiO,B-X,gamma prime system,B 3Pi0,X 3Delta1,16226.4,R,875,5,,0.50617,,1.66636,1009.02,4.498,-0.0107,0.53541,0.00301,1.62022,4,Ti O
TiO,C-X,alpha system,C 3Delta3,X 3Delta3,19334.03,R,838.26,4.76,0.047,0.48989,0.00306,1.69383,1009.02,4.498,-0.0107,0.53541,0.00301,1.62022,4,Ti O
TiO,C-X,alpha system,C 3Delta2,X 3Delta2,19343.66,R,838.26,4.76,0.047,0.48989,0.00306,1.69383,1009.02,4.498,-0.0107,0.53541,0.00301,1.62022,4,Ti O
TiO,C-X,alpha system,C 3Delta1,X 3Delta1,19341.68,R,838.26,4.76,0.047,0.48989,0.00306,1.69383,1009.02,4.498,-0.0107,0.53541,0.00301,1.62022,4,Ti O
BO,A-X,alpha system,A 2Pi1/2,X 2Sigma+,23646.43,R,1260.70,11.157,0.049,1.4018,0.0196,1.3533,1885.69,11.81,,1.7820,0.0166,1.2045,4,B O
BO,A-X,alpha system,A 2Pi3/2,X 2Sigma+,23521.3,R,1260.70,11.157,0.049,1.4018,0.0196,1.3533,1885.69,11.81,,1.7820,0.0166,1.2045,4,B O
BO,B-X,beta system,B 2Sigma+,X 2Sigma+,42872.34,R,1281.69,10.66,,1.5171,0.0210,1.3054,1885.69,11.81,,1.7820,0.0166,1.2045,4,B O
SiO,A-X,ultraviolet system,A 1Pi,X 1Sigma+,42640.71,R,852.8,6.43,0.0238,0.6307,0.00660,1.6206,1241.557,5.966,0.0054557,0.7267512,0.0050377,1.509739,4,Si O
", stringsAsFactors = FALSE)
  }
  molecular_cache$constants
}

molecular_cache <- new.env(parent = emptyenv())

# Masses of the most abundant isotopes (u).
isotope_masses <- c(H = 1.00782503, B = 11.0093054, C = 12, N = 14.0030740, O = 15.9949146,
                    F = 18.9984032, Mg = 23.9850417, Al = 26.9815385, Si = 27.9769265,
                    Cl = 34.9688527, Ca = 39.9625909, Ti = 47.9479409, Sr = 87.9056121)

reduced_mass <- function(atoms) {
  m <- isotope_masses[strsplit(atoms, " ", fixed = TRUE)[[1]]]
  prod(m) / sum(m)
}

# The constants of the upper ("u") or lower ("l") state of a row.
molecular_state <- function(row, which) {
  value <- function(name) row[[paste0(name, "_", which)]]
  state <- list(we = value("we"), wexe = value("wexe"), weye = value("weye"), Be = value("Be"),
                ae = value("ae"), re = value("re"))
  # the bond length from the rotational constant when it is not listed
  if (is.na(state$re) && !is.na(state$Be)) {
    state$re <- sqrt(16.857629 / (reduced_mass(row$atoms) * state$Be))
  }
  state
}

vibrational_term <- function(state, v) {
  h <- v + 0.5
  zero <- function(x) if (is.na(x)) 0 else x
  state$we * h - zero(state$wexe) * h^2 + zero(state$weye) * h^3
}

rotational_constant <- function(state, v) {
  state$Be - (if (is.na(state$ae)) 0 else state$ae) * (v + 0.5)
}

# The bands of one sub-band row, with their unnormalized strength.
system_bands <- function(row, temperature) {
  upper <- molecular_state(row, "u")
  lower <- molecular_state(row, "l")
  v_max <- row$v_max
  q <- franck_condon(row, upper, lower)
  pairs <- if (is.null(q)) {
    data.frame(v_upper = 0:v_max, v_lower = 0:v_max, fcf = NA_real_)
  } else {
    idx <- which(q >= 0.01, arr.ind = TRUE)
    data.frame(v_upper = idx[, 1] - 1L, v_lower = idx[, 2] - 1L, fcf = q[idx])
  }
  g_upper <- vibrational_term(upper, pairs$v_upper) - vibrational_term(upper, 0)
  origin <- row$nu00 + g_upper - vibrational_term(lower, pairs$v_lower) + vibrational_term(lower, 0)
  b_upper <- rotational_constant(upper, pairs$v_upper)
  b_lower <- rotational_constant(lower, pairs$v_lower)
  db <- b_upper - b_lower
  # the head forms at |m| = (B' + B'') / 2|B' - B''|, if that level is populated
  j_head <- abs((b_upper + b_lower) / (2 * db))
  j_limit <- 2 * sqrt(temperature / (1.438777 * 2 * b_lower))
  head <- !is.na(db) & db != 0 & j_head <= j_limit
  wavenumber <- ifelse(head, origin - (b_upper + b_lower)^2 / (4 * db), origin)
  shading <- switch(row$shading, V = "violet", R = "red", NA_character_)
  degraded <- ifelse(is.na(db), shading, ifelse(db < 0, "red", "violet"))
  strength <- ifelse(is.na(pairs$fcf), 1, pairs$fcf) * origin^4 *
    exp(-1.438777 * g_upper / temperature)
  data.frame(
    molecule = row$molecule, system = row$system, name = row$name, upper = row$upper,
    lower = row$lower, v_upper = as.integer(pairs$v_upper), v_lower = as.integer(pairs$v_lower),
    wavelength = vacuum_to_air(1e7 / wavenumber), origin = vacuum_to_air(1e7 / origin),
    degraded = degraded, head = head, fcf = pairs$fcf, strength = strength,
    stringsAsFactors = FALSE
  )
}

# Franck-Condon factors of the levels 0..v_max of the upper state and
# 0..v_max + 6 of the lower state (NULL when a bond length is unknown);
# kept for the R session.
franck_condon <- function(row, upper, lower) {
  if (is.na(upper$re) || is.na(lower$re)) return(NULL)
  key <- paste(row$molecule, row$upper, row$lower)
  if (!is.null(molecular_cache[[key]])) return(molecular_cache[[key]])
  mu <- reduced_mass(row$atoms)
  r <- seq(max(0.2, min(upper$re, lower$re) - 0.8), max(upper$re, lower$re) + 1.6,
           length.out = 6000)
  psi_u <- vibrational_wavefunctions(r, upper, mu, row$v_max)
  psi_l <- vibrational_wavefunctions(r, lower, mu, row$v_max + 6)
  q <- crossprod(psi_u, psi_l)^2
  q[is.na(q)] <- 0
  assign(key, q, envir = molecular_cache)
  q
}

# Normalized vibrational wavefunctions on the grid r (angstrom), one column
# per level: those of a Morse potential (Laguerre polynomials), or of a
# harmonic oscillator (Hermite polynomials) when the anharmonicity is not
# known. Unbound Morse levels are NA.
vibrational_wavefunctions <- function(r, state, mu, v_max) {
  out <- matrix(NA_real_, length(r), v_max + 1)
  if (!is.na(state$wexe) && state$wexe > 0) {
    lambda <- state$we / (2 * state$wexe)
    z <- 2 * lambda * exp(-0.2435576 * sqrt(mu * state$wexe) * (r - state$re))
    for (v in 0:v_max) {
      alpha <- 2 * lambda - 2 * v - 1
      if (alpha <= 0) break
      laguerre <- generalized_laguerre(v, alpha, z)
      log_envelope <- (lambda - v - 0.5) * log(z) - z / 2
      psi <- exp(log_envelope - max(log_envelope)) * laguerre
      out[, v + 1] <- psi / sqrt(sum(psi^2))
    }
  } else {
    xi <- 0.172221 * sqrt(mu * state$we) * (r - state$re)
    h0 <- rep(1, length(xi))
    h1 <- 2 * xi
    for (v in 0:v_max) {
      h <- if (v == 0) h0 else h1
      if (v >= 1) {
        h_next <- 2 * xi * h1 - 2 * v * h0
        h0 <- h1
        h1 <- h_next
      }
      psi <- h * exp(-xi^2 / 2)
      out[, v + 1] <- psi / sqrt(sum(psi^2))
    }
  }
  out
}

generalized_laguerre <- function(n, alpha, z) {
  l0 <- rep(1, length(z))
  if (n == 0) return(l0)
  l1 <- 1 + alpha - z
  if (n >= 2) {
    for (k in 1:(n - 1)) {
      l2 <- ((2 * k + 1 + alpha - z) * l1 - (k + alpha) * l0) / (k + 1)
      l0 <- l1
      l1 <- l2
    }
  }
  l1
}

# Air wavelength (nm) of a vacuum wavelength between 200 and 2000 nm, with
# the dispersion formula of Peck and Reeder (1972) used by the NIST ASD;
# vacuum wavelengths outside this range are returned unchanged.
vacuum_to_air <- function(wavelength) {
  s2 <- (1e3 / wavelength)^2
  n <- 1 + 1e-8 * (8060.51 + 2480990 / (132.274 - s2) + 17455.7 / (39.32957 - s2))
  ifelse(wavelength >= 200 & wavelength <= 2000, wavelength / n, wavelength)
}

air_to_vacuum <- function(wavelength) {
  vacuum <- wavelength
  for (i in 1:3) vacuum <- wavelength * vacuum / vacuum_to_air(vacuum)
  vacuum
}
