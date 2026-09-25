#include <Rcpp.h>
#include <cmath>
#include <complex>
#include <vector>
using namespace Rcpp;

// Exact Voigt profile via the Faddeeva function w(z) = exp(-z^2) erfc(-iz).
//
// For Im(z) >= 0, w(z) is evaluated with Weideman's rational expansion
// (Weideman, 1994, SIAM J. Numer. Anal. 31(5):1497-1518) with N = 32 terms.
// The coefficients are computed once from a discrete Fourier transform.
// The unit-area Voigt profile with Gaussian standard deviation sigma and
// Lorentzian half width at half maximum gamma is
//   V(x) = Re[w(z)] / (sigma * sqrt(2 * pi)),  z = (x + i * gamma) / (sigma * sqrt(2)).

namespace {

const int N_TERMS = 32;
const double PI = 3.14159265358979323846;

struct Weideman {
  double L;
  std::vector<double> a;  // polynomial coefficients, highest power first

  Weideman() {
    const int M = 2 * N_TERMS;
    const int M2 = 2 * M;
    L = std::sqrt(N_TERMS / std::sqrt(2.0));

    // f(t_k) for k = -M+1, ..., M-1, preceded by a zero: length 2M.
    std::vector<double> f(M2, 0.0);
    for (int k = -M + 1; k <= M - 1; ++k) {
      const double theta = k * PI / M;
      const double t = L * std::tan(theta / 2.0);
      f[k + M] = std::exp(-t * t) * (L * L + t * t);
    }
    // fftshift (circular shift by M), then the real part of the DFT.
    std::vector<double> g(M2);
    for (int i = 0; i < M2; ++i) g[i] = f[(i + M) % M2];
    std::vector<double> A(N_TERMS + 1);
    for (int j = 1; j <= N_TERMS; ++j) {
      double re = 0.0;
      for (int m = 0; m < M2; ++m) re += g[m] * std::cos(2.0 * PI * j * m / M2);
      A[j] = re / M2;
    }
    a.resize(N_TERMS);
    for (int n = 0; n < N_TERMS; ++n) a[n] = A[N_TERMS - n];
  }

  std::complex<double> w(std::complex<double> z) const {
    const std::complex<double> I(0.0, 1.0);
    const std::complex<double> denom = L - I * z;
    const std::complex<double> Z = (L + I * z) / denom;
    std::complex<double> p(0.0, 0.0);
    for (int n = 0; n < N_TERMS; ++n) p = p * Z + a[n];
    return 2.0 * p / (denom * denom) + (1.0 / std::sqrt(PI)) / denom;
  }
};

const Weideman& faddeeva() {
  static const Weideman instance;
  return instance;
}

}  // namespace

// Unit-area Voigt profile evaluated at `x` (already centered at the line
// position). sigma: Gaussian standard deviation; gamma: Lorentzian HWHM.
// [[Rcpp::export]]
NumericVector voigt_cpp(NumericVector x, double sigma, double gamma) {
  if (!(sigma >= 0.0) || !(gamma >= 0.0)) stop("'sigma' and 'gamma' must be non-negative.");
  if (sigma == 0.0 && gamma == 0.0) stop("'sigma' and 'gamma' cannot both be zero.");

  const R_xlen_t n = x.size();
  NumericVector out(n);

  if (sigma == 0.0) {  // pure Lorentzian
    for (R_xlen_t i = 0; i < n; ++i) {
      out[i] = gamma / (PI * (x[i] * x[i] + gamma * gamma));
    }
    return out;
  }

  const Weideman& W = faddeeva();
  const double s2 = sigma * std::sqrt(2.0);
  const double norm = sigma * std::sqrt(2.0 * PI);
  for (R_xlen_t i = 0; i < n; ++i) {
    if (ISNAN(x[i])) {
      out[i] = NA_REAL;
      continue;
    }
    const std::complex<double> z(x[i] / s2, gamma / s2);
    out[i] = W.w(z).real() / norm;
  }
  return out;
}
