#include <Rcpp.h>
#include <cmath>
#include <vector>
using namespace Rcpp;

// Penalized least-squares (Whittaker) smoothers used for baseline estimation.
//
// Each iteration solves (W + lambda * D'D) z = W y, where D is the
// second-order difference matrix. The system matrix is symmetric, positive
// definite and pentadiagonal, so it is factorized with a banded Cholesky
// decomposition in O(n) time and memory instead of the O(n^3) dense solve.

namespace {

// Bands of D'D for the second-order difference matrix of size (n - 2) x n.
struct Penalty {
  std::vector<double> d0, d1, d2;  // main, first and second (sub)diagonals
};

Penalty second_difference_penalty(int n) {
  Penalty P;
  P.d0.assign(n, 0.0);
  P.d1.assign(n, 0.0);
  P.d2.assign(n, 0.0);
  const double c[3] = {1.0, -2.0, 1.0};
  for (int k = 0; k + 2 < n; ++k) {
    for (int a = 0; a < 3; ++a) {
      P.d0[k + a] += c[a] * c[a];
      if (a < 2) P.d1[k + a + 1] += c[a] * c[a + 1];
    }
    P.d2[k + 2] += c[0] * c[2];
  }
  return P;
}

// Solves (diag(w) + lambda * D'D) z = w * y using a banded Cholesky factor.
// d1[i] and d2[i] hold the entries A(i, i-1) and A(i, i-2).
void solve_banded(const std::vector<double>& w, const double* y, double lambda,
                  const Penalty& P, std::vector<double>& z) {
  const int n = static_cast<int>(w.size());
  std::vector<double> l0(n), l1(n, 0.0), l2(n, 0.0), b(n);

  for (int i = 0; i < n; ++i) {
    const double a0 = w[i] + lambda * P.d0[i];
    const double a1 = lambda * P.d1[i];
    const double a2 = lambda * P.d2[i];
    if (i >= 2) l2[i] = a2 / l0[i - 2];
    if (i >= 1) {
      const double prev = (i >= 2) ? l2[i] * l1[i - 1] : 0.0;
      l1[i] = (a1 - prev) / l0[i - 1];
    }
    const double diag = a0 - l1[i] * l1[i] - l2[i] * l2[i];
    if (!(diag > 0.0)) {
      stop("The penalized system is not positive definite; try a larger 'lambda'.");
    }
    l0[i] = std::sqrt(diag);
  }

  // Forward substitution: L b = w * y
  for (int i = 0; i < n; ++i) {
    double s = w[i] * y[i];
    if (i >= 1) s -= l1[i] * b[i - 1];
    if (i >= 2) s -= l2[i] * b[i - 2];
    b[i] = s / l0[i];
  }
  // Backward substitution: L' z = b
  z.assign(n, 0.0);
  for (int i = n - 1; i >= 0; --i) {
    double s = b[i];
    if (i + 1 < n) s -= l1[i + 1] * z[i + 1];
    if (i + 2 < n) s -= l2[i + 2] * z[i + 2];
    z[i] = s / l0[i];
  }
}

// Asymmetric least squares (Eilers & Boelens, 2005).
void als_row(const double* y, int n, double lambda, double p, int max_iter,
             const Penalty& P, std::vector<double>& z) {
  std::vector<double> w(n, 1.0);
  for (int it = 0; it < max_iter; ++it) {
    solve_banded(w, y, lambda, P, z);
    bool changed = false;
    for (int i = 0; i < n; ++i) {
      const double wi = (y[i] > z[i]) ? p : 1.0 - p;
      if (wi != w[i]) changed = true;
      w[i] = wi;
    }
    if (!changed) break;
  }
}

// Asymmetrically reweighted penalized least squares (Baek et al., 2015).
void arpls_row(const double* y, int n, double lambda, double ratio, int max_iter,
               const Penalty& P, std::vector<double>& z) {
  std::vector<double> w(n, 1.0), wt(n), d(n);
  for (int it = 0; it < max_iter; ++it) {
    solve_banded(w, y, lambda, P, z);

    double sum = 0.0, sumsq = 0.0;
    int m = 0;
    for (int i = 0; i < n; ++i) {
      d[i] = y[i] - z[i];
      if (d[i] < 0.0) {
        sum += d[i];
        sumsq += d[i] * d[i];
        ++m;
      }
    }
    if (m < 2) break;
    const double mean = sum / m;
    const double var = (sumsq - m * mean * mean) / (m - 1);
    if (!(var > 0.0)) break;
    const double sd = std::sqrt(var);

    double num = 0.0, den = 0.0;
    for (int i = 0; i < n; ++i) {
      wt[i] = 1.0 / (1.0 + std::exp(2.0 * (d[i] - (2.0 * sd - mean)) / sd));
      num += (w[i] - wt[i]) * (w[i] - wt[i]);
      den += w[i] * w[i];
    }
    if (std::sqrt(num) / std::sqrt(den) < ratio) break;
    w.swap(wt);
  }
}

}  // namespace

// Row-wise baseline estimation for a matrix of spectra (one spectrum per row).
// method = 0: asymmetric least squares, `param` is the asymmetry p.
// method = 1: arPLS, `param` is the convergence ratio.
// [[Rcpp::export]]
NumericMatrix whittaker_baseline_cpp(NumericMatrix x, double lambda, double param,
                                     int max_iter, int method) {
  const int nr = x.nrow();
  const int nc = x.ncol();
  if (nc < 3) stop("Spectra must have at least 3 points.");

  const Penalty P = second_difference_penalty(nc);
  NumericMatrix baseline(nr, nc);
  std::vector<double> y(nc), z(nc);

  for (int r = 0; r < nr; ++r) {
    for (int j = 0; j < nc; ++j) y[j] = x(r, j);
    if (method == 0) {
      als_row(y.data(), nc, lambda, param, max_iter, P, z);
    } else {
      arpls_row(y.data(), nc, lambda, param, max_iter, P, z);
    }
    for (int j = 0; j < nc; ++j) baseline(r, j) = z[j];
    if (r % 64 == 0) Rcpp::checkUserInterrupt();
  }
  return baseline;
}
