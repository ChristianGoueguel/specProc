#ifndef SPECPROC_GLSW_H
#define SPECPROC_GLSW_H

#include <RcppEigen.h>

// Generalized least squares weighting filter (Martens et al., 2003).
//
// With C = Xd' Xd = V diag(lambda) V', the filter is G = V D^-1 V' where
// D = sqrt(lambda / alpha + 1). Directions outside the row space of Xd have
// lambda = 0 (D = 1), so G = I - V (I - D^-1) V' using the thin SVD of Xd.
// This avoids forming the p x p covariance matrix explicitly.
inline Eigen::MatrixXd glsw_filter(const Eigen::MatrixXd& Xd, double alpha) {
  const Eigen::Index p = Xd.cols();
  Eigen::BDCSVD<Eigen::MatrixXd> svd(Xd, Eigen::ComputeThinV);
  const Eigen::VectorXd s = svd.singularValues();
  const Eigen::MatrixXd V = svd.matrixV();

  const Eigen::VectorXd shrink =
      (1.0 - ((s.array().square() / alpha) + 1.0).sqrt().inverse()).matrix();

  Eigen::MatrixXd G = Eigen::MatrixXd::Identity(p, p);
  G.noalias() -= V * shrink.asDiagonal() * V.transpose();
  return G;
}

#endif
