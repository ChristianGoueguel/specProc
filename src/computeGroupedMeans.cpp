#include <Rcpp.h>
using namespace Rcpp;
#include <map>
#include <vector>
#include <numeric> // For std::accumulate

// Enable C++11
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
Rcpp::DataFrame computeGroupedMeans(Rcpp::CharacterVector groups, Rcpp::NumericMatrix values) {
  int nrows = values.nrow();
  int ncols = values.ncol();

  // Create a map to store sum and count for each group
  std::map<std::string, std::pair<double, int > > groupSums;

  // Initialize the map
  for (int i = 0; i < nrows; ++i) {
    std::string group = Rcpp::as<std::string>(groups[i]);
    if (groupSums.find(group) == groupSums.end()) {
      groupSums[group] = std::make_pair(0.0, 0);
    }
    double rowSum = 0.0;
    for (int j = 0; j < ncols; ++j) {
      rowSum += values(i, j);
    }
    groupSums[group].first += rowSum;
    groupSums[group].second += 1;
  }

  // Prepare vectors for the output data frame
  std::vector<std::string> uniqueGroups;
  std::vector<double> means;

  for (std::map<std::string, std::pair<double, int > >::iterator it = groupSums.begin(); it != groupSums.end(); ++it) {
    uniqueGroups.push_back(it->first);
    means.push_back(it->second.first / it->second.second);
  }

  // Return as a data frame
  return Rcpp::DataFrame::create(
    Rcpp::Named("group") = uniqueGroups,
    Rcpp::Named("mean") = means);
}
