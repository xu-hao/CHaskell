#include <vector>
#include <algorithm>

std::string intercalate(const std::string& a, const std::vector<std::string>& b) {
    return std::accumulate(b.begin()++, b.end(), *b.begin(), [&a](const std::string& a0, const std::string &b) { return a0 + a + b; });
}

namespace chaskell {

  std::string append(const std::string& a, const std::string& b) {
      return a + b;
  }


  int add(const int& a, const int& b) {
      return a + b;
  }

}
