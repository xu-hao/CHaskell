#include <vector>
#include <algorithm>

std::string intercalate(const std::string& a, const std::vector<std::string>& b) {
    return std::accumulate(b.begin()++, b.end(), *b.begin(), [&a](const std::string& a0, const std::string &b) { return a0 + a + b; });
}

std::string concat (const std::vector<std::string>& a) {
    return std::accumulate(a.begin()++, a.end(), *a.begin(), [](const std::string& a0, const std::string &b) { return a0 + b; });
}



namespace chaskell {

  std::string append(const std::string& a, const std::string& b) {
      return a + b;
  }

}
