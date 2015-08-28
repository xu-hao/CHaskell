#include <vector>
#include <algorithm>
namespace chaskell {

  std::string append(const std::string& a, const std::string& b) {
      return a + b;
  }

  std::vector<char> vectorchar(const char *string) {
      std::string s(string);
      return std::vector<char>(s.begin(), s.end());

  }
}
