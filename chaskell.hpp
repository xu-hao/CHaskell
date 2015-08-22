#progma once
#include <vector>
namespace chaskell {
  template<T>
  std::vector<T> append(const std::vector<T>& a, const std::vector<T>& b) {
      std::vector<T> v(a);
      std::insert(v.begin(), b.begin(), b.end());
  }

  std::string append(const std::string& a, const std::string& b);

  template<T>
  std::vector<T> intercalate(T& a, const std::vector<std::vector<T>>& b) {
  }

  std::string intercalate(const T& a, const T& b);
  template<F, T>
  std::vector<T> map(F a, const std::vector<T>& b) {
  }

  template<F>
  std::string map(F a, const std::string& b) {
  }

}
