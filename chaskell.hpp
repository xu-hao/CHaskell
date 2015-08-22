#progma once
#include <vector>
namespace chaskell {
  template<T>
  std::vector<T> append(const std::vector<T>& a, const std::vector<T>& b) {
      std::vector<T> v(a);
      std::insert(v.begin(), b.begin(), b.end());
  }

  std::string append(const std::string& a, const std::string& b);

  template<F, T>
  std::vector<T> map(F a, const std::vector<T>& b) {
  }

  template<F, B, A>
  B foldl(F f, B init, const std::vector<A> &l) {

  }
  template<F, A>
  A foldl1(F f, const std::vector<A> &l) {

  }
}
