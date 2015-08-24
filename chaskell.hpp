#ifndef CHASKELL_H
#define CHASKELL_H
#include <vector>
#include <string>
#include <algorithm>
#include <boost/variant.hpp>

std::string intercalate(const std::string& a, const std::vector<std::string>& b);

std::string concat (const std::vector<std::string>& a);

template<typename A, typename T>
std::vector<T> map(const std::function<T(const A&)>& a, const std::vector<A>& b) {
    std::vector<T> v;
    std::transform(b.begin(), b.end(), v.begin(), a);
    return v;
}

template<typename B, typename A>
B foldl(const std::function<B(const B&, const B&)> f, const B& init, const std::vector<A>& l) {
    return std::accumulate(l.begin(), l.end(), init, f);
}

template<typename A>
A foldl1(const std::function<A(const A&, const A&)>& f, const std::vector<A>& l) {
    return std::accumulate(l.begin()++, l.end(), *l.begin(), f);
}

template<typename A>
std::vector<A> cons(const A&a, const std::vector<A>& b) {
    std::vector<A> v{a};
    v.insert(v.end(), b.begin(), b.end());
    return v;
}

namespace chaskell {

  template <typename T>
  std::function<T> function(const T &a) {
      return std::function<T>(a);
  }

  template<typename T>
  std::vector<T> append(const std::vector<T>& a, const std::vector<T>& b) {
      std::vector<T> v(a);
      v.insert(v.begin(), b.begin(), b.end());
      return v;
  }

  std::string append(const std::string& a, const std::string& b);
}
#endif
