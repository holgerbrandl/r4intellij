#ifndef SCOPE_PROTECTOR_H
#define SCOPE_PROTECTOR_H

#include <memory>

#include <Rinternals.h>

namespace jetbrains {
namespace ther {
namespace device {
namespace protector {

class ScopeProtector {
 public:
  ScopeProtector();

  ScopeProtector(const ScopeProtector &) = delete;
  ScopeProtector &operator=(const ScopeProtector &) = delete;

  virtual ~ScopeProtector();

  void add(SEXP sexp);

 private:
  class Impl;

  const std::unique_ptr<Impl> pImpl;
};

} // protector
} // device
} // ther
} // jetbrains

#endif // SCOPE_PROTECTOR_H
