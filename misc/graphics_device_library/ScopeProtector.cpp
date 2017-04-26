#include "Common.h"
#include "ScopeProtector.h"

namespace jetbrains {
namespace ther {
namespace device {
namespace protector {

class ScopeProtector::Impl {
 public:
  Impl() : count(0) {
    DEVICE_TRACE;
  }

  virtual ~Impl() {
    DEVICE_TRACE;

    if (count > 0) {
      Rf_unprotect(count);
    }
  }

  void add(SEXP sexp) {
    DEVICE_TRACE;

    Rf_protect(sexp);
    count++;
  }

 private:
  int count;
};

ScopeProtector::ScopeProtector() : pImpl(new Impl) {
}

ScopeProtector::~ScopeProtector() = default;

void ScopeProtector::add(SEXP sexp) {
  pImpl->add(sexp);
}

} // protector
} // device
} // ther
} // jetbrains


