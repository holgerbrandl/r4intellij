#ifndef EVALUATOR_H
#define EVALUATOR_H

#include <string>

#include "ScopeProtector.h"

namespace jetbrains {
namespace ther {
namespace device {
namespace evaluator {

void evaluate(const std::string &command);

SEXP evaluate(const std::string &command, jetbrains::ther::device::protector::ScopeProtector *protector);

} // evaluator
} // device
} // ther
} // jetbrains

#endif // EVALUATOR_H
