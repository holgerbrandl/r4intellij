#include "Common.h"
#include "Evaluator.h"

#include <R_ext/Parse.h>

namespace jetbrains {
namespace ther {
namespace device {
namespace evaluator {
namespace {

using jetbrains::ther::device::protector::ScopeProtector;

SEXP createSexp(const std::string &str, ScopeProtector *protector) {
  SEXP result = Rf_allocVector(STRSXP, 1);

  protector->add(result);

  SET_STRING_ELT(result, 0, Rf_mkChar(str.c_str()));

  return result;
}

SEXP createExpressionSexp(SEXP strSexp, ScopeProtector *protector) {
  ParseStatus status;

  SEXP result = R_ParseVector(strSexp, 1, &status, R_NilValue);

  protector->add(result);

  return result;
}

SEXP createExpressionSexp(const std::string &str, ScopeProtector *protector) {
  return createExpressionSexp(createSexp(str, protector), protector);
}

SEXP evaluateExpression(SEXP exprSexp, ScopeProtector *protector) {
  SEXP result = Rf_eval(VECTOR_ELT(exprSexp, 0), R_GlobalEnv);

  protector->add(result);

  return result;
}

} // anonymous

void evaluate(const std::string &command) {
  DEVICE_TRACE;

  jetbrains::ther::device::protector::ScopeProtector protector;

  evaluateExpression(createExpressionSexp(command, &protector), &protector);
}


SEXP evaluate(const std::string &command, jetbrains::ther::device::protector::ScopeProtector *protector) {
  DEVICE_TRACE;

  return evaluateExpression(createExpressionSexp(command, protector), protector);
}

} // evaluator
} // device
} // ther
} // jetbrains
