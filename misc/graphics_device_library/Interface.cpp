#include "Interface.h"

extern "C" SEXP jetbrains_ther_device_init(SEXP snapshotDir) {
  jetbrains::ther::device::master::init(R_CHAR(STRING_ELT(snapshotDir, 0)));

  return R_NilValue;
}
