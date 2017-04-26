#ifndef SLAVE_DEVICE_H
#define SLAVE_DEVICE_H

#include <string>

#include <R_ext/GraphicsEngine.h>

namespace jetbrains {
namespace ther {
namespace device {
namespace slave {

pGEDevDesc instance(const std::string &snapshotDir, double width, double height);

void newPage();

void dump();

} // slave
} // device
} // ther
} // jetbrains

#endif // SLAVE_DEVICE_H
