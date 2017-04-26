#include <sstream>

#include "Evaluator.h"
#include "Common.h"
#include "SlaveDevice.h"

namespace jetbrains {
namespace ther {
namespace device {
namespace slave {
namespace {

pGEDevDesc INSTANCE = NULL;

int snapshotNumber = 0;

class InitHelper {
 public:
  InitHelper() : previousDevice(NULL) {
    if (!Rf_NoDevices()) {
      previousDevice = GEcurrentDevice();
    }
  }

  virtual ~InitHelper() {
    if (previousDevice != NULL) {
      int previousDeviceNumber = Rf_ndevNumber(previousDevice->dev);

      GEcopyDisplayList(previousDeviceNumber);
      Rf_selectDevice(previousDeviceNumber);
    }
  }

 private:
  pGEDevDesc previousDevice;
};

std::string calculateInitCommand(const std::string &snapshotDir, double width, double height) {
  std::stringstream ss;

  ss << "grDevices:::png" <<
      "(" <<
      "\"" << snapshotDir << "/snapshot_" << snapshotNumber << ".png" << "\"" << ", " <<
      width << ", " <<
      height << ", " <<
      "res = 96" <<
      ")";

  return ss.str();
}

pGEDevDesc init(const std::string &snapshotDir, double width, double height) {
  InitHelper helper; // helper backups and restores active device and copies its display list to slave device

  evaluator::evaluate(calculateInitCommand(snapshotDir, width, height));

  return GEcurrentDevice();
}

} // anonymous

pGEDevDesc instance(const std::string &snapshotDir, double width, double height) {
  DEVICE_TRACE;

  if (INSTANCE == NULL) {
    INSTANCE = init(snapshotDir, width, height);
  }

  return INSTANCE;
}

void newPage() {
  DEVICE_TRACE;

  dump();

  ++snapshotNumber;
}

void dump() {
  DEVICE_TRACE;

  if (INSTANCE != NULL) {
    GEkillDevice(INSTANCE);

    INSTANCE = NULL;
  }
}

} // slave
} // device
} // ther
} // jetbrains
