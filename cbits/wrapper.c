#include "wrapper.h"

void wrap_primme_display_params(primme_params const *params) {
  primme_display_params(*params);
}

void wrap_primme_get_version(int *major, int *minor) {
  *major = PRIMME_VERSION_MAJOR;
  *minor = PRIMME_VERSION_MINOR;
}
