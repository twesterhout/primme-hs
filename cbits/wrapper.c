#include "wrapper.h"

void wrap_primme_display_params(primme_params const *params) {
  primme_display_params(*params);
}
