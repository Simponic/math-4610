#include "lizfcm.test.h"
#include <stdlib.h>
#include <time.h>

UTEST(basic, unit_tests) { ASSERT_TRUE(1); }

UTEST_STATE();
int main(int argc, const char *const argv[]) {
  srand(time(NULL));

  return utest_main(argc, argv);
}
