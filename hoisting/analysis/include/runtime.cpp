#include <cstdint>
#include <cstdio>

extern "C" {

#define PLEDGERIZE(X) PlEdGeRiZe_##X


void
PLEDGERIZE(drop)() {
  printf("stdio");
  return;
}
