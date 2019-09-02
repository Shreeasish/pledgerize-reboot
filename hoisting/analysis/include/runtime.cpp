

int
std::unordered_map<std::string, std::deque<std::string>> promiseStringMap;

int
std::unorderd_map<std::string, int> promiseInstCounter;

extern "C" {

#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <err.h>

char active_pledges[1024] = "";

char pledge_string[1024] = "stdio";
# define CLEAR_STRING ClEaR_StRiNg
void
CLEAR_STRING(){
  return;
}

#define ADD(X) AdD_##X
void
ADD(stdio)(bool shouldAdd) {
//  if (shouldAdd) {
//    strcat(pledge_string, " stdio");
//  }
  return;
}

void
ADD(cpath)(bool shouldAdd) {
  if (shouldAdd) {
    strcat(pledge_string, " cpath");
  }
}

void
ADD(rpath)(bool shouldAdd) {
  if (shouldAdd) {
    strcat(pledge_string, " rpath");
  }
}

#define MAKE_PLEDGE MaKe_pLeDgE
void
MAKE_PLEDGE() {
  fprintf(stderr, "\nPLEDGING WITH \"%s\"\n", pledge_string);
  fflush(stderr);
  if (pledge(pledge_string, NULL) == -1) {
    err(1, "pledge");
  }
  strcpy(active_pledges, pledge_string);
  strcpy(pledge_string, "stdio");
  //pledge_string[0] = '\0';
  return;
}



}
