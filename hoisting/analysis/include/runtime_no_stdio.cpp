extern "C" {

#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <err.h>

char pledge_string[1024] = {'\0'};
# define CLEAR_STRING ClEaR_StRiNg
void
CLEAR_STRING(){
  return;
}

#define ADD(X) AdD_##X
void
ADD(stdio)(bool shouldAdd) {
  if (shouldAdd) {
    strcat(pledge_string, " stdio");
  }
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
  int length = strlen(pledge_string);
  char* built_string = pledge_string;
  if (length > 1) {
    built_string = built_string + 1;
  }
  fprintf(stderr, "\nPLEDGING WITH \"%s\"\n",built_string);
  fflush(stderr);
  if (pledge(built_string, NULL) == -1) {
    err(1, "pledge");
  }
  pledge_string[0] = '\0';
  return;
}

}
