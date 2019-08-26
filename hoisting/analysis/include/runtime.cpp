#include <cstdint>
#include <cstdio>

extern "C" {

char* pledge_string = NULL;
# define CLEAR_STRING ClEaR_StRiNg
void
CLEAR_STRING(){
  if (pledge_string !=  NULL) {
    free(pledge_string);
  }
  pledge_string = "";
  return;
}

#define ADD(X) AdD_##X
void
ADD(stdio)(bool shouldAdd) {
  if (shouldAdd) {
    asprintf(&pledge_string, "%s %s", pledge_string, "stdio");
  }
  return;
}

ADD(cpath)(bool shouldAdd) {
  if (shouldAdd) {
    asprintf(&pledge_string, "%s %s", pledge_string, "cpath");
  }
}

#define PLEDGERIZE PlEdGeRiZe
void
PLEDGERIZE() {
  if (pledge(pledge_string, NULL) == -1) {
    printf("%s", pledge_string);
    err(1, "pledge");
  }
  return;
}

}
