//	do {
//		pid = wait(&status);
//		if (pid == -1) {
//			if (errno != EINTR && errno != ECHILD)
//				fatal("wait");
//		} else if (WIFSIGNALED(status))
//			log_warnx("%s terminated; signal %d",
//			    (pid == engine_pid) ? "engine" :
//			    "frontend", WTERMSIG(status));
//	} while (pid != -1 || (pid == -1 && errno == EINTR));
//

#include<stdio.h>

int main() {
  int c = 0;
  int x = 5;
  int b = 2;
  int z = 6;
  int y = 3;

  do {
    c++;
    if (x == 10) {
      if ( x + y > 9 &&  b == 3) {
        printf("something");
      }
    } else if ( b == 8) {
      printf("something else");
    }
  } while ( c < 10 || (y > 5 && z == 7));

  return 0;
}

