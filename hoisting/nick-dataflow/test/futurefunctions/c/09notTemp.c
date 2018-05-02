/*  Using fread as an example for overlapping paths
**  fread(),stdio,rpath,wpath,tmppath,0x27
**  Naive analysis would assign this all the promises mentioned above
**  This example should only produce stdio, rpath, wpath (?) but not tmppath
*/

#include<stdio.h>


int
main() {
  FILE *outfile = fopen("./dummyTextFile.txt", "r");
  char buffer[100];
  fread(buffer, 50, 1, outfile);
  printf("%s\n",buffer);
  fclose(outfile);
  return 0;
}

