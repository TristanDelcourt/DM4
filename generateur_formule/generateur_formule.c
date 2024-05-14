#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>

void generer_formule(char* nomfichier, int n){
  char* lettre[28] = {"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "F", "T"};
   char* op[4] = {"&", "|", ">", "="};
  
  FILE* f = fopen(nomfichier, "w");
  int nb_op = 0;
  while(nb_op < n) {
    if(rand()%2){
      fprintf(f, "~");
      nb_op++;
    }
    fprintf(f,"%s", lettre[rand()%28]);
    fprintf(f,"%s", op[rand()%4]);
    nb_op++;
  }
  if(rand()%2){
    fprintf(f, "%s", "~");
  }
  fprintf(f, "%s", lettre[rand()%28]);

  fclose(f);
}

int main(int argc, char** argv) {
  assert(argc == 3);
  
  srand(time(NULL));
  generer_formule(argv[1], atoi(argv[2]));
  
  return 0 ; 
}