#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

#include "../utils.h"

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

char* variable (int i , int j, int num_digits){
  char* s = malloc(sizeof(char) * (4 + 2*num_digits));
  sprintf(s, "X_%d_%d", i, j);
  return s;
}

char* contrainte_une_ligne (int i , int n){
  int num_digits = log10(n) + 1;
  
  char** l = malloc(n*sizeof(char*));
  for(int j = 0; j < n; j++){
    l[j] = variable(i, j, num_digits);
  }
  char* f1 = au_moins_une(l, n);
  char* f2 = au_plus_une(l, n);

  //printf("  %d\n", (strlen(f1) + strlen(f2) + 4)); // '(' + '&' + '\0' + ')'
  
  char* f = malloc((strlen(f1) + strlen(f2) + 4) * sizeof(char));
  sprintf(f, "(%s&%s)", f1, f2);

  
  free(f1);
  free(f2);
  for(int j = 0; j < n; j++){
    free(l[j]);
  }
  free(l);
  

  return f;
}

char* contrainte_n_lignes (int n){
  int num_digits = log10(n) + 1;
  int var_length = 3 + 2*num_digits;
  
  int taille = taille_str_au_plus_une(n, var_length) + taille_str_au_moins_une(n, var_length) + 3;;
  int max_len = n*taille + n + 2; // '('+ contributions + (n-1)*'&' + \0 + ')'
  
  //printf( "%d\n", max_len);
  
  char* f = malloc(max_len * sizeof(char));
  f[0] = '(';
  f[1] = '\0';
  for(int i = 0; i < n; i++){
    char* une_ligne = contrainte_une_ligne(i, n);
    strcat(f, une_ligne);
    if(i!=n-1){
      strcat(f, "&");
    }
    free(une_ligne);
  }
  strcat(f, ")");

  return f;
}

char* contrainte_une_colonne (int j , int n){
  int num_digits = log10(n) + 1;

  char** l = malloc(n*sizeof(char*));
  for(int i = 0; i < n; i++){
    l[i] = variable(i, j, num_digits);
  }
  char* f = au_plus_une(l, n);
  
  for(int j = 0; j < n; j++){
    free(l[j]);
  }
  free(l);
  
  return f;
}

char* contrainte_n_colonnes (int n){
  int num_digits = log10(n) + 1;
  int var_length = 3 + 2*num_digits;
  
  int taille = taille_str_au_plus_une(n, var_length);
  int max_len = n*taille + n + 2; // '('+ contributions + (n-1)*'&' + \0 + ')'
  
  //printf( "%d\n", max_len);

  char* f = malloc(max_len * sizeof(char));
  f[0] = '(';
  f[1] = '\0';

  for(int j = 0; j < n; j++){
    char* une_colonne = contrainte_une_colonne(j, n);
    strcat(f, une_colonne);
    if(j!=n-1)
      strcat(f, "&");
    free(une_colonne);
  }

  strcat(f, ")");

  return f;
  
}

int nb_cases_diagonale(int i, int n){
  return -abs(i - n + 1) + n; // formule magique tkt
}

char* contrainte_une_diagonale (bool anti, int i , int n){
  int num_digits = log10(n) + 1;
  int nb_cases = nb_cases_diagonale(i, n);
  
  char** l = malloc(nb_cases*sizeof(char*));

  for(int j = 0; j < nb_cases; j++){
    if(anti){
      l[j] = variable(MIN(i-j, n-1-j), j + MAX(i-n+1, 0), num_digits); // encore magique
    }
    else{
      l[j] = variable(j + MAX(i+1-n, 0), j + MAX(n-i-1, 0), num_digits); // de meme magique
    }
  }
  
  char* f = au_plus_une(l, nb_cases);

  for(int j = 0; j < nb_cases; j++){
    free(l[j]);
  }
  free(l);

  return f;
}

int taille_str_diagonale(int n){
  int num_digits = log10(n) + 1;
  int var_length = 3 + 2*num_digits;
  int total = 0;
  for(int i = 1; i <= 2*n-3; i++){
    total += 2*taille_str_au_plus_une(nb_cases_diagonale(i, n), var_length);
  }
  return total;
}

char* contrainte_diagonales (int n){
  int num_digits = log10(n) + 1;
  int var_length = 3 + 2*num_digits;
  
  int max_len = taille_str_diagonale(n) + (2*(2*n-3) -1) + 3; // '(' + (2*(2*n-3) -1) * '&' + ')' + '\0'
  
  //printf("%d\n", max_len);

  char* f = malloc(max_len * sizeof(char));
  f[0] = '(';
  f[1] = '\0';

  for(int i = 1; i <= 2*n-3; i++){
    char* une_diagonale_1 = contrainte_une_diagonale(false, i, n);
    char* une_diagonale_2 = contrainte_une_diagonale(true, i, n);
    strcat(f, une_diagonale_1);
    strcat(f, "&");
    strcat(f, une_diagonale_2);
    if(i!=2*n-3)
      strcat(f, "&");
    free(une_diagonale_1);
    free(une_diagonale_2);

  }
  strcat(f, ")");

  return f;

}

void gen_formule_n_dames(int n){
  
  char* f1 = contrainte_n_lignes(n);
  char* f2 = contrainte_n_colonnes(n);
  char* f3 = contrainte_diagonales(n);

  char* f = malloc(strlen(f1) + strlen(f2) + strlen(f3) + 5);
  sprintf(f, "(%s&%s&%s)", f1, f2, f3);

  char* path = malloc(100 * sizeof(char));
  sprintf(path, "%d_dames.txt", n);
  FILE* out = fopen(path, "w");
  fprintf(out, "%s", f);
  printf("fichier %s créé\n", path);

  /*
  printf("%s\n\n", f1);
  printf("%s\n\n", f2);
  printf("%s\n\n", f3);
  printf("%s\n", f);
  */

  free(f1);
  free(f2);
  free(f3);
  free(f);
  free(path);
  fclose(out);
}

int main(int argc, char** argv){
  assert(argc == 2);
  
  int n = atoi(argv[1]);
  
  gen_formule_n_dames(n);
  
  return 0;
}