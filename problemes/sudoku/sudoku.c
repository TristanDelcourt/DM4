#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "../utils.h"

char* variable (int i , int j, int k, int n){
  int num_digits = 2*log10(n) + 1;
  
  char* s = malloc((5 + 3*num_digits) * sizeof(char));
  sprintf(s, "X_%d_%d_%d", i, j, k);
  return s;
}

char* condition_une_couleur_une_case(int i, int j, int n){
  char** l1 = malloc(n*n*sizeof(char*));
  for(int k = 1; k <= n*n; k++){
    l1[k-1] = variable(i, j, k, n);
  }

  char* f1 = au_moins_une(l1, n*n);

  free_liste(l1, n*n);
  int len = (int)(n*n*n*n - n*n)/2;
  char** l2 = malloc(len*sizeof(char*));

  int ind = 0;
  for(int k = 1; k <= n*n; k++){
    for(int l = k+1; l <= n*n; l++){
      char* v1 = variable(i, j, k, n);
      char* v2 = variable(i, j, l, n);
      char* f2 = malloc((strlen(v1) + strlen(v2) + 4)*sizeof(char));
      sprintf(f2, "(%s&%s)", v1, v2);
      l2[ind] = f2;
      free(v1);
      free(v2);
      ind++;
    }
  }
  char* f2 = au_moins_une(l2, len);
  free_liste(l2, len);
  char* f = malloc(strlen(f1) + strlen(f2) + 5);
  sprintf(f, "(%s&~%s)", f1, f2);
  free(f1);
  free(f2);
  return f;
  
}

char* condition_une_couleur_n_4_cases(int n){

  int len = 0;
  char** l = malloc(n*n*n*n*sizeof(char*));
  for(int i = 0; i < n*n; i++){
    for(int j = 0; j < n*n; j++){
      char* f = condition_une_couleur_une_case(i, j, n);
      l[i*n*n + j] = f;
      len += strlen(f);
    }
  }

  char* f = malloc((len + n*n*n*n-1 +3)*sizeof(char));
  f[0] = '(';
  f[1] = '\0';
  for(int i = 0; i < n*n*n*n; i++){
    strcat(f, l[i]);
    if(i!=n*n*n*n-1)
      strcat(f, "&");
  }
  strcat(f, ")");

  free_liste(l, n*n*n*n);
  return f;
}

char* condition_unique_chiffre_une_ligne(int i, int k, int n){
  int len = (int)(n*n*n*n - n*n)/2;
  char** l1 = malloc(len*sizeof(char*));

  int ind = 0;
  for(int j = 0; j < n*n; j++){
    for(int l = j+1; l < n*n; l++){
      char* v1 = variable(i, j, k, n);
      char* v2 = variable(i, l, k, n);
      char* f = malloc((strlen(v1) + strlen(v2) + 4)*sizeof(char));
      sprintf(f, "(%s&%s)", v1, v2);
      l1[ind] = f;
      free(v1);
      free(v2);
      ind++;
    }
  }

  char* f1 = au_moins_une(l1, len);
  free_liste(l1, len);
  char* f = malloc(strlen(f1) + 4);
  sprintf(f, "(~%s)", f1);
  free(f1);
  return f;
  
}

char* condition_unique_chiffre_n_lignes(int n){
  
  int len = 0;
  char** l = malloc(n*n*n*n*sizeof(char*));
  for(int i = 0; i < n*n; i++){
    for(int k = 1; k <= n*n; k++){
      char* f = condition_unique_chiffre_une_ligne(i, k, n);
      l[i*n*n + k - 1] = f;
      len += strlen(f);
    }
  }

  char* f = malloc((len + n*n*n*n-1 +3)*sizeof(char));
  f[0] = '(';
  f[1] = '\0';
  for(int i = 0; i < n*n*n*n; i++){
    strcat(f, l[i]);
    if(i!=n*n*n*n-1)
      strcat(f, "&");
  }
  strcat(f, ")");

  free_liste(l, n*n*n*n);
  return f;
}

char* condition_unique_chiffre_une_colonne(int j, int k, int n){
  int len = (int)(n*n*n*n - n*n)/2;
  char** l1 = malloc(len*sizeof(char*));

  int ind = 0;
  for(int i = 0; i < n*n; i++){
    for(int l = i+1; l < n*n; l++){
      char* v1 = variable(i, j, k, n);
      char* v2 = variable(l, j, k, n);
      char* f = malloc((strlen(v1) + strlen(v2) + 4)*sizeof(char));
      sprintf(f, "(%s&%s)", v1, v2);
      l1[ind] = f;
      free(v1);
      free(v2);
      ind++;
    }
  }

  char* f1 = au_moins_une(l1, len);
  free_liste(l1, len);
  char* f = malloc(strlen(f1) + 4);
  sprintf(f, "(~%s)", f1);
  free(f1);
  return f;

}

char* condition_unique_chiffre_n_colonnes(int n){

  int len = 0;
  char** l = malloc(n*n*n*n*sizeof(char*));
  for(int j = 0; j < n*n; j++){
    for(int k = 1; k <= n*n; k++){
      char* f = condition_unique_chiffre_une_colonne(j, k, n);
      l[j*n*n + k - 1] = f;
      len += strlen(f);
    }
  }

  char* f = malloc((len + n*n*n*n-1 +3)*sizeof(char));
  f[0] = '(';
  f[1] = '\0';
  for(int i = 0; i < n*n*n*n; i++){
    strcat(f, l[i]);
    if(i!=n*n*n*n-1)
      strcat(f, "&");
  }
  strcat(f, ")");

  free_liste(l, n*n*n*n);
  return f;
}

int main(void){
  int n = 3;

  /*
  char* f1 = condition_une_couleur_n_4_cases(n);
  printf("%s\n", f1);
  */
  char* f2 = condition_unique_chiffre_n_colonnes(n);
  printf("%s\n", f2);

  
  free(f2);
  
  return 0;
}