#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

char* au_moins_une(char** l, int n){
  int max_len = n + 2; // '(' + (n-1) * '|' + \0 + ')'
  for(int i = 0; i < n; i++)
    max_len += strlen(l[i]);
  
  //printf("%d\n", max_len);
  char* formule = malloc(max_len * sizeof(char));
  formule[0] = '(';
  formule[1] = '\0';
  
  for(int i = 0; i < n; i++){
      strcat(formule, l[i]);
      if (i!=n-1)
        strcat(formule, "|");
  }
  strcat(formule, ")");
  return formule;
}

char* au_plus_une(char** l, int n){
  int max_len = (n-1)*(n+1) + n + 3; // '(' + (n-1)*(n+1) * '&' + n * '|' + \0 + ')'
  for(int i = 0; i < n; i++)
    max_len += (n+1) * strlen(l[i]) + n; // n * '~'
  //printf("%d\n", max_len);
  char* formule = malloc(max_len * sizeof(char));
  formule[0] = '(';
  formule[1] = '\0';

  for(int i = -1; i < n; i++){
    for(int j = 0; j < n; j++){
      if(i != j)
        strcat(formule, "~");
      strcat(formule, l[j]);
      if (j!= n-1)
        strcat(formule, "&");
    }
    if(i!=n-1)
      strcat(formule, "|");
  }
  strcat(formule, ")");
  return formule;
}

int taille_str_au_moins_une(int n, int var_length){
  int num_digits = log10(n) + 1;

  int contribution_taille_au_moins = 2; // '(' + ')'
  contribution_taille_au_moins += n - 1; // (n-1) * '|'
  contribution_taille_au_moins += var_length*n; // n*(taille max de variable)

  return contribution_taille_au_moins;
}

int taille_str_au_plus_une(int n, int var_length){
  int num_digits = log10(n) + 1;

  int contribution_taille_au_moins = 2; // '(' + ')'
  contribution_taille_au_moins += n; // n * '|'
  contribution_taille_au_moins += n * n; // n*n * '~'
  contribution_taille_au_moins += n*(n+1) * var_length; // n(n+1) * (taille max de variable)
  contribution_taille_au_moins += (n-1) * (n+1); // (n-1)(n+1) * '&'
  
  return contribution_taille_au_moins;
}

void free_liste(char** l, int n){
  for(int i = 0; i < n; i++){
    free(l[i]);
  }
  free(l);
}

/*
int main(void){
  char* l[3] = {"(x & ~y)", "y", "z"};
  char* f1 = au_moins_une(l, 3);
  char* f2 = au_plus_une(l, 3);
  printf("%s\n", f1);
  printf("%s\n", f2);
  return 0;
}
*/