#include "utils_eva.h"
#include <string.h> 
#include <stdlib.h>
#include <stdio.h>


char* variable(int i, int j){
  char* str = malloc(8);
  sprintf(str,"X_%d_%d", i, j);
  return str;
}

char* contrainte_une_ligne(int i, int n){
  char** contrainte = malloc(n*sizeof(char*));
  for(int j = 0 ; j<n ; j++){
    contrainte[j] = variable(i,j);
  }
  char* f1 = au_plus_une(contrainte,n) ;
  char* f2 = au_moins_une(contrainte,n) ;
  char* result = malloc(strlen(f1) + strlen(f2) + 4);
  sprintf(result, "(%s&%s)", f1, f2);
  return result ; 
}

char* contrainte_une_colonne(int j, int n){
  char** contrainte = malloc(n*sizeof(char*));
  for(int i = 0 ; i<n ; i++){
    contrainte[i] = variable(i,j);
  }
  char* f1 = au_plus_une(contrainte,n) ;
  char* f2 = au_moins_une(contrainte,n) ;
  char* result = malloc(strlen(f1) + strlen(f2) + 4);
  sprintf(result, "(%s&%s)", f1, f2);
  return result ; 
}

char* contrainte_toutes_lignes(int n) {
  char* contrainte = malloc(2) ;
  int taille = 2 ; 
  char* contrainte_ligne ; 
  contrainte[0] = '(' ; 
  contrainte[1] = '\0' ;
  for (int i = 0 ; i<n ; i++) {
    contrainte_ligne = contrainte_une_ligne(i,n);
    taille += strlen(contrainte_ligne) + 1 ;
    contrainte = realloc(contrainte, taille);
    strcat(contrainte,contrainte_ligne);
    free(contrainte_ligne);
    if (i == n-1) {
      strcat(contrainte,")");
    } else {
      strcat(contrainte,"&");
    }
  }
  return contrainte ; 
}

char* contrainte_toutes_colonnes(int n) {
  char* contrainte = malloc(2) ;
  int taille = 2 ; 
  char* contrainte_ligne ; 
  contrainte[0] = '(' ; 
  contrainte[1] = '\0' ;
  for (int j = 0 ; j<n ; j++) {
    contrainte_ligne = contrainte_une_colonne(j,n);
    taille += strlen(contrainte_ligne) + 1 ;
    contrainte = realloc(contrainte, taille);
    strcat(contrainte,contrainte_ligne);
    free(contrainte_ligne);
    if (j == n-1) {
      strcat(contrainte,")");
    } else {
      strcat(contrainte,"&");
    }
  }
  return contrainte ; 
}

char* contrainte_une_diag_gauchedroite(int i, int j, int n){
  int nb_elts = n-i-j ;
  char** contrainte = malloc((nb_elts)*sizeof(char*));
  int indice = 0 ;
  while (i<n && j<n) {
    contrainte[indice] =  variable(i,j) ;
    i++;
    j++;
    indice++;
  }
  char* f1 = au_plus_une(contrainte,nb_elts) ;
  char* result = malloc(strlen(f1) + 3);
  sprintf(result, "(%s)", f1);
  return result ; 
}

char* contrainte_une_diag_droitegauche(int i, int j, int n){
  int nb_elts = j - i + 1 ;
  char** contrainte = malloc((nb_elts)*sizeof(char*));
  int indice = 0 ;
  while (i<n && j>=0) {
    contrainte[indice] =  variable(i,j) ;
    i++;
    j--;
    indice++;
  }
  char* f1 = au_plus_une(contrainte,nb_elts) ;
  char* result = malloc(strlen(f1) + 3);
  sprintf(result, "(%s)", f1);
  return result ; 
}

char* contrainte_toutes_diagonales(int n) {
  char* result = malloc(2);
  int taille = 2 ; 
  result[0] = '(';
  result[1] = '\0' ;
  char* result_temp ; 
  for (int i = 0 ; i<n ; i++){
    result_temp = contrainte_une_diag_gauchedroite(i,0,n);
    taille += strlen(result_temp) +1;
    result = realloc(result, taille);
    strcat(result,result_temp);
    free(result_temp);
    strcat(result,"&");    
  }
  for (int j = 1 ; j<n ; j++){
    result_temp = contrainte_une_diag_gauchedroite(0,j,n);
    taille += strlen(result_temp) +1;
    result = realloc(result, taille);
    strcat(result,result_temp);
    free(result_temp);
    strcat(result,"&");    
  }
  for (int i = 0 ; i<n ; i++){
    result_temp = contrainte_une_diag_droitegauche(i,n-1,n);
    taille += strlen(result_temp) +1;
    result = realloc(result, taille);
    strcat(result,result_temp);
    free(result_temp);
    strcat(result,"&");    
  }
  for (int j = 1 ; j<n ; j++){
    result_temp = contrainte_une_diag_droitegauche(0,j,n);
    taille += strlen(result_temp) +1;
    result = realloc(result, taille);
    strcat(result,result_temp);
    free(result_temp);
    if (j = n-1){
      strcat(result,")");
    } else {
      strcat(result,"&"); 
    }
  }
  return result ; 
}

void gen_formule_n_dames(int n, char* filename){
  FILE* f = fopen(filename, "w");
  char* temp = contrainte_toutes_lignes(n);
  fprintf(f,"%s", temp);
  free(temp);
  fprintf(f,"%s","&");
  temp = contrainte_toutes_colonnes(n) ;
  fprintf(f,"%s", temp);
  free(temp);
  fprintf(f,"%s","&");
  temp = contrainte_toutes_diagonales(n) ;
  fprintf(f,"%s", temp);
  free(temp);
  fclose(f);
}

int main(){
  gen_formule_n_dames(5, "test_eva.txt");
  return 0 ; 
}