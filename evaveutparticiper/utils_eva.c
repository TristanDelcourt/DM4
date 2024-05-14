#include <string.h> 
#include <stdio.h>
#include <stdlib.h>

char* au_moins_une(char** l, int n) {
  // I love you
  int taille = 1;
  char* disjonction = malloc(1);
  char c ; 
  int j ; 
  disjonction[0] = '(' ; 
  disjonction = realloc(disjonction, taille);
  disjonction[taille -1 ] = '(' ;
  for (int i = 0 ; i<n-1 ; i++) {
    c = l[i][0];
    j = 0 ; 
    while(c!='\0') {
      taille+= 1;
      disjonction = realloc(disjonction,taille);
      disjonction[taille-1] = c;
      j++ ; 
      c = l[i][j];
    }
    taille++;
    disjonction = realloc(disjonction,taille);
    disjonction[taille-1] = '|';
  }
  c = l[n-1][0] ;
  j = 0 ; 
  while(c!='\0') {
    taille++;
    disjonction = realloc(disjonction,taille);
    disjonction[taille-1] = c;
    j++ ; 
    c = l[n-1][j];
  }
  taille+=2;
  disjonction = realloc(disjonction,taille);
  disjonction[taille-2] = ')';
  disjonction[taille-1] = '\0';
  strcat(disjonction, ")");

  return disjonction ; 
}


char* tous(char** l, int n) {
  // I love you
  int taille = 0;
  char* disjonction = NULL;
  char c ; 
  int j ; 
  taille++ ; 
  disjonction = realloc(disjonction, taille);
  disjonction[taille -1 ] = '(' ;
  for (int i = 0 ; i<n-1 ; i++) {
    c = l[i][0];
    j = 0 ; 
    while(c!='\0') {
      taille+= 1;
      disjonction = realloc(disjonction,taille);
      disjonction[taille-1] = c;
      j++ ; 
      c = l[i][j];
    }
    taille++;
    disjonction = realloc(disjonction,taille);
    disjonction[taille-1] = '&';
  }
  c = l[n-1][0] ;
  j = 0 ; 
  while(c!='\0') {
    taille++;
    disjonction = realloc(disjonction,taille);
    disjonction[taille-1] = c;
    j++ ; 
    c = l[n-1][j];
  }
  taille+=2;
  disjonction = realloc(disjonction,taille);
  disjonction[taille-2] = ')';
  disjonction[taille-1] = '\0';

  return disjonction ; 
}

char* au_plus_une(char** l, int n) {
  char* result = malloc(2) ;
  char* l_temp[n] ;
  char* result_temp ; 
  int taille_actuelle = 2 ; 
  result[0] = '(' ;
  result[1] = '\0' ;
  for (int i = -1 ; i<n ; i++)  {
    for (int j = 0 ; j<n ; j++) {
      l_temp[j] = malloc(strlen(l[j]) + 2);
      l_temp[j][0] = '\0' ;
      if (j!=i) {
         strcat(l_temp[j],"~");
      }
      strcat(l_temp[j], l[j]);
    }
    result_temp = tous(l_temp, n);
    taille_actuelle += strlen(result_temp) + 1 ;
    result = realloc(result, taille_actuelle);
    strcat(result,result_temp);
    if (i!=n-1) {
      taille_actuelle++ ; 
      result = realloc(result, taille_actuelle);
      strcat(result,"|") ;
    }
    free(result_temp);
    for (int j = 0 ; j<n ; j++){
      free(l_temp[j]);
    }
  }
  strcat(result,")");
  return result ; 
}
