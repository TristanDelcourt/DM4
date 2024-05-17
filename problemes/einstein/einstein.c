
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* traduit que pour un element, au plus une maison y correspond */
char* au_plus_une_maison (char* elt){
  char* result = malloc(2) ;
  int taille = 2 ; 
  result[0] ='('; 
  result[1] = '\0' ;
  char* temp = malloc(11 + 2*strlen(elt));
  for (int i = 1 ; i<=4 ; i++){
    for (int j = i+1 ; j<=5 ; j++){
      if (i == 4 && j==5) {
        sprintf(temp,"(~%s_%d|~%s_%d))",elt,i,elt,j);
      } else {
        sprintf(temp,"(~%s_%d|~%s_%d)&",elt,i,elt,j);
      }
      taille += 11 + 2*strlen(elt) ;
      result = realloc(result,taille);
      strcat(result,temp);
    }
  }
  free(temp);
  return result ; 
}

/* traduit que pour une maison, au plus un element y correspond*/
char* au_plus_un_elt (int k, char** l){
  char* result = malloc(2) ;
  int taille = 2 ; 
  result[0] ='('; 
  result[1] = '\0' ;
  char* temp ; 
  int taille_temp ;
  for (int i = 0 ; i<4 ; i++){
    for (int j = i+1 ; j<5 ; j++){
      taille_temp = 11 + strlen(l[i]) + strlen(l[j]) ;
      temp = malloc(taille_temp);
      if (i == 3 && j==4) {
        sprintf(temp,"(~%s_%d|~%s_%d))",l[i],k,l[j],k);
      } else {
        sprintf(temp,"(~%s_%d|~%s_%d)&",l[i],k,l[j],k);
      }
      taille += taille_temp ;
      result = realloc(result,taille);
      strcat(result,temp);
      free(temp);
    }
  }
  return result ; 
}

char* au_moins_une_maison (char* elt){
  int taille = 2;
  char* formule = malloc(taille);

  formule[0] = '(';
  formule[1] = '\0';

  char* temp = malloc(strlen(elt) + 3);

  for(int i = 1; i <=5 ; i++){
      sprintf(temp, "%s_%d", elt, i);
      taille+=strlen(temp) + 1;
      formule = realloc(formule, taille);
      strcat(formule, temp);
      if (i!=5)
        strcat(formule, "|");
      else
        strcat(formule, ")");
  }
  free(temp);
  return formule;
}

char* au_moins_un_elt (int k, char** l){
  int taille = 2;
  char* formule = malloc(taille);

  formule[0] = '(';
  formule[1] = '\0';

  int taille_temp = strlen(l[0]) + 3;
  char* temp = malloc(taille_temp);

  for(int i = 0; i < 5 ; i++){
      int taille_temp = strlen(l[i]) + 3;
      temp = realloc(temp, taille_temp);
      sprintf(temp, "%s_%d", l[i], k);
      taille+=strlen(temp) + 1;
      formule = realloc(formule, taille);
      strcat(formule, temp);
      if (i!=4)
        strcat(formule, "|");
      else
        strcat(formule, ")");
  }
  free(temp);
  return formule;
}

char* unicite (char** l){
  int taille = 2;
  char* formule = malloc(taille);

  formule[0] = '(';
  formule[1] = '\0';

  char* temp ;

  for (int i = 1 ; i<=5 ; i++){
    
    temp = au_plus_un_elt(i,l);
    taille += strlen(temp) + 3;
    formule = realloc(formule, taille);
    strcat(formule,"(");
    strcat(formule,temp);
    free(temp);
    strcat(formule, ")&");

    temp = au_moins_un_elt(i,l);
    taille += strlen(temp) + 3;
    formule = realloc(formule, taille);
    strcat(formule, "(");
    strcat(formule,temp);
    free(temp);
    strcat(formule, ")&");
  }

  for (int i = 0 ; i<5 ; i++){

    temp = au_plus_une_maison(l[i]);
    taille += strlen(temp) + 3;
    formule = realloc(formule, taille);
    strcat(formule,"(");
    strcat(formule,temp);
    free(temp);
    strcat(formule,")&");

    temp = au_moins_une_maison(l[i]);
    taille += strlen(temp) + 3;
    formule = realloc(formule, taille);
    strcat(formule,"(");
    strcat(formule,temp);
    free(temp);

    if (i!=4)
      strcat(formule,")&");
    else 
      strcat(formule, "))");
  }

  return formule;
  
}


/*traduit que la maison associée à l'élément numéro 1 est voisine de celle associée à l'élément numéro 2*/
char* voisin_gauche (char* elt1, char* elt2) {
  char* result = malloc(2); 
  int taille = 2 ; 
  int taille_temp = strlen(elt1) + strlen(elt2) + 8 ;
  result[0] ='('; 
  result[1] = '\0' ;
  char* temp = malloc(taille_temp) ; 
  for (int i = 1 ; i<5 ; i++) {
    taille += taille_temp + 1 ;
    result = realloc(result, taille);
    sprintf(temp, "(%s_%d&%s_%d)",elt1,i,elt2,(i+1));
    strcat(result,temp);
    if (i!=4) {
      strcat(result,"|");
    } else {
      strcat(result,")");
    }
  }
  free(temp);
  return result ; 
}

/*traduit le fait que l'élément 1 et 2 doivent être dans la même maison*/
char* conjonction(char* elt1, char* elt2) {
  char* result = malloc(2); 
  int taille = 2 ; 
  int taille_temp = strlen(elt1) + strlen(elt2) + 8 ;
  result[0] ='('; 
  result[1] = '\0' ;
  char* temp = malloc(taille_temp) ;
  for (int i = 1 ; i<=5 ; i++) {
    taille += taille_temp + 1 ;
    result = realloc(result, taille);
    sprintf(temp, "(%s_%d&%s_%d)",elt1,i,elt2,i);
    strcat(result,temp);
    if (i!=5) {
      strcat(result,"|");
    } else {
      strcat(result,")");
    }
  }
  free(temp);
  return result ; 
}


/* traduit le fait que les maisons correspondant à un l'élément 1 et celle correspondant à l'élément 2 sont voisines*/
char* voisins(char* elt1, char* elt2) {
  char* temp_1 = voisin_gauche(elt1,elt2); 
  char* temp_2 = voisin_gauche(elt2,elt1); 
  char* result = malloc(4 + strlen(temp_1) + strlen(temp_2));
  sprintf(result, "(%s|%s)", temp_1,temp_2);
  free(temp_1);
  free(temp_2);
  return result ; 
}  


/*écrit la formule propositionnelle traduisant le problème d'Einstein dans le fichier nomfichier*/
void probleme_einstein(char* nomfichier){
  FILE* f = fopen(nomfichier, "w");
  
  char* couleurs[5] = {"rouge","bleue","jaune","blanche","verte"}; 
  char* boissons[5] = {"cafe","the","lait","yop","eau"}; 
  char* animaux[5] = {"chiens","oiseaux","chats","poisson","cheval"};
  char* sports[5] = {"velo","danse","escalade","karate","basket"}; 

  char** conditions = malloc(19*sizeof(char*));

  
  char** types_elements[4] = {couleurs , boissons , animaux , sports};
  for (int i = 0 ; i<4 ; i++) {
    conditions[i] = unicite(types_elements[i]);
  }
  
  conditions[4] = conjonction("anglais","rouge");
  conditions[5] = conjonction("suedois","chiens");
  conditions[6] = conjonction("danois","the");
  conditions[7] = voisin_gauche("verte","blanche");
  conditions[8] = conjonction("verte","cafe");
  conditions[9] = conjonction("velo","oiseaux");
  conditions[10] = conjonction("jaune","danse");
  conditions[11] = "lait_3";
  conditions[12] = "norvegien_1";
  conditions[13] = voisins("escalade","chats");
  conditions[14] = voisins("cheval","danse");
  conditions[15] = conjonction("yop","basket");
  conditions[16] = conjonction("allemand","karate");
  conditions[17] = voisins("norvegien","bleue");
  conditions[18] = voisins("escalade","eau");

  for (int i = 0 ; i<19 ; i++){
    fprintf(f,"%s",conditions[i]);
    if (i!=11 & i!=12) {
      free(conditions[i]);
    }
    if (i != 18) {
      fprintf(f,"%s", "&");
    } 
  } 
  
  fclose(f);
}



void test() {
  FILE* f = fopen("testeinstein.txt", "w");
  char* l[5] = {"rouge","bleue","jaune","blanche","verte"};
  
  char* test1 = voisin_gauche("verte","blanche");
  //fprintf(f, "%s\n\n",test1);
  free(test1);
  
  test1 = conjonction ("anglais","rouge");
  //fprintf(f, "%s\n\n",test1);
  free(test1);
  
  test1 = au_plus_une_maison("rouge");
  //fprintf(f, "%s\n\n",test1);
  free(test1);

  test1 = au_plus_un_elt(1,l);
  //fprintf(f, "%s\n\n",test1);
  free(test1);

  test1 = au_moins_une_maison ("rouge");
  //fprintf(f, "%s\n\n",test1);
  free(test1);

  test1 = au_moins_un_elt (1, l);
  //fprintf(f, "%s\n\n",test1);
  free(test1);

  test1 = unicite(l);
  fprintf(f, "%s\n\n",test1);
  free(test1);

  fclose(f);
}

int main() {
  test();
  probleme_einstein("formule_ein.txt");
  return 0 ; 
}