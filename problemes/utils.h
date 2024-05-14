#pragma once

char* au_moins_une(char** l, int n);
char* au_plus_une(char** l, int n);

int taille_str_au_moins_une(int n, int var_length);
int taille_str_au_plus_une(int n, int var_length);

void free_liste(char** l, int n);
