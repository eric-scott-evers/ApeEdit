
// #include <stdio.h>
// every thing needs \n  

#include <locale.h>
#include <curses.h>
#include <stdlib.h>

int main(){
  setlocale(LC_ALL, "");
  initscr();

  printw("\u20ac \n");
  return 0;
}


