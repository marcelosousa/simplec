// #include <stdlib.h>
int f() {
  int i=0;

  // Normal If Statement
  if (rand()){
    i--;
  } else {
    i++;
  }

  // If Then Statement
  if (rand()){
    i--;
  }

}

int main(int argc, char **argv)
{
  int i=0;


  // Nested if with return
  if (rand()){
    if (rand()) {
          return 0; 
    } 
  }

  // Simple while loop with break and continue
  while (i < 10) {
    if (i > 2) {
      continue;
    } else {
      break;
    }
  }

  return i;
}
