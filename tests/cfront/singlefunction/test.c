// #include <stdlib.h>

int main(int argc, char **argv)
{
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

  // If with return on then branch
  if (rand()){
    return 0;
  }

  // If with return on else branch
  if (rand()){
    i = 5;
  } else {
    return 1;
  }

  // Nested if
  if (rand()){
    if (rand()) {
      i++;
    } else {
      i--;
    }
  }

  // Nested if with return
  if (rand()){
    if (rand()) {
          return 0; 
    } 
  }

  // Nested if with return
  if (rand()){
    if (rand()) {
      return 0; 
    } else {
      return 1;
    }
  }

  // Simple while loop
  while (i < 10) {
    i++;
  }

  // Simple nested loop
  int j = 0;
  while (i < 10) {
    while (j < 10) {
      i++; j++;
    }
  }
  // Simple while loop with continue 
  while (i < 10) {
    if (i > 2) {
      continue;
    } else {
      i++;
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

  j = 0;

  while (i < 10) {
     if (rand()){
       break;
     } else {
       while (j < 10) {
         if (i > 5) {
           j++; continue;
         } else {
           break;
         }
       }
       i++;
     }
     i++;
  }
  return i;
}
