int main(int argc, char **argv)
{
  int i=0;

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

  return i;
}
