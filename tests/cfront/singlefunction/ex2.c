// Statement after return
// on both branches of if
// The resulting graph is disconnected
int main(int argc, char **argv)
{
  int i=0;
  if (rand()) {
    return 0;
  } else {
    return 1;
  }
  i++;
  return i;
}
