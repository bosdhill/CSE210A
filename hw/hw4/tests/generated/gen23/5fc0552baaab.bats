load ../../harness

@test "5fc0552baaab" {
  check 'while (¬true)     do 
    z    := x   -     NN  ' '⇒ skip, {}'
}
