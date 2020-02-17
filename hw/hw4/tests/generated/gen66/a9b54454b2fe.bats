load ../../harness

@test "a9b54454b2fe" {
  check 'if (¬(x=  3+  y))    then  g     :=-4  else  x  :=  y' '⇒ g := -4, {}
⇒ skip, {g → -4}'
}
