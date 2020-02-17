load ../../harness

@test "614f48c372c0" {
  check 'if (¬((x     +Dk)    -   3   <    y     -   x))      then skip  else   

 y   := -2  +x  ' '⇒ y := (-2+x), {}
⇒ skip, {y → -2}'
}
