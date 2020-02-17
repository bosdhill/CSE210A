load ../../harness

@test "6037c976cdb6" {
  check 'if (¬true)    then  skip  else y   :=     1     *  x     ' '⇒ y := (1*x), {}
⇒ skip, {y → 0}'
}
