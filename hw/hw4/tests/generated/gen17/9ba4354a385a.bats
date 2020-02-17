load ../../harness

@test "9ba4354a385a" {
  check 'if false    then x :=   x     *    z    else y :=     Q1  *    eU' '⇒ y := (Q1*eU), {}
⇒ skip, {y → 0}'
}
