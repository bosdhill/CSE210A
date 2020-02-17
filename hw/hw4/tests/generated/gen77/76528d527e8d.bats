load ../../harness

@test "76528d527e8d" {
  check 'while C+  M   =  x  -4     ∧ true      do  x:=y     -  3' '⇒ skip, {}'
}
