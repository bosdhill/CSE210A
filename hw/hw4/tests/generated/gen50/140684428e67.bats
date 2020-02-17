load ../../harness

@test "140684428e67" {
  check 'y   :=   x  *     -3  *    1 ;

skip     ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
