load ../../harness

@test "4934ec237221" {
  check 'skip  ;g:=3    * 3     ' '⇒ g := (3*3), {}
⇒ skip, {g → 9}'
}
