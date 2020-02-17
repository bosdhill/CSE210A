load ../../harness

@test "639c3fa48cdd" {
  check 'xx:=0   *   x ; 

skip' '⇒ skip; skip, {xx → 0}
⇒ skip, {xx → 0}'
}
