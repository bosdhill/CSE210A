load ../../harness

@test "7051609ce010" {
  check 'x:=    2  *   0     ; skip    ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
