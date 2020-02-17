load ../../harness

@test "e1dc9a37b25a" {
  check 'if (¬true)   then  



skip     else     x:=   z    + z    ' '⇒ x := (z+z), {}
⇒ skip, {x → 0}'
}
