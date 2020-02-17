load ../../harness

@test "0a9f4b72cb7c" {
  check 'x  :=    y     * 3   ;  
I4  :=  z   +   3     ' '⇒ skip; I4 := (z+3), {x → 0}
⇒ I4 := (z+3), {x → 0}
⇒ skip, {I4 → 3, x → 0}'
}
