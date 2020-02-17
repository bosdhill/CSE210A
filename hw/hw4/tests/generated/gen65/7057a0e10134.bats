load ../../harness

@test "7057a0e10134" {
  check 'if (¬(x+ -4    =     1    -    0))      then x  := 4    else  
skip  ' '⇒ x := 4, {}
⇒ skip, {x → 4}'
}
