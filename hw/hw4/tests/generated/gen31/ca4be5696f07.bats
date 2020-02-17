load ../../harness

@test "ca4be5696f07" {
  check 'if (true   ∧   x -     z     <  z*  y)      then z     :=    -3   +     x     else 
 skip' '⇒ skip, {}'
}
