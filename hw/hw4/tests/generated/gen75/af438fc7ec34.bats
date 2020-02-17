load ../../harness

@test "af438fc7ec34" {
  check 'if (x   +     -2  <     z     -     x     ∨    y*    x  =     z) then  b    :=    gB  +   y      else 
skip     ' '⇒ b := (gB+y), {}
⇒ skip, {b → 0}'
}
