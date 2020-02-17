load ../../harness

@test "fcf8a30ec1b6" {
  check 'if (t   +    x    <    y     +0    ∨     false)     then 

  skip    else  x :=    z   -   -1   ' '⇒ x := (z--1), {}
⇒ skip, {x → 1}'
}
