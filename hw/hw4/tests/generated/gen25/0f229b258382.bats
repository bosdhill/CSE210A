load ../../harness

@test "0f229b258382" {
  check 'if (false    ∨y  -     x <3*    Q)    then x := z* 4 else 

 x    :=x     +     3    ' '⇒ x := (x+3), {}
⇒ skip, {x → 3}'
}
