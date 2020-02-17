load ../../harness

@test "b107e5a3d11d" {
  check 'x:=z   ; 
  x :=    x    +    x     ' '⇒ skip; x := (x+x), {x → 0}
⇒ x := (x+x), {x → 0}
⇒ skip, {x → 0}'
}
