load ../../harness

@test "2f0f91294a6a" {
  check 'skip  ; x :=  x    +  x     ' '⇒ x := (x+x), {}
⇒ skip, {x → 0}'
}
