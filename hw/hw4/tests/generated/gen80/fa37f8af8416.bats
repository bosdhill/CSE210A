load ../../harness

@test "fa37f8af8416" {
  check 'x:=  Ad  +2 ;x    :=   x   *   x     ' '⇒ skip; x := (x*x), {x → 2}
⇒ x := (x*x), {x → 2}
⇒ skip, {x → 4}'
}
