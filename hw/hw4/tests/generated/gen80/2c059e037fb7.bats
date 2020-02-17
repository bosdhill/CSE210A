load ../../harness

@test "2c059e037fb7" {
  check 'skip    ;x   :=  x  * 2   ' '⇒ x := (x*2), {}
⇒ skip, {x → 0}'
}
