load ../../harness

@test "4bc54e5b5fdf" {
  check 'if (true∨   true)  then  x   := x-     z     else 
    skip  ' '⇒ x := (x-z), {}
⇒ skip, {x → 0}'
}
