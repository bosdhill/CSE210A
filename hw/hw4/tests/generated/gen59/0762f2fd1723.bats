load ../../harness

@test "0762f2fd1723" {
  check 'if (true ∨ 3    +     y   <    I   +y)      then  y    :=   go +z  else   skip ' '⇒ y := (go+z), {}
⇒ skip, {y → 0}'
}
