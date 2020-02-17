load ../../harness

@test "686bb8e58a7c" {
  check 'b     :=  z*3    ; 
x :=   y     -  -1' '⇒ skip; x := (y--1), {b → 0}
⇒ x := (y--1), {b → 0}
⇒ skip, {b → 0, x → 1}'
}
