load ../../harness

@test "ce13c9876122" {
  check 'x:= y- -3  ; 
   x  :=x- x' '⇒ skip; x := (x-x), {x → 3}
⇒ x := (x-x), {x → 3}
⇒ skip, {x → 0}'
}
