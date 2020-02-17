load ../../harness

@test "caf55c981065" {
  check 'if (z     <    x *3) then  


skip  else  x  :=   x *    -2' '⇒ x := (x*-2), {}
⇒ skip, {x → 0}'
}
