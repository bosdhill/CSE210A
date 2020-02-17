load ../../harness

@test "f69a11937d1e" {
  check 'if (x*     z     =     z -    y1 ∨     true)   then 

x    :=Kx   else skip     ' '⇒ x := Kx, {}
⇒ skip, {x → 0}'
}
