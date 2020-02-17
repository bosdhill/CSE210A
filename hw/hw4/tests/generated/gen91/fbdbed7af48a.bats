load ../../harness

@test "fbdbed7af48a" {
  check 'z:=z-y ; 
  x :=  z-    x  ' '⇒ skip; x := (z-x), {z → 0}
⇒ x := (z-x), {z → 0}
⇒ skip, {x → 0, z → 0}'
}
