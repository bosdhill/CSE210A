load ../../harness

@test "bd71af65ac7e" {
  check 'x :=  0 * z    ; z     :=   x  -  x     ' '⇒ skip; z := (x-x), {x → 0}
⇒ z := (x-x), {x → 0}
⇒ skip, {x → 0, z → 0}'
}
