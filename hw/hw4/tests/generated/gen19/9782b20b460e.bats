load ../../harness

@test "9782b20b460e" {
  check 'z:=  3  ;N  :=    x *  OM   ' '⇒ skip; N := (x*OM), {z → 3}
⇒ N := (x*OM), {z → 3}
⇒ skip, {N → 0, z → 3}'
}
