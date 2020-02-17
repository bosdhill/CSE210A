load ../../harness

@test "f364d1a556dc" {
  check 'z    :=    1   - y     ; 
 
 y   :=   x *    y    ' '⇒ skip; y := (x*y), {z → 1}
⇒ y := (x*y), {z → 1}
⇒ skip, {y → 0, z → 1}'
}
