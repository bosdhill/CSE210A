load ../../harness

@test "b14f5e45af30" {
  check 'z :=    y    *   3    ;   
 r     := y   *   1  ' '⇒ skip; r := (y*1), {z → 0}
⇒ r := (y*1), {z → 0}
⇒ skip, {r → 0, z → 0}'
}
