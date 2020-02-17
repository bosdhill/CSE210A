load ../../harness

@test "92a441e98c67" {
  check 'z:= 1    +     4;y:=   V   - y    ' '⇒ skip; y := (V-y), {z → 5}
⇒ y := (V-y), {z → 5}
⇒ skip, {y → 0, z → 5}'
}
