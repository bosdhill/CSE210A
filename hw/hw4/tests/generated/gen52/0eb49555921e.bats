load ../../harness

@test "0eb49555921e" {
  check 'z  := -4-     KV     ;   U := 4  + z' '⇒ skip; U := (4+z), {z → -4}
⇒ U := (4+z), {z → -4}
⇒ skip, {U → 0, z → -4}'
}
