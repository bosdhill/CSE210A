load ../../harness

@test "67014418197e" {
  check 'z   := 3     -    -2  ;M4:=  -3 +0' '⇒ skip; M4 := (-3+0), {z → 5}
⇒ M4 := (-3+0), {z → 5}
⇒ skip, {M4 → -3, z → 5}'
}
