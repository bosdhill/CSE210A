load ../../harness

@test "2309e50e8cd2" {
  check 'z   := 2-    z;  skip    ' '⇒ skip; skip, {z → 2}
⇒ skip, {z → 2}'
}
