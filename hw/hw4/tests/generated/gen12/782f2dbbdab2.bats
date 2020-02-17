load ../../harness

@test "782f2dbbdab2" {
  check 'skip    ;S8   :=y   --3 ' '⇒ S8 := (y--3), {}
⇒ skip, {S8 → 3}'
}
