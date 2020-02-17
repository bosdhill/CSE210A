load ../../harness

@test "26912b104832" {
  check 'skip     ;  v:=   y+  -1 ' '⇒ v := (y+-1), {}
⇒ skip, {v → -1}'
}
