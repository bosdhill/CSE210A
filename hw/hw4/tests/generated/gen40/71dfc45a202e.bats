load ../../harness

@test "71dfc45a202e" {
  check 'of:= zk  *     1  ;z     := y     +     z  ' '⇒ skip; z := (y+z), {of → 0}
⇒ z := (y+z), {of → 0}
⇒ skip, {of → 0, z → 0}'
}
