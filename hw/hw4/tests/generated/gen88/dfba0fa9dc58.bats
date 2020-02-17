load ../../harness

@test "dfba0fa9dc58" {
  check 'z     := y     +x    ; 
skip' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
