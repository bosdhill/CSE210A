load ../../harness

@test "a9f97a5fee5c" {
  check 'if (¬false)   then  
z :=2 +x else y :=  y    *z  ' '⇒ z := (2+x), {}
⇒ skip, {z → 2}'
}
