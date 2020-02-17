load ../../harness

@test "85bd1d908743" {
  check 'y     := s   -y   ;
z:=   x     +     x' '⇒ skip; z := (x+x), {y → 0}
⇒ z := (x+x), {y → 0}
⇒ skip, {y → 0, z → 0}'
}
