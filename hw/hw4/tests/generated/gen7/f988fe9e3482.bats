load ../../harness

@test "f988fe9e3482" {
  check 'if (¬false) then  
 e   :=     2   + E3      else skip   ' '⇒ e := (2+E3), {}
⇒ skip, {e → 2}'
}
