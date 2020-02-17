load ../../harness

@test "16e84ec1bde4" {
  check 'y     :=  z     +x   ;

 u  :=  d   *   3' '⇒ skip; u := (d*3), {y → 0}
⇒ u := (d*3), {y → 0}
⇒ skip, {u → 0, y → 0}'
}
