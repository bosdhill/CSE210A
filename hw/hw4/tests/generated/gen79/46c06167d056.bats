load ../../harness

@test "46c06167d056" {
  check 'y := y -     z     ;

skip   ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
