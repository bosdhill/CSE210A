load ../../harness

@test "c0da79324f29" {
  check 'skip    ;y:=o     ;
skip     ' '⇒ y := o; skip, {}
⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
