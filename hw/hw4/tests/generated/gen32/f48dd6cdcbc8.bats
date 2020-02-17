load ../../harness

@test "f48dd6cdcbc8" {
  check 'y     :=y    *  3    ;
 z:=    Wt  *   2' '⇒ skip; z := (Wt*2), {y → 0}
⇒ z := (Wt*2), {y → 0}
⇒ skip, {y → 0, z → 0}'
}
