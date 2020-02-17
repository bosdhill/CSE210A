load ../../harness

@test "33e58595831a" {
  check 'if (¬false)    then  S   :=   y    else    z     :=  1     *   -2   ' '⇒ S := y, {}
⇒ skip, {S → 0}'
}
