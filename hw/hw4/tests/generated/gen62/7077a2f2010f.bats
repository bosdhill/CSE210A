load ../../harness

@test "7077a2f2010f" {
  check 'AQ  :=  -3     + z    ;
y  :=z   *  -2 ' '⇒ skip; y := (z*-2), {AQ → -3}
⇒ y := (z*-2), {AQ → -3}
⇒ skip, {AQ → -3, y → 0}'
}
