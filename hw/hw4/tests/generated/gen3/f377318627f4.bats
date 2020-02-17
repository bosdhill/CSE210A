load ../../harness

@test "f377318627f4" {
  check 'z    :=    1   + z   ;

skip' '⇒ skip; skip, {z → 1}
⇒ skip, {z → 1}'
}
