load ../../harness

@test "082492acc363" {
  check 'z:=d +     z   ' '⇒ skip, {z → 0}'
}
