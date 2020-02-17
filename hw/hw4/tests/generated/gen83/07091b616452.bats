load ../../harness

@test "07091b616452" {
  check 'while (¬true)     do z    :=   3     *     Ts     ' '⇒ skip, {}'
}
