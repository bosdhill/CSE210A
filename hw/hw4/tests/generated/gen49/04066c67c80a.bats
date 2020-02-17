load ../../harness

@test "04066c67c80a" {
  check 'if (¬false)  then skip    else y     :=H-z    ' '⇒ skip, {}'
}
