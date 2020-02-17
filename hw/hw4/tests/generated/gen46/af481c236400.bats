load ../../harness

@test "af481c236400" {
  check 'if (¬(x=x    *     3))  then   skip      else skip     ' '⇒ skip, {}'
}
