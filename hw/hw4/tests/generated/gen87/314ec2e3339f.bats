load ../../harness

@test "314ec2e3339f" {
  check 'if (¬(-2     *2    <2    +   z))      then skip     else skip ' '⇒ skip, {}'
}
