load ../../harness

@test "2d43fae20782" {
  check 'while x  +    y     <3  -     -4    ∧z     * y  =    -2     *    2     do    skip' '⇒ skip, {}'
}
