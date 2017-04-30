#!/usr/bin/env php
<?php

# Usage:
#  generate-password.php <user_id> <old_hash>

$secret1 = 4273249;
$secret2 = 5593633;

if (!isset($argv[2])) {
  # First password for this user
  $secret3 = crc32($argv[1]);
} else {
  # Existing user, password reset
  $secret3 = crc32($argv[2]);
}

$counter = $secret3;

for ($i=0; $i<10000000; $i++) {
  # This loop makes the passwords hard to reverse
  $counter = ($counter * $secret1) % $secret2;
}

$password = "";
for ($i=0; $i<10; $i++) {
  # Generate random passwords
  $counter = ($counter * $secret1) % $secret2;
  $password .= chr($counter % 94 + 33);
}

$hash = md5($password);
echo "$password $hash\n";
