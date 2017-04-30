#!/usr/bin/env php
<?php

# Usage:
#  generate-password.php <user_id> <old_hash>

$secret1 = 6533205;
$secret2 = 2340262;

if (!isset($argv[2])) {
  # First password for this user
  $secret3 = crc32($argv[1]);
} else {
  # Existing user, password reset
  $secret3 = crc32($argv[2]);
}

$counter = $secret3;

echo $secret3 . "\n";

for ($i=0; $i<10000000; $i++) {
  # This loop makes the passwords hard to reverse
  $counter = ($counter * $secret1) % $secret2;
}

echo $counter . "\n";
echo "\n";

$password = "";
for ($i=0; $i<10; $i++) {
  # Generate random passwords
  $counter = ($counter * $secret1) % $secret2;
  echo 'counter ' . $counter .' password ' .$password . ' mod ' . ($counter % 94 + 33) . ' char ' . chr($counter % 94 + 33) . "\n";
  $password .= chr($counter % 94 + 33);
}

$hash = md5($password);
echo "$password $hash\n";
