open! Core_kernel

type t = {
  mutable previous: int64;
  cooldown: int64;
}

let create ~cooldown = {
  previous = 0L;
  cooldown;
}

let check latch now = Int64.(now > (latch.previous + latch.cooldown))

let trigger latch now = latch.previous <- now
