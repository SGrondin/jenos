open! Core_kernel

open Int64

type t = {
  mutable previous: int64;
  cooldown: int64;
  mutex: Lwt_mutex.t;
}

let create ~cooldown = {
  previous = 0L;
  cooldown;
  mutex = Lwt_mutex.create ();
}

let check latch now = now > (latch.previous + latch.cooldown)

let trigger latch now = latch.previous <- now

let wait_and_trigger latch now =
  let next = latch.previous + latch.cooldown in
  if now > next
  then begin
    latch.previous <- now;
    Lwt.return_unit
  end
  else begin
    Lwt_mutex.with_lock latch.mutex (fun () ->
      let s = (next - now) // 1_000_000_000L in
      let%lwt () = Lwt_unix.sleep s in
      latch.previous <- now;
      Lwt.return_unit
    )
  end
