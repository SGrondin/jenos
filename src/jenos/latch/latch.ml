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

module Time = struct
  let get () = Time_now.nanoseconds_since_unix_epoch () |> Int63.to_int64
  let min = 60L * 1_000_000_000L
  let sec = 1_000_000_000L
  let ms = 1_000_000L
end

let check ?(now = Time.get ()) latch = now > (latch.previous + latch.cooldown)

let trigger ?(now = Time.get ()) latch = latch.previous <- now

let wait_and_trigger ?(now = Time.get ()) ?custom_cooldown latch =
  let cooldown = Option.value custom_cooldown ~default:latch.cooldown in
  let next = latch.previous + cooldown in
  if now > next
  then begin
    latch.previous <- now;
    Lwt.return_unit
  end
  else begin
    Lwt_mutex.with_lock latch.mutex (fun () ->
      let%lwt () = Lwt_unix.sleep ((next - now) // Time.sec) in
      latch.previous <- now;
      Lwt.return_unit
    )
  end
