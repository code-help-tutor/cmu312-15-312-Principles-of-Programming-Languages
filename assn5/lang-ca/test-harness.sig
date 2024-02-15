signature TEST_HARNESS =
sig
  val tests: (unit -> string option) list
  val testEC: bool -> unit
end
