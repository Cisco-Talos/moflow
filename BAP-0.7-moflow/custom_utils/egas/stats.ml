module L = List
module Pr = Printf

class timer (name:string) =
  object
    val mutable count = 0
    val mutable total_time = 0.0
    val mutable t0 = 0.0
    method get_total_time = total_time
    method start = t0 <- Unix.time() 
    method stop = 
      total_time <- total_time +. (Unix.time() -. t0);
      count <- count + 1
    method to_str big_total = 
      let perc = (total_time /. big_total) *. 100.0 in
      Pr.sprintf "%.0f%%\t%.0fs\t%d\t%s" perc total_time count name
    method set_count c = count <- c
    method set_total_time t = total_time <- t
  end;;

let tIMERS : timer list ref = ref []
let fAKE_TIMER = new timer "unaccounted"

let register_timer timer = 
  tIMERS := timer::!tIMERS

let dump_timers wall_total = 
  let cmp timer1 timer2 = 
    let r = timer1#get_total_time -. timer2#get_total_time in
    - (int_of_float r)
  in
  let timers = L.sort cmp !tIMERS in
  let strs = L.map (fun t -> t#to_str wall_total) timers in
  let total = L.fold_left (fun acc t -> acc +. t#get_total_time) 0.0 timers in
  let _ = fAKE_TIMER#set_count 1 in
  let _ = fAKE_TIMER#set_total_time (wall_total -. total) in
  let last_stat = fAKE_TIMER#to_str wall_total in
  let start = "----------STATS----------" in
  let desc = "%\ttotal\tcount\tdesc" in
  let fin = "-------------------------" in
  String.concat "\n" (start::desc::strs@[last_stat;fin])
