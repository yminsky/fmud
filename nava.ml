open Core
open Mudder

(*
   | ████████ ██    ██ ██████  ███████ ███████
   |    ██     ██  ██  ██   ██ ██      ██
   |    ██      ████   ██████  █████   ███████
   |    ██       ██    ██      ██           ██
   |    ██       ██    ██      ███████ ███████
*)

type door =
  { direction : string
  ; destination : string
  }

type item =
  {in_room : bool
  ; place : string
  ; description : string
  }

type person =
  { nick: string
  ; roomn: string
  }

type room =
  { name: string
  ; doors: door list
  ; description: string
  }

type world =
  { people: person list
  ; rooms: room list
  ; items: item list
  }


  (*
   | ████████ ███████ ██   ██ ████████
   |    ██    ██       ██ ██     ██
   |    ██    █████     ███      ██
   |    ██    ██       ██ ██     ██
   |    ██    ███████ ██   ██    ██
*)

let description = {|
  Hey! This is a MUD! Enjoy.

  Look around, and see what you can find. We don't
  have many features yet, but we'll have more soon!
  |}

let help = {|
  Hey, it's a baby MUD!

  Not much here yet, but:

    Type /leave to exit the game
    Type /help for this message
    Type /whisper and who you are whispering to to send a
    message to that person
    And anything else you type is sent to everyone else who
    is logged in.

  Have fun!
  |}

  (*
   |  ██████   █████  ███    ███ ███████
   | ██       ██   ██ ████  ████ ██
   | ██   ███ ███████ ██ ████ ██ █████
   | ██    ██ ██   ██ ██  ██  ██ ██
   |  ██████  ██   ██ ██      ██ ███████
*)

(* Items *)

let pebble = { in_room = true;
               place = "Welcome";
               description = "a normal gray pebble."}
(* Doors *)

let n_welcome = { direction = "north"; destination = "Basic" }
let s_basic   = { direction = "south"; destination = "Welcome" }

(* Rooms *)

let welcome =
  { name = "Welcome"
  ; doors = [ n_welcome ]
  ; description = {|
Welcome to the room! There is a door to the north.
|}
  }

let basic =
  { name = "Basic"
  ; doors = [ s_basic ]
  ; description = {|
This is like, the most boring room ever.
There is a door to the south.
|}
  }

let init =
  { people = []
  ; rooms = [ basic; welcome ]
  ; items = [pebble]
  }

(*
   | ██   ██ ███████ ██      ██████  ███████ ██████  ███████
   | ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
   | ███████ █████   ██      ██████  █████   ██████  ███████
   | ██   ██ ██      ██      ██      ██      ██   ██      ██
   | ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████
*)

let get_person w nick =
  List.find_exn w.people ~f:(fun p -> p.nick = nick)

let get_people_in_room w roomn =
  List.filter w.people ~f:(fun p -> p.roomn = roomn)

let get_room w nick =
  let roomn = (get_person w nick).roomn in
  List.find_exn w.rooms ~f:(fun r -> r.name = roomn)

let drop_nick nick people =
  List.filter people ~f:(fun p -> not (p.nick = nick))

let other_people w nick =
  drop_nick nick w.people

let other_people_in_room w nick roomn =
  drop_nick nick (get_people_in_room w roomn)

(*
   |  █████   ██████ ████████ ██  ██████  ███    ██ ███████
   | ██   ██ ██         ██    ██ ██    ██ ████   ██ ██
   | ███████ ██         ██    ██ ██    ██ ██ ██  ██ ███████
   | ██   ██ ██         ██    ██ ██    ██ ██  ██ ██      ██
   | ██   ██  ██████    ██    ██  ██████  ██   ████ ███████
*)

let looking w nick =
  let room = get_room w nick in
  let roomn = room.name in
  let people_in_room = drop_nick nick (get_people_in_room w roomn) in
  let people_description =
    match people_in_room with
    | [] -> [ "Nobody is in the room." ]
    | _ ->
      let names_in_room = List.map people_in_room ~f:(fun p -> p.nick) in
      [ "You see"
      ; String.concat ~sep:" and " names_in_room
      ]
  in
  let items_in_room = 
    List.filter w.items ~f:(fun i ->i.in_room = true && i.place = roomn) 
  in
  let item_description =
    match items_in_room with
    | [] -> []
    | _ ->
      let descriptions = List.map items_in_room ~f:(fun i -> i.description) in
      [ "You see"
      ; String.concat ~sep:" and " descriptions
      ]
  in
  String.concat ~sep:" " (room.description :: people_description @ item_description)

let moving w nick direction =
  let me = List.find_exn w.people ~f:(fun p -> p.nick = nick) in
  let roomh = me.roomn in
  let room = List.find_exn w.rooms ~f:(fun r -> roomh = r.name) in
  let door = List.find room.doors ~f:(fun d -> d.direction = direction) in
  match door with
  | None ->
    let action =
      Send_message {nick; message = "Sorry! You can't go in that direction!"}
    in
    (w, [action])
  | Some door ->
    let new_me = { me with roomn = door.destination } in
    let nw = { w with people = new_me :: other_people w nick } in
    let leaving_message = String.concat ~sep:" " [ nick; "has left the room." ] in
    let leaving =
      List.map (get_people_in_room nw roomh)
        ~f:(fun p -> Send_message { nick = p.nick; message = leaving_message })
    in
    let entering_message = String.concat ~sep:" " [ nick; "has entered the room." ] in
    let entering =
      List.map (get_people_in_room w door.destination)
        ~f:(fun p -> Send_message { nick = p.nick; message = entering_message })
    in
    let actions =
      List.concat
        [ [ Send_message { nick; message = looking nw nick }]
        ; leaving
        ; entering
        ]
    in
    (nw,actions)

let nick_added w nick =
  let person = { nick; roomn = "Welcome" } in
  let nw = { w with people = person :: w.people } in
  let welcome_message =
    Send_message { nick; message = String.concat ["Welcome to the MUD, "; nick; "!"] }
  in
  let hello_messages =
    List.map w.people ~f:(fun p ->
      Send_message {nick = p.nick; message = nick ^ " has arrived!"})
  in
  let actions = welcome_message :: hello_messages in
  (nw,actions)

let nick_removed w nick =
  let nw = { w with people = other_people w nick } in
  let goodbye_message = nick ^ " has left the world." in
  let actions = List.map (other_people w nick) ~f:(fun p ->
    Send_message { nick = p.nick; message = goodbye_message })
  in
  (nw,actions)

let handle_line w nick line =
  match String.split ~on:' ' line with
  | "" :: [] | [] -> (w,[])
  | "/leave" :: [] -> (w, [Kill_client { nick }])
  | "/help"  :: [] -> (w, [Send_message { nick;message = help }])
  | "/look"  :: [] -> (w, [Send_message { nick; message = looking w nick }])
  | "/move"  :: dir :: [] -> moving w nick dir
  | "/whisper" :: to_nick :: message ->
    (w, [Send_message { nick = to_nick  
                      ; message = nick ^ " whispered: " ^ String.concat message }])
  | _ ->
    (* just show what was said to everyone in the same room *)
    let hearers =
      drop_nick nick (get_people_in_room w (get_person w nick).roomn)
    in
    let actions =
      let message = nick ^ ": " ^ line in
      List.map hearers ~f:(fun p -> Send_message { nick = p.nick; message })
    in
    (w,actions)

let () =
  start_mud ~port:12321
    { init
    ; description
    ; handle_line
    ; nick_added
    ; nick_removed
    }
