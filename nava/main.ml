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
[@@deriving sexp]

type item =
  { in_room : bool
  ; place : string
  ; description : string
  ; name : string
  }
[@@deriving sexp]

type person =
  { nick : string
  ; roomn : string
  ; health : int
  }
[@@deriving sexp]

type room =
  { name: string
  ; doors: door list
  ; description: string
  }
[@@deriving sexp]

type world =
  { people: person list
  ; rooms: room list
  ; items: item list
  }
[@@deriving sexp]

  (*
   | ████████ ███████ ██   ██ ████████
   |    ██    ██       ██ ██     ██
   |    ██    █████     ███      ██
   |    ██    ██       ██ ██     ██
   |    ██    ███████ ██   ██    ██
*)

let description = "
  Hey! This is a MUD! Enjoy.

  Look around, and see what you can find. We don't
  have many features yet, but we'll have more soon!
"

let help = "
  Not much here yet, but:

    Type /leave to exit the game
    Type /help for this message
    Type /whisper and who you are whispering to to send a
    message to that person
    Type /look to see what is in the room
    Type /move to leave the room
    Type /take and the item you wish to take to collect it
    Type /drop to drop a thing in your inventory to leave it
    a room
    Type /hit to attack somebody
    Type /inventory to see what things you have
    And anything else you type is sent to everyone else who
    is logged in.

  Have fun!
"

  (*
   |  ██████   █████  ███    ███ ███████
   | ██       ██   ██ ████  ████ ██
   | ██   ███ ███████ ██ ████ ██ █████
   | ██    ██ ██   ██ ██  ██  ██ ██
   |  ██████  ██   ██ ██      ██ ███████
*)

(* Items *)

let pebble = { in_room = true;
               place = "Basic";
               description = "
a normal gray pebble";
               name = "pebble"}
let lantern = { in_room = true;
                place = "Welcome";
                description = "
an obsidian lantern with a small handel.
The flame is lit.";
                name = "obsidian lantern"}
let red_book = {in_room = true;
                place = "Library";
                description = "
a red book with a lether cover.
The name is Monsters Through the Ages";
                name = "monsters through the ages"}

let green_book = {in_room = true;
                  place = "Library";
                  description = "
a green, well worn book.
The name is Runes for Dummies.
There is a lock on the cover.";
                  name = "runes for dummies"}
let blue_book = {in_room = true;
                 place = "Library";
                 description = "
a blue flower on the cover.
It looks good as new.
The name is blurred but you can make out the title
Flowers of the World";
                 name = "flowers of the world"}
let hunting_knife = {in_room = true;
                     place = "Prisons";
                     description = "
a small curved hunting knife.
It has a lether handle and a carving of a flower on the blade.";
                     name = "hunting knife"}

(* Rooms *)

let welcome =
  { name = "Welcome"
  ; doors = [
      { direction = "north"; destination = "Basic" };
      { direction = "west"; destination = "Prisons"};
      { direction = "east"; destination = "Library" };
    ]
  ; description = "
You are in a room with cobblestone walls
A small obsidian lantern sits in the middle of the floor.
There is a door to the north, the east and the west.
"
  }
let statue_garden =
  { name = "Statue Garden";
    doors = [
      { direction = "north"; destination = "prisons"};
    ];
    description = "
You enter a very well lit room.
Stone walkways partition off the room into four parts.
In one part, there are five stone greek statues.
Another section has bronze statues.
The third has wooden abstract structures.
The final section has ceramic structures.
"
  }


let prisons =
  { name = "Prisons";
    doors = [
      { direction = "east"; destination = "Welcome"};
      { direction = "south"; destination = "Statue Garden"};
    ];
    description = "
You enter a cold dark room.
As you walk in torches along the walls light up.
You see four cells in the room, one in each corner.
Withen every cell there is a sleeping prisoner.
There is a table in the middle of the room with a small hunting knife.
"}

let library =
  { name = "Library"
  ; doors = [
      { direction = "west"; destination = "Welcome"};
    ]
  ; description = "
You enter a room with wooden bookshelfs around the room.
Books fill every shelf.
A small chair is stationed in the middle of the room.
It's legs are nailed down. Three books are in the middle of the room.
There is a door to the west.
"
  }

let basic =
  { name = "Basic"
  ; doors = [ { direction = "south"; destination = "Welcome" };]
  ; description = {|
This is the most boring room ever.
There is a door to the south.
|}
  }

let init =
  { people = []
  ; rooms = [ basic; welcome;library; prisons;statue_garden ]
  ; items = [ pebble;lantern; red_book; blue_book; green_book; hunting_knife]
  }

(*
   | ██   ██ ███████ ██      ██████  ███████ ██████  ███████
   | ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
   | ███████ █████   ██      ██████  █████   ██████  ███████
   | ██   ██ ██      ██      ██      ██      ██   ██      ██
   | ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████
*)

let get_person w nick =
  List.find w.people ~f:(fun p -> p.nick = nick)

let get_people_in_room w roomn =
  List.filter w.people ~f:(fun p -> p.roomn = roomn)

let get_room w nick =
  match get_person w nick with
  | None -> None
  | Some person ->
    let roomn = person.roomn in
    List.find w.rooms ~f:(fun r -> r.name = roomn)

let drop_nick nick people =
  List.filter people ~f:(fun p -> not (p.nick = nick))

let other_people w nick =
  drop_nick nick w.people

let other_people_in_room w nick roomn =
  drop_nick nick (get_people_in_room w roomn)

let items_in_room w roomn =
  List.filter w.items ~f:(fun i ->i.in_room = true && i.place = roomn)

let drop_item name (items:item list) =
  List.filter items ~f:(fun i -> not (i.name = name))

let replace_person w person =
  let people = List.map w.people ~f:(fun p -> (if p.nick = person.nick then person else p)) in
  {w with people = people}
(*
   |  █████   ██████ ████████ ██  ██████  ███    ██ ███████
   | ██   ██ ██         ██    ██ ██    ██ ████   ██ ██
   | ███████ ██         ██    ██ ██    ██ ██ ██  ██ ███████
   | ██   ██ ██         ██    ██ ██    ██ ██  ██ ██      ██
   | ██   ██  ██████    ██    ██  ██████  ██   ████ ███████
*)


let inventory w nick =
  let things_i_have =
    List.filter w.items ~f:(fun i -> not (i.in_room) && (i.place = nick)) in
  let thing_names =  List.map things_i_have ~f:(fun i-> i.name) in
  let inv_string = String.concat ~sep:" \n " thing_names  in
  "INVENTORY \n" ^ inv_string
;;

let looking w nick =
  match get_room w nick with
  | None -> assert false
  | Some room ->
    let roomn =  room.name in
    let people_in_room = drop_nick nick (get_people_in_room w roomn) in
    let people_description =
      match people_in_room with
      | [] -> [ "Nobody is in the room." ]
      | _ ->
        let names_in_room = List.map people_in_room ~f:(fun p -> p.nick) in
        [ "You see"
        ; String.concat ~sep:" and " names_in_room ^ "."
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
        ; String.concat ~sep:" and " descriptions ^ "."
        ]
    in
    room.description ^ String.concat ~sep:" " (people_description @ item_description)

let taking w nick item_name =
  match get_person w nick with
  | None -> assert false
  | Some me ->
    let items_in_room =
      items_in_room w me.roomn
    in
    let item = List.find items_in_room ~f:(fun i -> item_name = i.name) in
    match item with
    | None ->
      let message = "Sorry " ^ nick ^ ". I don't see a " ^ item_name in
      (w, Send_message {nick; message} :: [])
    | Some item ->
      let new_items =
        { item with
          in_room = false
        ; place = nick
        } :: drop_item item_name w.items
      in
      let nw = { w with items = new_items } in
      (nw, Send_message{nick; message = "Okay. You now have the " ^ item_name} :: [])
;;

let drop w nick item_name =
  match get_room w nick with
  | None -> assert false
  | Some room ->
    let items = List.map w.items ~f:(fun i ->
      if i.name = item_name
      && not i.in_room
      && i.place = nick
      then { i with in_room = true; place = room.name }
      else i)
    in
    { w with items }
;;

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

let check_health w nick =
  match get_person w nick with
  | None -> assert false
  | Some me ->
    (w, [Send_message {nick; message = Int.to_string me.health}])

let action w nick name =
  match get_room w nick with
  | None -> assert false
  | Some room ->
    match name with
    | "sing" ->
      if room.name = "Prisons" then
        let actions = [Send_message {nick; message = "
The prisoners look up as you sing, and you experience a brief
moment of solidarity."}]
        in
        (w,actions)
      else
        let actions = [Send_message {nick; message = "You sing a lovely song, but no one notices."}] in
        (w,actions)
    | _ ->
      let actions = [ Send_message {nick; message = "I don't know how to "^name}] in
      (w,actions)
;;

let hit w nick vic =
  match (get_person w nick, get_person w vic) with
  | None, _ -> assert false
  | Some _, None -> (w, [Send_message {nick; message = "Ha! " ^ vic ^ " isn't a player!"}])
  | Some attacker, Some attacked ->
    if attacker.roomn = attacked.roomn
    then
      let attacked = { attacked with health = attacked.health - 1 } in
      if attacked.health = 0
      then
        let items = List.map w.items ~f:(fun i ->
          if i.in_room = false && i.place = nick
          then {i with in_room = false; place = attacker.roomn}
          else i )
        in
        let nw = {w with items} in
        if  vic = nick
        then
          let message = "You kill yourself. You think 'Why the hell did I do that?'" in
          (nw, [Send_message {nick; message}; Kick {nick}])
        else
          (nw,
           [Send_message {nick = vic; message = nick ^ " killed you."};
            Send_message {nick; message = "You killed " ^vic } ;
            Kick {nick = vic} ] )
      else if vic = nick
      then
        (replace_person w attacked,
         [Send_message
            {nick; message =
                     "You sock yourself in the face.
   You think 'That hurt. Why did I do that?'"}])
      else
        (replace_person w attacked,
         [Send_message
            {nick = vic; message =
                           "You've been hit by " ^ nick ^ "!"};
          Send_message
            {nick; message =
                     "You hit " ^ vic ^ "."}
         ])
    else
      (replace_person w attacked,
       [Send_message
          {nick; message =
                   "Sorry! " ^ vic ^ " isn't in this room!"}])

let nick_added w nick =
  match List.find w.people ~f:(fun p -> p.nick = nick) with
  | None ->
    let person = { nick; roomn = "Welcome" ; health = 20 } in
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
  | Some p ->
    let person = {p with health = 20 ; roomn = "Welcome"} in
    let nw = { w with people = person :: other_people w nick } in
    let welcome_message =
      Send_message { nick; message = String.concat ["Welcome back to the MUD, "; nick; "!"] }
    in
    let hello_messages =
      List.map (other_people w nick) ~f:(fun p ->
        Send_message {nick = p.nick; message = nick ^ " has returned!"})
    in
    let actions = welcome_message :: hello_messages in
    (nw,actions)

let nick_removed w nick =
  let me = Option.value_exn ( get_person w nick)  in
  let new_me =  { me with roomn = "Welcome"} in
  let nw = replace_person w new_me in
  let goodbye_message = nick ^ " vanished in a puff of smoke." in
  let actions = List.map (other_people w nick) ~f:(fun p ->
    Send_message { nick = p.nick; message = goodbye_message })
  in
  (nw,actions)

let handle_line w nick line =
  match String.split ~on:' ' line with
  | "" :: [] | [] -> (w,[])
  | "/leave" :: [] -> (w, [Kick { nick }])
  | "/help"  :: [] -> (w, [Send_message { nick;message = help }])
  | "/look"  :: [] -> (w, [Send_message { nick; message = looking w nick }])
  | "/move"  :: dir :: [] -> moving w nick dir
  | "/take" :: name -> taking w nick (String.concat ~sep:" " name)
  | "/hit" :: name :: [] -> hit w nick name
  | "/health" ::[] -> check_health w nick
  | "/action" :: name -> action w nick (String.concat ~sep:" " name)
  | "/drop" :: name :: [] ->
    let nw = drop w nick name in
    let message =
      if nw = w then "Sorry, you don't have one of those"
      else "Okay, you have dropped your " ^ name ^ "."
    in
    let actions = [ Send_message { nick; message }] in
    (nw,actions)
  | "/inventory" :: [] -> (w, [Send_message {nick; message = inventory w nick}])
  | "/whisper" :: to_nick :: message ->
    (w, [Send_message { nick = to_nick
                      ; message = nick ^ " whispered: " 
                                  ^ String.concat ~sep:" " message }])
  | _ ->
    (* just show what was said to everyone in the same room *)
    let hearers =
      match get_person w nick with
      | None -> assert false
      | Some lalaland ->
        drop_nick nick (get_people_in_room w lalaland.roomn)
    in
    let actions =
      let message = nick ^ ": " ^ line in
      List.map hearers ~f:(fun p -> Send_message { nick = p.nick; message })
    in
    (w,actions)

let () =
  printf "Starting mud!\n%!";
  start_mud
    { init
    ; description
    ; handle_line
    ; nick_added
    ; nick_removed
    ; sexp_of_world
    }
