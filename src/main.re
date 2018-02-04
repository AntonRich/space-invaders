type element;

type context;

[@bs.get] external elementWidth : element => int = "width";

[@bs.get] external elementHeight : element => int = "height";

[@bs.val]
external getElementById : string => element = "document.getElementById";

[@bs.send]
external getContext2d : (element, [@bs.as "2d"] _) => context = "getContext";

type size = {
  width: int,
  height: int
};

let canvas = getElementById("screen");

let screen = getContext2d(canvas);

let getScreenSize = canvas => {
  width: elementWidth(canvas),
  height: elementHeight(canvas)
};

type position = {
  x: int,
  y: int
};

type player = {
  size,
  center: position
};

type invader = {
  size,
  center: position,
  patrolX: int,
  speedX: float
};

type bullet = {
  size,
  center: position,
  velocity: position
};

type body =
  | Player(player)
  | Invader(invader)
  | Bullet(bullet);

type gameState = {
  bodies: list(body),
  playerAlive: bool,
  invadersLeft: bool
};

let update = body =>
  switch body {
  | Player(_) => body
  | Invader(_) => body
  | Bullet(_) => body
  };