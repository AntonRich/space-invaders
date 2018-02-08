type element;

type context;

[@bs.get] external elementWidth : context => int = "width";

[@bs.get] external elementHeight : context => int = "height";

[@bs.val]
external getElementById : string => element = "document.getElementById";

[@bs.send]
external getContext2d : (element, [@bs.as "2d"] _) => context = "getContext";

[@bs.send]
external fillRect : (context, int, int, int, int) => unit = "fillRect";

[@bs.send]
external clearRect : (context, int, int, int, int) => unit = "clearRect";

[@bs.val]
external requestCancellableAnimationFrame : (unit => unit) => int =
  "requestAnimationFrame";

[@bs.val] external cancelAnimationFrame : int => unit = "";

type size = {
  width: int,
  height: int
};

let canvas = getElementById("screen");

let screen = getContext2d(canvas);

let getScreenSize = context => {
  width: elementWidth(context),
  height: elementHeight(context)
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

let tick = game => {
  bodies: List.map(update, game.bodies),
  playerAlive: true,
  invadersLeft: true
};

let drawBody = (screen, body) =>
  switch body {
  | Player({center, size}) =>
    fillRect(
      screen,
      center.x - size.width / 2,
      center.y - size.height / 2,
      size.width,
      size.height
    )
  | Invader({center, size}) =>
    fillRect(
      screen,
      center.x - size.width / 2,
      center.y - size.height / 2,
      size.width,
      size.height
    )
  | Bullet({center, size}) =>
    fillRect(
      screen,
      center.x - size.width / 2,
      center.y - size.height / 2,
      size.width,
      size.height
    )
  };

let draw = (game, screen) => {
  let screenSize = getScreenSize(screen);
  clearRect(screen, 0, 0, screenSize.width, screenSize.height);
  List.map(drawBody(screen), game.bodies);
};

/* let initialState = {
     bodies: [{
                size: {
                  width: 12,
                  height: 6
                },
                center: {
                  x: 30,
                  y: 30
                }
              }],
     playerAlive: true,
     invadersLeft: true
   }; */
let gameLoop = () => {};