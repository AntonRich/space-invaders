type element;

type context;

type event;

type window;

[@bs.get] external elementWidth : element => int = "width";

[@bs.get] external elementHeight : element => int = "height";

[@bs.val]
external getElementById : string => element = "document.getElementById";

[@bs.val] external document : element = "document";

[@bs.send]
external getContext2d : (element, [@bs.as "2d"] _) => context = "getContext";

[@bs.send]
external fillRect : (context, int, int, int, int) => unit = "fillRect";

[@bs.send]
external clearRect : (context, int, int, int, int) => unit = "clearRect";

[@bs.val]
external requestAnimationFrame : (int => int) => int = "requestAnimationFrame";

[@bs.val] external cancelAnimationFrame : int => unit = "cancelAnimationFrame";

[@bs.send]
external addEventListener : (element, string, event => unit) => unit =
  "addEventListener";

[@bs.get] external getKeyCode : event => int = "keyCode";

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

type keyboard = {
  mutable left: bool,
  mutable right: bool,
  mutable space: bool
};

type gameState = {
  bodies: list(body),
  playerAlive: bool,
  invadersLeft: bool
};

let update = (keyboard, body) =>
  switch body {
  | Player({center, size}) =>
    Player({
      size,
      center: {
        x: center.x + (keyboard.left ? (-2) : 0) + (keyboard.right ? 2 : 0),
        y: center.y
      }
    })
  | Invader(_) => body
  | Bullet(_) => body
  };

let tick = (game, keyboard) => {
  bodies: List.map(update(keyboard), game.bodies),
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

let draw = (game, canvas) => {
  let screen = getContext2d(canvas);
  let screenSize = getScreenSize(canvas);
  clearRect(screen, 0, 0, screenSize.width, screenSize.height);
  List.map(drawBody(screen), game.bodies);
};

let initialState = {
  bodies: [
    Player({
      size: {
        width: 12,
        height: 6
      },
      center: {
        x: 120,
        y: 300
      }
    }),
    Invader({
      size: {
        width: 30,
        height: 10
      },
      center: {
        x: 10,
        y: 90
      },
      patrolX: 3,
      speedX: 3.
    })
  ],
  playerAlive: true,
  invadersLeft: true
};

let gameKeyboard = {left: false, right: false, space: false};

addEventListener(
  document,
  "keydown",
  e => {
    let keyCode = getKeyCode(e);
    switch keyCode {
    | 37 => gameKeyboard.left = true
    | 39 => gameKeyboard.right = true
    | 32 => gameKeyboard.space = true
    | _ => Js.log("")
    };
  }
);

addEventListener(
  document,
  "keyup",
  e => {
    let keyCode = getKeyCode(e);
    switch keyCode {
    | 37 => gameKeyboard.left = false
    | 39 => gameKeyboard.right = false
    | 32 => gameKeyboard.space = false
    | _ => Js.log("")
    };
  }
);

let rec gameLoop = (state, keyboard, frameId) => {
  let nextState = tick(state, keyboard);
  draw(nextState, canvas);
  requestAnimationFrame(gameLoop(nextState, keyboard));
};

gameLoop(initialState, gameKeyboard, 0);