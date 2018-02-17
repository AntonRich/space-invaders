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

type position = {
  x: int,
  y: int
};

type velocity = {
  x: int,
  y: int
};

type body =
  | Player(size, position)
  | Invader(size, position, velocity)
  | Bullet(size, position, velocity);

type keyboard = {
  mutable left: bool,
  mutable right: bool,
  mutable space: bool
};

type gameBoard = {bodies: list(body)};

let canvas = getElementById("screen");

let screen = getContext2d(canvas);

let getScreenSize = context => {
  width: elementWidth(context),
  height: elementHeight(context)
};

let updateBody = (keyboard, body) =>
  switch body {
  | Player(size, position) =>
    Player(
      size,
      {
        x: position.x + (keyboard.left ? (-2) : 0) + (keyboard.right ? 2 : 0),
        y: position.y
      }
    )
  | Invader(size, position, velocity) =>
    Invader(
      size,
      {x: position.x + velocity.x, y: position.y + velocity.y},
      velocity
    )
  | Bullet(size, position, velocity) =>
    Bullet(
      size,
      {x: position.x + velocity.x, y: position.y + velocity.y},
      velocity
    )
  };

let isPlayer = body =>
  switch body {
  | Player(_, _) => true
  | _ => false
  };

let findPlayer = bodies => List.find(isPlayer, bodies);

let getPosition = body =>
  switch body {
  | Player(_, position) => position
  | Invader(_, position, _) => position
  | Bullet(_, position, _) => position
  };

let tick = (game, keyboard) => {
  let player = findPlayer(game.bodies);
  let playerPosition = getPosition(player);
  let newBullets =
    if (keyboard.space) {
      [Bullet({width: 3, height: 3}, playerPosition, {x: 0, y: (-6)})];
    } else {
      [];
    };
  let allBodies = List.append(game.bodies, newBullets);
  {bodies: List.map(updateBody(keyboard), allBodies)};
};

let drawToScreen = (screen, size, position: position) =>
  fillRect(
    screen,
    position.x - size.width / 2,
    position.y - size.height / 2,
    size.width,
    size.height
  );

let drawBody = (screen, body) =>
  switch body {
  | Player(size, position) => drawToScreen(screen, size, position)
  | Invader(size, position, _) => drawToScreen(screen, size, position)
  | Bullet(size, position, _) => drawToScreen(screen, size, position)
  };

let draw = (game, canvas) => {
  let screen = getContext2d(canvas);
  let screenSize = getScreenSize(canvas);
  clearRect(screen, 0, 0, screenSize.width, screenSize.height);
  List.map(drawBody(screen), game.bodies);
};

let initialState = {
  bodies: [
    Player({width: 18, height: 8}, {x: 120, y: 300}),
    Invader({width: 30, height: 10}, {x: 10, y: 90}, {x: 0, y: 0})
  ]
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
    | _ => ()
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
    | _ => ()
    };
  }
);

let rec gameLoop = (state, keyboard, frameId) => {
  let nextState = tick(state, keyboard);
  draw(nextState, canvas);
  requestAnimationFrame(gameLoop(nextState, keyboard));
};

gameLoop(initialState, gameKeyboard, 0);