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

let isInvader = body =>
  switch body {
  | Invader(_, _, _) => true
  | _ => false
  };

let findPlayer = bodies => List.find(isPlayer, bodies);

let findInvaders = bodies => List.find_all(isInvader, bodies);

let getPosition = body =>
  switch body {
  | Player(_, position) => position
  | Invader(_, position, _) => position
  | Bullet(_, position, _) => position
  };

let getSize = body =>
  switch body {
  | Player(size, _) => size
  | Invader(size, _, _) => size
  | Bullet(size, _, _) => size
  };

let colliding = (body1, body2) => {
  let size1 = getSize(body1);
  let size2 = getSize(body2);
  let position1 = getPosition(body1);
  let position2 = getPosition(body2);
  ! (
    body1 === body2
    || position1.x
    + size1.width
    / 2 < position2.x
    - size2.width
    / 2
    || position1.y
    + size1.height
    / 2 < position2.y
    - size2.height
    / 2
    || position1.x
    - size1.width
    / 2 > position2.x
    + size2.width
    / 2
    || position1.y
    - size1.height
    / 2 > position2.y
    + size2.height
    / 2
  );
};

let notCollidingWithAny = (bodies, body) =>
  List.length(List.filter(b => colliding(b, body), bodies)) == 0;

let invaderShot = invader => {
  let invaderPosition = getPosition(invader);
  let invaderSize = getSize(invader);
  if (Js.Math.random() > 0.995) {
    [
      Bullet(
        {width: 3, height: 3},
        {x: invaderPosition.x, y: invaderPosition.y + invaderSize.height / 2},
        {x: 0, y: 2}
      )
    ];
  } else {
    [];
  };
};

let tick = (game, keyboard) => {
  let player = findPlayer(game.bodies);
  let playerPosition = getPosition(player);
  let invaders = findInvaders(game.bodies);
  let survivingBodies =
    List.filter(notCollidingWithAny(game.bodies), game.bodies);
  let playerBullets =
    if (keyboard.space) {
      [Bullet({width: 3, height: 3}, playerPosition, {x: 0, y: (-6)})];
    } else {
      [];
    };
  let invaderBullets = List.flatten(List.map(invaderShot, invaders));
  let newBullets = List.append(playerBullets, invaderBullets);
  let allBodies = List.append(survivingBodies, newBullets);
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
    Invader({width: 30, height: 10}, {x: 10, y: 20}, {x: 0, y: 0}),
    Invader({width: 30, height: 10}, {x: 45, y: 20}, {x: 0, y: 0}),
    Invader({width: 30, height: 10}, {x: 80, y: 20}, {x: 0, y: 0}),
    Invader({width: 30, height: 10}, {x: 115, y: 20}, {x: 0, y: 0}),
    Invader({width: 30, height: 10}, {x: 150, y: 20}, {x: 0, y: 0}),
    Invader({width: 30, height: 10}, {x: 185, y: 20}, {x: 0, y: 0}),
    Invader({width: 30, height: 10}, {x: 220, y: 20}, {x: 0, y: 0}),
    Invader({width: 30, height: 10}, {x: 255, y: 20}, {x: 0, y: 0}),
    Invader({width: 30, height: 10}, {x: 290, y: 20}, {x: 0, y: 0})
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