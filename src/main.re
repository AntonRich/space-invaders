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

type gameBoard = {
  player,
  invaders: list(invader),
  bullets: list(bullet)
};

let updatePlayer = (keyboard, {size, center}: player) => {
  size,
  center: {
    x: center.x + (keyboard.left ? (-2) : 0) + (keyboard.right ? 2 : 0),
    y: center.y
  }
};

let updateInvaders = invaders => invaders;

let updateBullet = ({size, velocity, center}: bullet) : bullet => {
  size,
  velocity,
  center: {
    x: center.x,
    y: center.y + velocity.y
  }
};

let updateBullets = (keyboard, bullets, origin) => {
  let updatedBullets = List.map(updateBullet, bullets);
  if (keyboard.space) {
    let bullet = {
      size: {
        width: 3,
        height: 3
      },
      velocity: {
        x: 0,
        y: (-6)
      },
      center: {
        x: origin.x,
        y: origin.y
      }
    };
    List.append([bullet], updatedBullets);
  } else {
    updatedBullets;
  };
};

let tick = (game, keyboard) => {
  let player = game.player;
  {
    player: updatePlayer(keyboard, player),
    invaders: updateInvaders(game.invaders),
    bullets: updateBullets(keyboard, game.bullets, player.center)
  };
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
  drawBody(screen, Player(game.player));
  List.map(drawBody(screen), List.map(b => Bullet(b), game.bullets));
  List.map(drawBody(screen), List.map(b => Invader(b), game.invaders));
};

let initialState = {
  player: {
    size: {
      width: 18,
      height: 8
    },
    center: {
      x: 120,
      y: 300
    }
  },
  invaders: [
    {
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
    }
  ],
  bullets: []
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
    | _ => ignore()
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
    | _ => ignore()
    };
  }
);

let rec gameLoop = (state, keyboard, frameId) => {
  let nextState = tick(state, keyboard);
  draw(nextState, canvas);
  requestAnimationFrame(gameLoop(nextState, keyboard));
};

gameLoop(initialState, gameKeyboard, 0);