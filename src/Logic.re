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

type gameBoard = {
  bodies: list(body),
  size
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

let insideGameBoard = (boardSize, body) => {
  let bodyPosition = getPosition(body);
  let bodySize = getSize(body);
  boardSize.width > bodyPosition.x
  + bodySize.width
  / 2
  && bodyPosition.x
  - bodySize.width
  / 2 > 0
  && boardSize.height > bodyPosition.y
  + bodySize.height
  / 2
  && bodyPosition.y
  - bodySize.height
  / 2 > 0;
};

let inRightBoardEdge = (boardSize, body) => {
  let bodyPosition = getPosition(body);
  let bodySize = getSize(body);
  boardSize.width == bodyPosition.x
  + bodySize.width
  / 2
  || boardSize.width
  + 1 == bodyPosition.x
  + bodySize.width
  / 2;
};

let inLeftBoardEdge = (boardSize, body) => {
  let bodyPosition = getPosition(body);
  let bodySize = getSize(body);
  bodyPosition.x
  - bodySize.width
  / 2 == 0
  || bodyPosition.x
  - bodySize.width
  / 2 == (-1);
};

let updateBody = (keyboard, boardSize, body) =>
  switch body {
  | Player(size, position) when insideGameBoard(boardSize, body) =>
    Player(
      size,
      {
        x: position.x + (keyboard.left ? (-2) : 0) + (keyboard.right ? 2 : 0),
        y: position.y
      }
    )
  | Player(size, position) when inRightBoardEdge(boardSize, body) =>
    Player(size, {x: position.x + (keyboard.left ? (-2) : 0), y: position.y})
  | Player(size, position) when inLeftBoardEdge(boardSize, body) =>
    Player(size, {x: position.x + (keyboard.right ? 2 : 0), y: position.y})
  | Player(_, _) => body
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

let notBulletAndInsideGameBoard = (boardSize, body) => {
  let bodyPosition = getPosition(body);
  isPlayer(body)
  || isInvader(body)
  || boardSize.width >= bodyPosition.x
  && insideGameBoard(boardSize, body);
};

let tick = (game, keyboard) => {
  let player = findPlayer(game.bodies);
  let playerPosition = getPosition(player);
  let invaders = findInvaders(game.bodies);
  let survivingBodies =
    game.bodies
    |> List.filter(notCollidingWithAny(game.bodies))
    |> List.filter(notBulletAndInsideGameBoard(game.size));
  let playerBullets =
    if (keyboard.space) {
      [Bullet({width: 3, height: 3}, playerPosition, {x: 0, y: (-6)})];
    } else {
      [];
    };
  let invaderBullets = List.flatten(List.map(invaderShot, invaders));
  let newBullets = List.append(playerBullets, invaderBullets);
  let allBodies = List.append(survivingBodies, newBullets);
  {
    bodies: List.map(updateBody(keyboard, game.size), allBodies),
    size: game.size
  };
};