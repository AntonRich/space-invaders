open Dom;

open Logic;

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
  clearRect(screen, 0, 0, elementWidth(canvas), elementHeight(canvas));
  List.map(drawBody(screen), game.bodies);
};