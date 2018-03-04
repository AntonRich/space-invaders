open Dom;

open Logic;

open Drawer;

let makeGameBoard = ({width, height}) => {
  let playerSize = {width: 18, height: 8};
  let playerPosition: position = {x: width / 2, y: height - 10};
  let invaderSize = {width: width / 10, height: height / 30};
  let invaderSpeed = {x: 0, y: 0};
  let invaderXOffset = invaderSize.width / 2 + invaderSize.width / 10;
  let invaderXSeparation = invaderSize.width + invaderSize.width / 10;
  {
    bodies: [
      Player(playerSize, playerPosition),
      Invader(
        invaderSize,
        {x: invaderXOffset + invaderXSeparation * 0, y: 10},
        invaderSpeed
      ),
      Invader(
        invaderSize,
        {x: invaderXOffset + invaderXSeparation * 1, y: 10},
        invaderSpeed
      ),
      Invader(
        invaderSize,
        {x: invaderXOffset + invaderXSeparation * 2, y: 10},
        invaderSpeed
      ),
      Invader(
        invaderSize,
        {x: invaderXOffset + invaderXSeparation * 3, y: 10},
        invaderSpeed
      ),
      Invader(
        invaderSize,
        {x: invaderXOffset + invaderXSeparation * 4, y: 10},
        invaderSpeed
      ),
      Invader(
        invaderSize,
        {x: invaderXOffset + invaderXSeparation * 5, y: 10},
        invaderSpeed
      ),
      Invader(
        invaderSize,
        {x: invaderXOffset + invaderXSeparation * 6, y: 10},
        invaderSpeed
      ),
      Invader(
        invaderSize,
        {x: invaderXOffset + invaderXSeparation * 7, y: 10},
        invaderSpeed
      ),
      Invader(
        invaderSize,
        {x: invaderXOffset + invaderXSeparation * 8, y: 10},
        invaderSpeed
      )
    ],
    size: {
      width,
      height
    }
  };
};

let gameKeyboard = {left: false, right: false, space: false};

type key =
  | Left
  | Right
  | Space
  | UnusedKey;

let keyFromEvent = e => {
  let keyCode = getKeyCode(e);
  switch keyCode {
  | 37 => Left
  | 39 => Right
  | 32 => Space
  | _ => UnusedKey
  };
};

addEventListener(
  document,
  "keydown",
  e => {
    let key = keyFromEvent(e);
    switch key {
    | Left => gameKeyboard.left = true
    | Right => gameKeyboard.right = true
    | Space => gameKeyboard.space = true
    | _ => ()
    };
  }
);

addEventListener(
  document,
  "keyup",
  e => {
    let key = keyFromEvent(e);
    switch key {
    | Left => gameKeyboard.left = false
    | Right => gameKeyboard.right = false
    | Space => gameKeyboard.space = false
    | _ => ()
    };
  }
);

let rec gameLoop = (state, keyboard, canvas, frameId) => {
  let nextState = tick(state, keyboard);
  draw(nextState, canvas);
  requestAnimationFrame(gameLoop(nextState, keyboard, canvas));
};

let canvas = getElementById("screen");

let screen = getContext2d(canvas);

let startButton = getElementById("start");

let initialBoard =
  makeGameBoard({width: elementWidth(canvas), height: elementHeight(canvas)});

let startGame = () => {
  gameLoop(initialBoard, gameKeyboard, canvas, 0);
  ();
};

addEventListener(startButton, "click", e => startGame());