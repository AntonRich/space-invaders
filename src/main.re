type screenSize = { x: int, y: int };
type element;
type context;

[@bs.get] external elementWidth: element => int = "width";
[@bs.get] external elementHeight: element => int = "height";
[@bs.val] external getElementById: string => element = "document.getElementById";
[@bs.send] external getContext2d : (element, [@bs.as "2d"] _) => context = "getContext";

let canvas = getElementById("screen");

let screen = getContext2d(canvas);

let getScreenSize = (canvas) => { x: elementWidth(canvas), y: elementHeight(canvas) };

Js.log(canvas);
Js.log(getScreenSize(canvas))