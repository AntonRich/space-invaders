open Webapi.Dom;
open Webapi.Canvas;

let canvas = Document.getElementById("screen", document);

let screen =
  switch (canvas) {
  | Some(canvas) => CanvasElement.getContext2d(canvas)
  | None => raise(Not_found);
  };

  Js.log(screen)