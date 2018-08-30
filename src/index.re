open Reprocessing;

type stateT = {
  playerY: float,
  playerVY: float,
  debris: list((float, float)),
  offsetX: float
};

let playerX = 50.;
let playerHeight = 30.;
let gravity = 400.;
let floorY = 200.;
let speed = 100.;

let setup = env => {
  Env.size(~width=800, ~height=300, env);
  {
    playerY: floorY,
    playerVY: 0.,
    debris: [(200., 50.), (400., 30.), (600., 40.)],
    offsetX: 0.
  };
};

let draw = ({playerY, playerVY, debris, offsetX} as state, env) => {
  /* Background */
  Draw.background(Utils.color(~r=199, ~g=217, ~b=229, ~a=255), env);

  /* Floor */
  Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=30), env);
  Draw.rectf(
    ~pos=(0., floorY +. playerHeight -. 20.),
    ~width=float_of_int(Env.width(env)),
    ~height=float_of_int(Env.height(env)) -. floorY,
    env
  );

  /* Player */
  Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=255), env);
  Draw.rectf(
    ~pos=(playerX, playerY),
    ~width=playerHeight,
    ~height=playerHeight,
    env
  );

  /* Debris */
  Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=255), env);
  List.iter(
    ((x, height)) =>
      Draw.rectf(
        ~pos=(x -. offsetX, floorY -. height +. playerHeight),
        ~width=20.,
        ~height,
        env
      ),
    debris
  );

  let deltaTime = Env.deltaTime(env);

  {
    ...state,
    playerY: min(playerY +. playerVY *. 2. *. deltaTime, floorY),
    playerVY:
      if (Env.keyPressed(Space, env) && playerY === floorY) {
        (-200.);
      } else {
        playerVY +. gravity *. deltaTime;
      },
    offsetX: offsetX +. speed *. deltaTime
  };
};

run(~setup, ~draw, ());