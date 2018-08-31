open Reprocessing;

type runningT =
  | Running
  | Restart;

type stateT = {
  playerY: float,
  playerVY: float,
  debris: list((float, float)),
  offsetX: float,
  running: runningT,
};

let playerX = 50.;
let playerHeight = 30.;
let gravity = 400.;
let floorY = 200.;
let speed = 100.;
let debrisWidth = 20.;

let setup = env => {
  Env.size(~width=800, ~height=300, env);
  {
    playerY: floorY,
    playerVY: 0.,
    debris: [(200., 50.), (400., 30.), (600., 40.)],
    offsetX: 0.,
    running: Running,
  };
};

let draw = ({playerY, playerVY, debris, offsetX, running} as state, env) => {
  /* Background */
  Draw.background(Utils.color(~r=199, ~g=217, ~b=229, ~a=255), env);

  /* Floor */
  Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=30), env);
  Draw.rectf(
    ~pos=(0., floorY +. playerHeight -. 20.),
    ~width=float_of_int(Env.width(env)),
    ~height=float_of_int(Env.height(env)) -. floorY,
    env,
  );

  /* Debris */
  Draw.fill(Utils.color(~r=80, ~g=80, ~b=80, ~a=255), env);
  List.iter(
    ((x, height)) =>
      Draw.rectf(
        ~pos=(x -. offsetX, floorY -. height +. playerHeight),
        ~width=debrisWidth,
        ~height,
        env,
      ),
    debris,
  );

  /* Player */
  Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=255), env);
  Draw.rectf(
    ~pos=(playerX, playerY),
    ~width=playerHeight,
    ~height=playerHeight,
    env,
  );

  /* Collision detection */
  let collided =
    List.exists(
      ((x, height)) =>
        Utils.intersectRectRect(
          ~rect1Pos=(x -. offsetX, floorY -. height +. playerHeight),
          ~rect1W=debrisWidth,
          ~rect1H=height,
          ~rect2Pos=(playerX, playerY),
          ~rect2W=playerHeight,
          ~rect2H=playerHeight,
        ),
      debris,
    );

  let deltaTime = Env.deltaTime(env);

  switch (running) {
  | Running => {
      ...state,
      playerY: min(playerY +. playerVY *. 2. *. deltaTime, floorY),
      playerVY:
        Env.keyPressed(Space, env) && playerY === floorY ?
          (-200.) : playerVY +. gravity *. deltaTime,
      offsetX: offsetX +. speed *. deltaTime,
      running: collided ? Restart : Running,
    }
  | Restart =>
    if (Env.keyPressed(Space, env)) {
      {
        ...state,
        playerY: floorY,
        playerVY: 0.,
        offsetX: 0.,
        running: Running,
      };
    } else {
      state;
    }
  };
};

run(~setup, ~draw, ());