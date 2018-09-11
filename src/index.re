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
  sprite: imageT,
  score: int,
  speed: float,
};

type anatomyT = {
  width: float,
  height: float,
  pos: (float, float),
};

let playerX = 50.;
let playerHeight = 47.;
let playerWidth = 44.;
let gravity = 500.;
let floorY = 200.;
let debrisWidth = 17.;

/* Get all collision boxes/coordinates for the player */
let getPlayerCollisionBoxes = playerY => [
  {
    pos: (playerX +. 20., playerY),
    width: playerWidth -. 20.,
    height: playerHeight -. 27.,
  },
  {pos: (playerX +. 10., playerY +. 30.), width: 20., height: 17.},
  {pos: (playerX, playerY +. 15.), width: playerWidth -. 8., height: 15.},
];

let generateSingleDebris = x => (
  x +. Utils.randomf(~min=200., ~max=400.),
  Utils.randomf(~min=30., ~max=50.),
);

let generateInitialDebris = () => [
  generateSingleDebris(200.),
  generateSingleDebris(400.),
  generateSingleDebris(600.),
  generateSingleDebris(800.),
];

let setup = env => {
  Env.size(~width=800, ~height=300, env);
  {
    playerY: floorY,
    playerVY: 0.,
    debris: generateInitialDebris(),
    offsetX: 0.,
    running: Running,
    sprite: Draw.loadImage(~filename="assets/sprite.png", ~isPixel=true, env),
    score: 0,
    speed: 200.,
  };
};

let generateNewDebris = ({debris, offsetX, speed}) =>
  List.map(
    ((x, _) as d) =>
      if (x -. offsetX +. debrisWidth <= 0.) {
        let newX =
          List.fold_left(
            (maxX, (x, _)) => max(maxX, x +. speed /. 1000.),
            0.,
            debris,
          );
        generateSingleDebris(newX);
      } else {
        d;
      },
    debris,
  );

let draw =
    (
      {sprite, playerY, playerVY, debris, offsetX, running, score, speed} as state,
      env,
    ) => {
  /* Background */
  Draw.background(Utils.color(~r=246, ~g=246, ~b=246, ~a=255), env);

  /* Floor */
  Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=0), env);
  Draw.rectf(
    ~pos=(0., floorY +. playerHeight),
    ~width=float_of_int(Env.width(env)),
    ~height=float_of_int(Env.height(env)) -. floorY,
    env,
  );
  Draw.subImagef(
    sprite,
    ~pos=(0., floorY +. playerHeight -. 20.),
    ~width=2400. /. 2.,
    ~height=26. /. 2.,
    ~texPos=(2, 104),
    ~texWidth=2400,
    ~texHeight=26,
    env,
  );

  /* Debris */
  Draw.fill(Utils.color(~r=50, ~g=50, ~b=50, ~a=0), env);
  List.iter(
    ((x, height)) => {
      Draw.rectf(
        ~pos=(x -. offsetX, floorY -. height +. playerHeight),
        ~width=debrisWidth,
        ~height,
        env,
      );

      Draw.subImagef(
        sprite,
        ~pos=(x -. offsetX, floorY -. height +. playerHeight),
        ~width=debrisWidth,
        ~height,
        ~texPos=(446, 2),
        ~texWidth=34,
        ~texHeight=70,
        env,
      );
    },
    debris,
  );

  /* Player */
  let drawRunningPlayer = () =>
    switch (int_of_float(offsetX /. 15.) mod 2) {
    | 0 =>
      Draw.subImagef(
        sprite,
        ~pos=(playerX, playerY),
        ~width=playerWidth,
        ~height=playerHeight,
        ~texPos=(1678 + 88 * 3, 2),
        ~texWidth=88,
        ~texHeight=94,
        env,
      )
    | 1 =>
      Draw.subImagef(
        sprite,
        ~pos=(playerX, playerY),
        ~width=playerWidth,
        ~height=playerHeight,
        ~texPos=(1678 + 88 * 2, 2),
        ~texWidth=88,
        ~texHeight=94,
        env,
      )
    | _ => assert(false)
    };

  let drawIdlePlayer = () =>
    Draw.subImagef(
      sprite,
      ~pos=(playerX, playerY),
      ~width=playerWidth,
      ~height=playerHeight,
      ~texPos=(1678, 2),
      ~texWidth=88,
      ~texHeight=94,
      env,
    );

  let drawDeadPlayer = () =>
    Draw.subImagef(
      sprite,
      ~pos=(playerX, playerY),
      ~width=playerWidth,
      ~height=playerHeight,
      ~texPos=(1678 + 88 * 4, 2),
      ~texWidth=88,
      ~texHeight=94,
      env,
    );

  if (playerY === floorY && running === Running) {
    drawRunningPlayer();
  } else if (running === Restart) {
    drawDeadPlayer();
  } else {
    drawIdlePlayer();
  };

  /* 88x94 1678, y: 2*/

  /* Collision detection */
  let collided =
    List.exists(
      (anatomy: anatomyT) =>
        List.exists(
          ((x, height)) =>
            Utils.intersectRectRect(
              ~rect1Pos=(x -. offsetX, floorY -. height +. playerHeight),
              ~rect1W=debrisWidth,
              ~rect1H=height,
              ~rect2Pos=anatomy.pos,
              ~rect2W=anatomy.width,
              ~rect2H=anatomy.height,
            ),
          debris,
        ),
      getPlayerCollisionBoxes(playerY),
    );

  /* Draw collision boxes */
  Draw.fill(Utils.color(~r=0, ~g=255, ~b=0, ~a=0), env);
  List.iter(
    (anatomy: anatomyT) =>
      Draw.rectf(
        ~pos=anatomy.pos,
        ~width=anatomy.width,
        ~height=anatomy.height,
        env,
      ),
    getPlayerCollisionBoxes(playerY),
  );

  let debris = generateNewDebris(state);
  let deltaTime = Env.deltaTime(env);

  Draw.text(~body=string_of_int(score), ~pos=(5, 5), env);

  switch (running) {
  | Running => {
      ...state,
      debris,
      playerY: min(playerY +. playerVY *. 3. *. deltaTime, floorY),
      playerVY:
        Env.keyPressed(Space, env) && playerY === floorY ?
          (-200.) : playerVY +. gravity *. deltaTime,
      offsetX: offsetX +. speed *. deltaTime,
      running: collided ? Restart : Running,
      score: score + 1,
    }
  | Restart =>
    if (Env.keyPressed(Space, env)) {
      {
        ...state,
        debris: generateInitialDebris(),
        playerY: floorY,
        playerVY: 0.,
        offsetX: 0.,
        running: Running,
        score: 0,
      };
    } else {
      state;
    }
  };
};

run(~setup, ~draw, ());