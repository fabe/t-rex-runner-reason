open Reprocessing;
open WebsocketClient;

let ws =
  Websocket.make("ws://localhost:8080", ~protocols=[|"echo-protocol"|]);

Websocket.onOpen(ws, _ => Websocket.send(ws, "Go Time!"));

let restartTimeout = ref(0);
let remoteTimeout = ref(0);
Websocket.onMessage(
  ws,
  ev => {
    Js.log("happened");
    remoteTimeout := 20;
  },
);

Websocket.onError(
  ws,
  ev => {
    Js.log(ev);
    Websocket.close(ws);
  },
);

/* Websocket.onClose(ws, ev => Js.log(ev)); */

type runningT =
  | Running
  | Restart;

type debrisT =
  | Narrow
  | Wide;

type stateT = {
  playerY: float,
  playerVY: float,
  debris: list((float, float, debrisT)),
  offsetX: float,
  running: runningT,
  sprite: imageT,
  score: int,
  speed: float,
  floorTextureOffset: (float, float),
  clouds: list((float, float)),
  soundJump: soundT,
  soundLevel: soundT,
  soundCollission: soundT,
  remoteTimout: int,
};

type anatomyT = {
  width: float,
  height: float,
  pos: (float, float),
};

let playerX = 50.;
let playerHeight = 47.;
let playerWidth = 44.;
let gravity = 600.;
let floorY = 200.;
let debrisWidth = 17.;
let debrisWidthWide = 51.;
let floorTextureWidth = 2400.;

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
  Utils.random(~min=0, ~max=2) === 1 ? Narrow : Wide,
);

let generateInitialDebris = () => [
  generateSingleDebris(200.),
  generateSingleDebris(600.),
  generateSingleDebris(1100.),
  generateSingleDebris(1600.),
];

let generateNewDebris = ({debris, offsetX, speed}) =>
  List.map(
    ((x, _, width) as d) =>
      if (x
          -. offsetX
          +. (width === Narrow ? debrisWidth : debrisWidthWide) <= 0.) {
        let newX =
          List.fold_left(
            (maxX, (x, _, _)) => max(maxX, x +. speed /. 4.),
            0.,
            debris,
          );
        generateSingleDebris(newX);
      } else {
        d;
      },
    debris,
  );

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
    speed: 300.,
    floorTextureOffset: (0., 0.),
    clouds: [(500., 20.), (600., 30.), (800., 30.)],
    soundJump: Env.loadSound("assets/jump.wav", env),
    soundLevel: Env.loadSound("assets/level-up.wav", env),
    soundCollission: Env.loadSound("assets/collission.wav", env),
    remoteTimout: 20,
  };
};

let generateSingleCloud = x => (
  x +. Utils.randomf(~min=200., ~max=800.),
  Utils.randomf(~min=80., ~max=140.),
);

let generateNewClouds = ({clouds, offsetX, speed}) =>
  List.map(
    ((x, _) as d) =>
      if (x -. offsetX /. 4. +. 92. <= 0.) {
        let newX =
          List.fold_left(
            (maxX, (x, _)) => max(maxX, x +. speed /. 10.),
            0.,
            clouds,
          );
        generateSingleCloud(newX);
      } else {
        d;
      },
    clouds,
  );

let draw =
    (
      {
        sprite,
        playerY,
        playerVY,
        debris,
        offsetX,
        running,
        score,
        speed,
        floorTextureOffset,
        clouds,
        soundJump,
        soundCollission,
        soundLevel,
        remoteTimout,
      } as state,
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

  let (floorTextureOffsetLeft, floorTextureOffsetRight) = floorTextureOffset;
  Draw.subImagef(
    sprite,
    ~pos=(0. -. floorTextureOffsetLeft, floorY +. playerHeight -. 15.),
    ~width=floorTextureWidth,
    ~height=26. /. 2.,
    ~texPos=(2, 104),
    ~texWidth=int_of_float(floorTextureWidth),
    ~texHeight=26,
    env,
  );
  Draw.subImagef(
    sprite,
    ~pos=(
      0. +. floorTextureWidth -. floorTextureOffsetRight,
      floorY +. playerHeight -. 15.,
    ),
    ~width=floorTextureWidth,
    ~height=26. /. 2.,
    ~texPos=(2, 104),
    ~texWidth=int_of_float(floorTextureWidth),
    ~texHeight=26,
    env,
  );

  /* Debris */
  Draw.fill(Utils.color(~r=50, ~g=50, ~b=50, ~a=0), env);
  List.iter(
    ((x, height, width)) =>
      switch (width) {
      | Narrow =>
        Draw.subImagef(
          sprite,
          ~pos=(x -. offsetX, floorY -. height +. playerHeight),
          ~width=debrisWidth,
          ~height,
          ~texPos=(446, 2),
          ~texWidth=34,
          ~texHeight=70,
          env,
        )
      | Wide =>
        Draw.subImagef(
          sprite,
          ~pos=(x -. offsetX, floorY -. height +. playerHeight),
          ~width=debrisWidthWide,
          ~height,
          ~texPos=(850, 2),
          ~texWidth=int_of_float(debrisWidthWide) * 2,
          ~texHeight=100,
          env,
        )
      },
    debris,
  );

  /* Scores */
  let drawDigit = (~digit, ~pos) =>
    Draw.subImage(
      sprite,
      ~pos,
      ~width=10,
      ~height=12,
      ~texPos=(1294 + digit * 20, 2),
      ~texWidth=20,
      ~texHeight=23,
      env,
    );

  let scoreString = ref(string_of_int(score));
  while (String.length(scoreString^) < 5) {
    scoreString := "0" ++ scoreString^;
    ();
  };

  String.iteri(
    (i, c) =>
      drawDigit(
        ~digit=int_of_string(String.make(1, c)),
        ~pos=(Env.width(env) - 5 * 11 + i * 12 - 20, 20),
      ),
    scoreString^,
  );

  /* Clouds */
  List.iter(
    ((x, y)) =>
      Draw.subImagef(
        sprite,
        ~pos=(x -. offsetX /. 4., y),
        ~width=92. /. 2.,
        ~height=27. /. 2.,
        ~texPos=(166, 2),
        ~texWidth=92,
        ~texHeight=27,
        env,
      ),
    clouds,
  );

  /* Player */
  let drawRunningPlayer = () =>
    switch (int_of_float(offsetX /. 30.) mod 2) {
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

  if (running === Restart) {
    Draw.subImagef(
      sprite,
      ~pos=(float_of_int(Env.width(env)) /. 2. -. 381. /. 4., 80.),
      ~width=381. /. 2.,
      ~height=21. /. 2.,
      ~texPos=(1294, 29),
      ~texWidth=381,
      ~texHeight=21,
      env,
    );

    Draw.subImagef(
      sprite,
      ~pos=(float_of_int(Env.width(env)) /. 2. -. 72. /. 4., 115.),
      ~width=72. /. 2.,
      ~height=64. /. 2.,
      ~texPos=(2, 2),
      ~texWidth=72,
      ~texHeight=64,
      env,
    );
  };

  /* Collision detection */
  let collided =
    List.exists(
      (anatomy: anatomyT) =>
        List.exists(
          ((x, height, width)) =>
            Utils.intersectRectRect(
              ~rect1Pos=(x -. offsetX, floorY -. height +. playerHeight),
              ~rect1W=
                switch (width) {
                | Narrow => debrisWidth
                | Wide => debrisWidthWide
                },
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

  /* Calculate new speed (level up) */
  let newSpeed =
    switch (score) {
    | score when score > 100 && score < 200 => 350.
    | score when score > 200 && score < 400 => 400.
    | score when score > 400 && score < 500 => 450.
    | score when score > 500 => 500.
    | _ => 300.
    };

  /* Play Sounds */
  if (running === Running && Env.key(Space, env) && playerY === floorY) {
    Env.playSound(soundJump, env);
  };

  if (collided && running === Running) {
    Env.playSound(soundCollission, env);
    restartTimeout := 50;
  };

  if (newSpeed > speed) {
    Env.playSound(soundLevel, env);
  };

  if (remoteTimeout^ > 0) {
    remoteTimeout := remoteTimeout^ - 1;
  };

  if (restartTimeout^ > 0) {
    restartTimeout := restartTimeout^ - 1;
  };

  switch (running) {
  | Running => {
      ...state,
      debris,
      playerY: min(playerY +. playerVY *. 3. *. deltaTime, floorY),
      playerVY:
        (Env.key(Space, env) || remoteTimeout^ > 0) && playerY === floorY ?
          (-200.) : playerVY +. gravity *. deltaTime,
      offsetX: offsetX +. speed *. deltaTime,
      running: collided ? Restart : Running,
      score: int_of_float(offsetX) / 25,
      floorTextureOffset: (
        int_of_float(floorTextureOffsetLeft)
        >= int_of_float(floorTextureWidth) ?
          floorTextureWidth *. (-1.) :
          floorTextureOffsetLeft +. speed *. deltaTime,
        int_of_float(floorTextureOffsetRight)
        >= int_of_float(floorTextureWidth)
        * 2 ?
          0. : floorTextureOffsetRight +. speed *. deltaTime,
      ),
      clouds: generateNewClouds(state),
      speed: newSpeed,
    }
  | Restart =>
    if ((Env.key(Space, env) || remoteTimeout^ > 0) && restartTimeout^ === 0) {
      {
        ...state,
        debris: generateInitialDebris(),
        playerY: floorY,
        playerVY: 0.,
        offsetX: 0.,
        running: Running,
        score: 0,
        floorTextureOffset: (0., 0.),
        clouds: [(200., 80.), (600., 100.)],
      };
    } else {
      state;
    }
  };
};

run(~setup, ~draw, ());