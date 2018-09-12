module MessageEvent = {
  type t;
  type data('a);

  include EventRe.Impl({
    type nonrec t = t;
  });

  [@bs.get] external data: t => data('a) = "data";
  [@bs.get] external type_: t => string = "type";
};

module CloseEvent = {
  type t;
  type code =
    | NormalClosue
    | GoingAway
    | ProtocolError
    | UnsupportedData
    | Empty_
    | NoStatus
    | AbnormalClosure
    | InvalidFramePayload
    | PolicyViolation
    | MessageTooBig
    | MissingExtension
    | InternalError
    | ServiceRestart
    | TryAgainLater
    | BadGateway
    | TLSHandshake
    | CustomCode(int, t);

  include EventRe.Impl({
    type nonrec t = t;
  });

  [@bs.get] external code: t => int = "code";
  [@bs.get] external reason: t => string = "reason";
  [@bs.get] external wasClean: t => bool = "wasClean";

  let code = ev =>
    switch (code(ev)) {
    | 1000 => NormalClosue
    | 1001 => GoingAway
    | 1002 => ProtocolError
    | 1003 => UnsupportedData
    | 1004 => Empty_
    | 1005 => NoStatus
    | 1006 => AbnormalClosure
    | 1007 => InvalidFramePayload
    | 1008 => PolicyViolation
    | 1009 => MessageTooBig
    | 1010 => MissingExtension
    | 1011 => InternalError
    | 1012 => ServiceRestart
    | 1013 => TryAgainLater
    | 1014 => BadGateway
    | 1015 => TLSHandshake
    | n => CustomCode(n, ev)
    };
};

module Websocket = {
  exception UnknownReadyState(int);

  type readyState =
    | Connecting
    | Open
    | Closing
    | Closed;

  type binaryType =
    | Blob
    | ArrayBuffer;

  type protocols = array(string);

  type t('msg) = {
    .
    [@bs.set] "binaryType": string,
    [@bs.set] "onopen": MessageEvent.t => unit,
    [@bs.set] "onerror": MessageEvent.t => unit,
    [@bs.set] "onclose": CloseEvent.t => unit,
    [@bs.set] "onmessage": MessageEvent.t => unit,
    "bufferedAmount": int,
    "url": string,
    "protocol": string,
    "readyState": string,
    "extensions": string,
    [@bs.meth]
    "close": (Js.Undefined.t(string), Js.Undefined.t(string)) => unit,
    [@bs.meth] "send": 'msg => unit,
  };

  [@bs.new] external make_: (string, protocols) => t('msg) = "WebSocket";

  let readyState = ws =>
    switch (ws##readyState) {
    | 0 => Connecting
    | 1 => Open
    | 2 => Closing
    | 3 => Closed
    | n => raise(UnknownReadyState(n))
    };

  let make = (~protocols=[||], url) => make_(url, protocols);
  let close = (~code=Js.undefined, ~reason=Js.undefined, ws) =>
    ws##close(code, reason);
  let onOpen = (ws, handler) => ws##onopen #= handler;
  let onError = (ws, handler) => ws##onerror #= handler;
  let onClose = (ws, handler) => ws##onclose #= handler;
  let onMessage = (ws, handler) => ws##onmessage #= handler;
  let send = (ws, msg) => ws##send(msg);
  let setBinaryType = (ws, binaryType) =>
    switch (binaryType) {
    | Blob => ws##binary #= "blob"
    | ArrayBuffer => ws##binary #= "arraybuffer"
    };
};