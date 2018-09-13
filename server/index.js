#!/usr/bin/env node
var WebSocketServer = require('websocket').server;
var http = require('http');
var qs = require('qs');
var admin = require('firebase-admin');

var serviceAccount = require('./runner-c06bf-firebase-adminsdk-t38iv-72a4616119.json');

admin.initializeApp({
  credential: admin.credential.cert(serviceAccount),
  databaseURL: 'https://runner-c06bf.firebaseio.com',
});

var server = http.createServer(function(request, response) {
  console.log(new Date() + ' Received request for ' + request.url);

  response.setHeader('Access-Control-Allow-Origin', '*');

  const url = request.url.split('?')[0];
  const params = qs.parse(request.url.split('?')[1]);
  console.log(url, params);

  switch (url) {
    case '/set':
      const data = {
        highscore: {
          name: params.name || 'Anon',
          score: params.score || 0,
        },
      };
      const setRes = admin
        .database()
        .ref('/')
        .set(data);

      setRes.then(() => {
        response.writeHead(201);
        response.write(data || '');
        response.end();
      });
      break;

    case '/get':
      const getRes = admin
        .database()
        .ref('/')
        .once('value');

      getRes.then(snapshot => {
        response.writeHead(201);
        response.write(
          `${snapshot.val().highscore.name},${snapshot.val().highscore.score}`
        );
        response.end();
      });

      break;

    default:
      response.writeHead(404);
      response.end();
      break;
  }
});
server.listen(8080, function() {
  console.log(new Date() + ' Server is listening on port 8080');
});

wsServer = new WebSocketServer({
  httpServer: server,
  // You should not use autoAcceptConnections for production
  // applications, as it defeats all standard cross-origin protection
  // facilities built into the protocol and the browser.  You should
  // *always* verify the connection's origin and decide whether or not
  // to accept it.
  autoAcceptConnections: false,
});

function originIsAllowed(origin) {
  // put logic here to detect whether the specified origin is allowed.
  return true;
}

let connections = [];
wsServer.on('request', function(request) {
  if (!originIsAllowed(request.origin)) {
    // Make sure we only accept requests from an allowed origin
    request.reject();
    console.log(
      new Date() + ' Connection from origin ' + request.origin + ' rejected.'
    );
    return;
  }

  var connection = request.accept('echo-protocol', request.origin);
  connections.push(connection);
  console.log(new Date() + ' Connection accepted.');
  connection.on('message', function(message) {
    if (message.type === 'utf8') {
      console.log('Received Message: ' + message.utf8Data);
      connections.forEach(c => c.sendUTF(message.utf8Data));
    } else if (message.type === 'binary') {
      console.log(
        'Received Binary Message of ' + message.binaryData.length + ' bytes'
      );
      connection.sendBytes(message.binaryData);
    }
  });
  connection.on('close', function(reasonCode, description) {
    console.log(
      new Date() + ' Peer ' + connection.remoteAddress + ' disconnected.'
    );
  });
});
