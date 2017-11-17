import _ from 'lodash'
import store from './Store.js'

var ws;
var backlog = [];

function onopen() {
  // dispatch: CONNECTION_OPEN
  const user = JSON.parse(localStorage.getItem('user')) || {};
  // send user-id and token
  // console.log('onopen');
  if( user.id && user.token ) {
    ws.send(JSON.stringify({
      tag: 'with-token',
      userId: user.id,
      token: user.token,
    }));
  } else {
    ws.send(JSON.stringify({
      tag: 'without-token'
    }));
  }
  _.forEach(backlog, msg => {
    ws.send(JSON.stringify(msg));
  });
  backlog = [];
}
function onclose() {
  // dispatch: CONNECTION_LOST
  // try to re-connect
  console.log('onclose');
  _.delay(connect, 1000);
}
function onerror() {
  // display the error
  console.log('onerror');
}
function onmessage(event) {
  const msg = JSON.parse(event.data);
  // console.log('onmessage', msg);
  store.dispatch(msg);
}

function connect() {
  if( process && process.env && process.env.NODE_ENV === 'production')
    ws = new WebSocket('ws://beta.clozecards.com/api/ws');
  else
    ws = new WebSocket('ws://' + window.location.host + '/api/ws');
  ws.onopen = onopen;
  ws.onclose = onclose;
  ws.onerror = onerror;
  ws.onmessage = onmessage;
}

function relay(msg) {
  store.dispatch(msg);
  if(ws.readyState===0)
    backlog.push(msg);
  else
    ws.send(JSON.stringify(msg));
}

export default {
  connect,
  relay
}


// Time series

// Time series have:
//   Key             :: UTCTime -> UTCTime
//   InitialValue    :: a
//   Join            :: (a -> a -> a)
//   EmptyIntervalFn :: (previous_value -> current_value))
//   Prune range
// Time series entries:
//    sid
//    date
//    value

// High score over the last 24 hours.
// By hour, sum, const 0, top 24

// High score over the last 7 days.
// By day, sum, const 0, top 7

// High score over the last 30 days.
// By day, sum, const 0, top 30

// Total score.
// 0, sum, const 0, top 1

// Progress
// By day, replace, id, top 1
