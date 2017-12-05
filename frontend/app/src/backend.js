// @flow
import _ from "lodash";
import store from "./Store.js";
import { isQuery } from "./actions";
import type { Action } from "./actions";

var ws;
var backlog = [];

function onopen() {
  // dispatch: CONNECTION_OPEN
  const user = JSON.parse(localStorage.getItem("user") || "{}") || {};
  // send user-id and token
  // console.log('onopen');
  if (user.id && user.token) {
    ws.send(
      JSON.stringify({
        tag: "with-token",
        userId: user.id,
        token: user.token
      })
    );
  } else {
    ws.send(
      JSON.stringify({
        tag: "without-token"
      })
    );
  }
  _.forEach(backlog, msg => {
    ws.send(JSON.stringify(msg));
  });
  backlog = [];
}
function onclose() {
  // dispatch: CONNECTION_LOST
  // try to re-connect
  console.log("onclose");
  _.delay(connect, 1000);
}
function onerror() {
  // display the error
  console.log("onerror");
}
function onmessage(event) {
  if (typeof event.data !== "string")
    throw new Error("Unexpected event type: " + typeof event.data);
  const msg = JSON.parse(event.data);
  // console.log('onmessage', msg);
  store.dispatch(msg);
}

function connect() {
  const isReactSnap = navigator.userAgent === "ReactSnap";
  const isProduction =
    process && process.env && process.env.NODE_ENV === "production";
  if (isProduction && isReactSnap)
    ws = new WebSocket("ws://beta.clozecards.com/api/ws");
  else ws = new WebSocket("ws://" + window.location.host + "/api/ws");
  ws.onopen = onopen;
  ws.onclose = onclose;
  ws.onerror = onerror;
  ws.onmessage = onmessage;

  if (!isReactSnap) {
    const preloadedState = window.__PRELOADED_STATE__;
    _.forEach(preloadedState, action => {
      if (isQuery(action)) {
        send(action);
      }
    });
    delete window.__PRELOADED_STATE__;
  }
}

function relay(msg: Action) {
  store.dispatch(msg);
  send(msg);
}

function send(msg: Action) {
  if (ws.readyState === 0) backlog.push(msg);
  else ws.send(JSON.stringify(msg));
}

export default {
  connect,
  relay
};

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
