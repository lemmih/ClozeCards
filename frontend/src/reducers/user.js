import { SET_ACTIVE_USER, LOGIN, LOGOUT, LOGIN_FAILED } from '../actions/user.js'

export default function (state={}, action) {
  switch (action.type) {
    case SET_ACTIVE_USER:
      return Object.assign({token: action.payload.token}, action.payload.user);
    case LOGIN:
      return Object.assign({}, state, {status: 'logging-in'});
    case LOGIN_FAILED:
      return Object.assign({}, state, {status: 'failed'});
    case LOGOUT:
      return {};
    default:
      return state;
  }
}
