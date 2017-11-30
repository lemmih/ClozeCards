import { Set } from "immutable";
import {
  SET_ACTIVE_USER,
  LOGIN,
  LOGOUT,
  LOGIN_FAILED,
  SET_FAVORITE,
  UNSET_FAVORITE
} from "../actions/user.js";

export default function(state = {}, action) {
  switch (action.type) {
    case SET_ACTIVE_USER:
      return Object.assign(
        { token: action.payload.token },
        action.payload.user,
        { favorites: Set(action.payload.user.favorites) }
      );
    case LOGIN:
      return Object.assign({}, state, { status: "logging-in" });
    case LOGIN_FAILED:
      return Object.assign({}, state, { status: "failed" });
    case LOGOUT:
      return {};
    case SET_FAVORITE:
      return { ...state, favorites: state.favorites.add(action.payload) };
    case UNSET_FAVORITE:
      return { ...state, favorites: state.favorites.delete(action.payload) };
    default:
      return state;
  }
}
