import { SET_ONLINE } from "../actions/online";
import { Set } from "immutable";

export default function(state = Set(), action) {
  switch (action.type) {
    case SET_ONLINE:
      const { online, offline } = action.payload;
      return state.union(online).subtract(offline);
    default:
      return state;
  }
}
