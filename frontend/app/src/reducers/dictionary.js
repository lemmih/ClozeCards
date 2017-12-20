import _ from "lodash";
import {
  SHOW_DICTIONARY,
  HIDE_DICTIONARY,
  PIN_DICTIONARY,
  UNPIN_DICTIONARY,
  RECEIVE_DICTIONARY_RESULTS
} from "../actions/dictionary";
import { Map } from "immutable";

export function activeWord(state = null, action) {
  switch (action.type) {
    case SHOW_DICTIONARY:
      if (state && state.pinned) return state;
      return { ...action.payload, pinned: false };
    case HIDE_DICTIONARY:
      if (state && state.pinned) return state;
      return null;
    case PIN_DICTIONARY:
      if (!state) return state;
      return { ...action.payload, pinned: !state.pinned };
    case UNPIN_DICTIONARY:
      return null;
    default:
      return state;
  }
}
export function cache(state = Map(), action) {
  switch (action.type) {
    case RECEIVE_DICTIONARY_RESULTS:
      let s = state;
      _.forOwn(action.payload, (value, key) => {
        s = s.set(key, value);
      });
      return s;
    default:
      return state;
  }
}
