import {
  SHOW_DICTIONARY,
  HIDE_DICTIONARY,
  PIN_DICTIONARY,
  UNPIN_DICTIONARY
} from "../actions/dictionary";

export default function(state = null, action) {
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
