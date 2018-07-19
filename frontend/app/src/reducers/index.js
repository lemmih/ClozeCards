import { combineReducers } from "redux";
import content from "./content";
import decks from "./decks";
import decksBySlug from "./decksBySlug";
import user from "./user";
import cards from "./cards";
import search from "./search";
import notes from "./notes";
import { activeWord, cache } from "./dictionary";
import highlight from "./highlight";
import highscore from "./highscore";
import online from "./online";
import diagnose from "./diagnose";

export default combineReducers({
  decks,
  decksBySlug,
  content,
  user,
  cards,
  search,
  notes,
  dictionary: combineReducers({ activeWord, cache }),
  highlight,
  highscore,
  online,
  diagnose
});
