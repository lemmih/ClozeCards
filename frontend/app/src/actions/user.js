export const SET_ACTIVE_USER = "SET_ACTIVE_USER";
export const LOGIN = "LOGIN";
export const LOGIN_FAILED = "LOGIN_FAILED";
export const LOGOUT = "LOGOUT";
export const SET_FAVORITE = "SET_FAVORITE";
export const UNSET_FAVORITE = "UNSET_FAVORITE";

export function login(email, password) {
  return {
    type: LOGIN,
    payload: {
      email,
      password
    }
  };
}
export function logout() {
  return {
    type: LOGOUT,
    payload: null
  };
}
export function setFavorite(deckId) {
  return {
    type: SET_FAVORITE,
    payload: deckId
  };
}
export function unsetFavorite(deckId) {
  return {
    type: UNSET_FAVORITE,
    payload: deckId
  };
}
