export const SET_ACTIVE_USER = "SET_ACTIVE_USER";
export const LOGIN = "LOGIN";
export const LOGIN_FAILED = "LOGIN_FAILED";
export const LOGOUT = "LOGOUT";

export function login(email, password) {
  return {
    type: LOGIN,
    payload: {
      email, password
    }
  };
}
export function logout() {
  return {
    type: LOGOUT,
    payload: null
  };
}
