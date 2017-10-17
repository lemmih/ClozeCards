export const RECEIVE_POST = "RECEIVE_POST";

// What does a post contain?
// Title.
// Tags.
// Type.
// Slugs
// # Comments.
// # Likes
export function receivePost(id, title, tags, type, slugs, nLikes, nComments) {
  return {
    type: RECEIVE_POST,
    payload: {
      id,
      title,
      tags,
      type,
      slugs,
      nLikes,
      nComments,
    }
  }
}
