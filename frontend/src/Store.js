import _ from 'lodash'
import { applyMiddleware, createStore } from "redux"
import {createLogger} from "redux-logger"
import thunk from "redux-thunk"
import promise from "redux-promise-middleware"
import { Map } from 'immutable'

import uuid from 'uuid/v4'

import rootReducer from './reducers'

const dummyPosts = [
  { id: uuid()
  , title: 'Title 1'
  , type: 'word-list'
  , tags: ['Exam']
  , nLikes: 0
  , nComments: 0
  , slugs: ['post-1'] },
  { id: uuid()
  , title: 'Title 2'
  , type: 'story'
  , tags: ['Story']
  , nLikes: 2
  , nComments: 4
  , slugs: ['post-2'] },
  { id: uuid()
  , title: 'Title 3'
  , type: 'blog-post'
  , tags: ['Audio']
  , nLikes: 3
  , nComments: 0
  , slugs: ['post-3'] },
  { id: uuid()
  , title: 'Title 4'
  , type: 'article'
  , tags: []
  , nLikes: 0
  , nComments: 0
  , slugs: ['post-4'] },
  { id: uuid()
  , title: 'Title 5'
  , type: 'poem'
  , tags: []
  , nLikes: 10
  , nComments: 10
  , slugs: ['post-5'] },
  { id: uuid()
  , title: 'Title 6'
  , type: 'lesson'
  , tags: []
  , nLikes: 0
  , nComments: 3
  , slugs: ['post-6'] }];

// posts
// post-fetched
// post-fetching
// for content: key missing => not fetched
//              value null => fetching
//              value not null => fetched
// post-contents indexed by post-id
// notes-contents indexed by post-id + user-id
// comments: {id, parent, owner, date, edited}
// comment-contents index by comment-id ?
const defaultState = {
  posts: Map(dummyPosts.map((obj) => [obj.id, obj])),
  content: Map(),
};

const middleware = applyMiddleware(promise(), thunk, createLogger());

export default createStore(rootReducer, defaultState, middleware)
