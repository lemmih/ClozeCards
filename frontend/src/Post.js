import _ from 'lodash'
import React, { Component } from 'react'
import { withRouter, Redirect } from 'react-router-dom'
import { connect } from 'react-redux'
import {
  Container, Item,
} from 'semantic-ui-react'
import { Set } from 'immutable'
import './App.css';
import uuid from 'uuid/v4'

import PostItem from './PostItem.js'
import PostBody from './PostBody.js'
import PostComments from './PostComments.js'

import {receivePost} from './actions/postActions'
import {receiveContent} from './actions/contentActions'

import mkSlug from './misc/slugs'

function toViewPostProps(store, ownProps) {
  const slug = ownProps.slug;
  const post = store.posts.find((post) => _.includes(post.slugs,slug));
  const content = post ? store.content.get(post.id) : null;
  return {
    takenSlugs: store.posts.map(p => post&&p.id===post.id ? Set() : Set(p.slugs)).toSet().flatten(),
    post: post,
    content: content
  };
}
export const ViewPost = connect(toViewPostProps)(class ViewPost extends Component {
  state = { editable: false }

  handleEdit = () => {
    this.setState({ editable: true });
  }
  handleSave = (post) => {
    const takenSlugs = this.props.takenSlugs;
    const slug = mkSlug(post.title, takenSlugs);
    const slugs = _.uniq([slug].concat(post.slugs));
    const postId = uuid();
    this.props.dispatch(receivePost(post.id, post.title, post.tags, post.types, slugs,0,0));
    this.props.dispatch(receiveContent(post.id, this.bodyRef.postEditorRaw()));
    this.setState({ editable: false });
    // this.props.history.push('/posts/'+slug);
  }
  handleBodyRef = (ref) => this.bodyRef = ref;

  render = () => {
    if( !this.props.post )
      return <Redirect to="/"/>
    return(
      <div>
        <Container style={{paddingTop: "2em"}}>
          <Item.Group>
            <PostItem
              post={this.props.post}
              editable={this.state.editable}
              onEdit={this.handleEdit}
              onSave={this.handleSave}
              mayEdit={true}/>
          </Item.Group>

          <PostBody
            postContent={this.props.content}
            editable={this.state.editable}
            showAnnotations={false}
            ref={this.handleBodyRef} />

          { false && <PostComments/>}
        </Container>
      </div>
    );
  }
})

function toNewPostProps(store) {
  return {
    posts: store.posts
  };
}

const NewPost = withRouter(connect(toNewPostProps)(class NewPost extends Component {
  onSave = (post) => {
    const takenSlugs = this.props.posts.map(post => Set(post.slugs)).toSet().flatten();
    const slug = mkSlug(post.title, takenSlugs);
    const postId = uuid();
    this.props.dispatch(receivePost(postId, post.title, post.tags, post.types, [slug],0,0));
    this.props.dispatch(receiveContent(postId, this.bodyRef.postEditorRaw()));
    this.props.history.push('/posts/'+slug);
  }
  handleBodyRef = (ref) => this.bodyRef = ref;

  render = () => {
    return(
      <div>
        <Container style={{paddingTop: "2em"}}>
          <Item.Group>
            <PostItem
              mayEdit={true}
              editable={true}
              onSave={this.onSave}
              />
          </Item.Group>

          <PostBody ref={this.handleBodyRef} editable={true} showAnnotations={false}/>
        </Container>
      </div>
    );
  }
}))

export { NewPost };
