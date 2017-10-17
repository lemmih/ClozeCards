import _ from 'lodash'
import React, { Component } from 'react'
import { connect } from 'react-redux'
import {
  Container, Item
} from 'semantic-ui-react'
import Loading from './components/Loading.js'
import './App.css';

import PostItem from './PostItem.js'

const toProps = (store) => {
  return {
    posts: store.posts
  };
};

const BrowsePosts = connect(toProps)(class BrowsePosts extends Component {
  render = () => {
    return(
      <Loading active >
        <Container style={{paddingTop: "2em"}}>
          <Item.Group divided>
            { this.props.posts.map((post, id) => <PostItem key={id} post={post}/>).toArray() }
          </Item.Group>
        </Container>
      </Loading>
    );
  }
});

export default BrowsePosts;
