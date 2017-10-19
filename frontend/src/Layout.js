import _ from 'lodash'
import React, { Component } from 'react'
import { BrowserRouter as Router, Route, Link, Switch } from 'react-router-dom'
import LandingPage from './LandingPage'
import {Menu} from 'semantic-ui-react'
import { connect } from 'react-redux'

import { ViewPost, NewPost } from './Post'
import BrowsePosts from './BrowsePosts'

class Layout extends Component {
  matchViewPost = ({match}) => {
    return <ViewPost slug={match.params.slug}/>;
  }
  matchViewNotes = ({match}) => {
    return <ViewPost slug={match.params.slug} notes/>;
  }
  matchStudy = ({match}) => {
    return <ViewPost slug={match.params.slug} study/>;
  }
  render = () => {
    return(
      <Router>
        <div>
          <Menu attached="top" inverted={true} color="blue" size="huge" stackable={true}>
            <Menu.Item as={Link} to={"/"} header>ClozeCards</Menu.Item>
            <Menu.Item as={Link} to={"/top/"}>Top Posts</Menu.Item>
            <Menu.Item as={Link} to={"/content/"}>Recent Posts</Menu.Item>
            <Menu.Item as={Link} to={"/content/"}>Trending Posts</Menu.Item>
            <Menu.Item as={Link} to={"/teach/"}>Teach</Menu.Item>
            <Menu.Menu position="right">
              <Menu.Item as={Link} to={"/new-content/"} name="New Post"></Menu.Item>
              <Menu.Item as={Link} to={"/news/"}>News</Menu.Item>
              <Menu.Item as={Link} to={"/login/"}name="login"/>
            </Menu.Menu>
          </Menu>
          <Switch>
            <Route path="/" exact={true} component={LandingPage}/>
            <Route path="/top/" exact={true} render={() => <BrowsePosts/>}/>
            <Route path="/new-content/" exact={true} render={() => <NewPost/>}/>
            <Route path="/posts/:slug" exact={true} render={this.matchViewPost}/>
            <Route path="/posts/:slug/notes" exact={true} render={this.matchViewNotes}/>
            <Route path="/posts/:slug/study" exact={true} render={this.matchStudy}/>
          </Switch>
        </div>
      </Router>
    );
  }
}

export default connect((store) => store)(Layout);
