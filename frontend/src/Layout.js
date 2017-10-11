import React, { Component } from 'react'
import { BrowserRouter as Router, Route, Link } from 'react-router-dom'
import App from './App'
import LandingPage from './LandingPage'
import Post from './Post'
import {Menu} from 'semantic-ui-react'

class Layout extends Component {
  render = () => {
    return(
      <Router>
        <div>
          <Menu attached="top" inverted={true} color="blue" size="huge" stackable={true}>
            <Menu.Item as={Link} to={"/"} header>ClozeCards</Menu.Item>
            <Menu.Item as={Link} to={"/content/"}>Top Posts</Menu.Item>
            <Menu.Item as={Link} to={"/content/"}>Recent Posts</Menu.Item>
            <Menu.Item as={Link} to={"/content/"}>Trending Posts</Menu.Item>
            <Menu.Item as={Link} to={"/teach/"}>Teach</Menu.Item>
            <Menu.Menu position="right">
              <Menu.Item as={Link} to={"/new-content/"} name="New Post"></Menu.Item>
              <Menu.Item as={Link} to={"/news/"}>News</Menu.Item>
              <Menu.Item as={Link} to={"/login/"}name="login"/>
            </Menu.Menu>
          </Menu>
          <Route path="/" exact={true} component={LandingPage}/>
          <Route path="/content/" exact={true} component={App}/>
          <Route path="/new-content/" exact={true} render={() => <Post editable={true}/>}/>
        </div>
      </Router>
    );
  }
}

export default Layout;
