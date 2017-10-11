import React, { Component } from 'react'
import { BrowserRouter as Router, Route, Link } from 'react-router-dom'
import App from './App'
import LandingPage from './LandingPage'
import Post from './Post'

class Layout extends Component {
  render = () => {
    return(
      <Router>
        <div>
          <Link to={"/"}>Home</Link> | <Link to={"/content/"}>Texts</Link> | <Link to={"/new-content/"}>New</Link>
          <Route path="/" exact={true} component={LandingPage}/>
          <Route path="/content/" exact={true} component={App}/>
          <Route path="/new-content/" exact={true} render={() => <Post editable={true}/>}/>
        </div>
      </Router>
    );
  }
}

export default Layout;
