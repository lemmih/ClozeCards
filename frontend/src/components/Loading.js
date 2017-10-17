import _ from 'lodash'
import React, { Component } from 'react'
import {
  Loader
} from 'semantic-ui-react'

class Loading extends Component {
  state = {mounted: false}

  componentDidMount = () => {
    _.defer(() => {
      this.setState({mounted: true})
    })
  }
  render = () => {
    const ready = !!this.props.active && this.state.mounted;
    if( ready ) {
      if( this.props.children.length === 1)
        return this.props.children[0];
      return <div>{this.props.children}</div>;
    } else {
      return <Loader active/>
    }
  }
}

export default Loading;
