// @flow
import _ from "lodash";
import * as React from "react";
import { Loader } from "semantic-ui-react";

type Props = {
  active: boolean,
  children?: React.ChildrenArray<React.Element<any>>
};

type State = {
  mounted: boolean
};

class Loading extends React.PureComponent<Props, State> {
  state = { mounted: false };

  componentDidMount = () => {
    _.defer(() => {
      this.setState({ mounted: true });
    });
  };
  render = () => {
    const { active, children } = this.props;
    const ready = !!active && this.state.mounted;
    if (ready) {
      if (children && children.length === 1) return children[0];
      return <div>{children}</div>;
    } else {
      return <Loader active />;
    }
  };
}

export default Loading;
