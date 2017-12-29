// @flow
import React, { PureComponent } from "react";
import { Container, Grid } from "semantic-ui-react";
import { connect } from "react-redux";

import Highscore from "../components/highscore";

type Props = {
  daily: { [string]: number },
  weekly: { [string]: number }
};

function toProps(store) {
  const { daily, weekly } = store.highscore;
  const id = store.user.id;
  return {
    highlight: id,
    daily,
    weekly,
    online: store.online
  };
}

export default connect(toProps)(
  class Statistics extends PureComponent<Props> {
    render = () => {
      const { daily, weekly, highlight, online } = this.props;
      return (
        <Container text>
          <Grid stackable textAlign="center">
            <Grid.Row columns={2}>
              <Grid.Column>
                <Highscore
                  title="Weekly"
                  highscore={weekly}
                  highlight={highlight}
                  mark={online}
                />
              </Grid.Column>
              <Grid.Column>
                <Highscore
                  title="Daily"
                  highscore={daily}
                  highlight={highlight}
                  mark={online}
                />
              </Grid.Column>
            </Grid.Row>
          </Grid>
        </Container>
      );
    };
  }
);
