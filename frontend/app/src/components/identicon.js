// @flow
import React, { PureComponent } from "react";
import Identiconjs from "identicon.js";
import sha256 from "sha256";

// ptions = {
//       foreground: [0, 0, 0, 255],               // rgba black
//       background: [255, 255, 255, 255],         // rgba white
//       margin: 0.2,                              // 20% margin
//       size: 420,                                // 420px square
//       format: 'svg'                             // use SVG instead of PNG
//     };

type Props = {
  id: number | string,
  size?: number,
  format?: string,
  margin?: number,
  foreground?: Array<number>,
  background?: Array<number>
};

export default class Identicon extends PureComponent<Props> {
  format = () => {
    switch (this.props.format) {
      case "svg":
        return "data:image/svg+xml;base64,";
      default:
        return "data:image/png;base64,";
    }
  };
  render = () => {
    const { id } = this.props;
    return (
      <img
        alt={id}
        src={
          this.format() +
          new Identiconjs(sha256(id), { ...this.props }).toString()
        }
      />
    );
  };
}
Identicon.defaultProps = {
  background: [0xff, 0xff, 0xff, 0x00],
  format: "svg"
};
