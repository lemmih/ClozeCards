import React, { PureComponent } from "react";

const dictStyle = {
  position: "fixed",
  top: 0,
  left: "50%",
  marginLeft: "-15em",
  width: "30em",
  zIndex: 1001,
  boxShadow: "1px 1px 5px #0074D9",
  background: "white",
  padding: "1ex"
};

export default class extends PureComponent {
  render = () => {
    return <div style={dictStyle}>Dictionary</div>;
  };
}
