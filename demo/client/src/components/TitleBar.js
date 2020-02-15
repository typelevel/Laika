import React, { Component } from 'react';

class TitleBar extends Component {
  render() {
    return (
      <div className="title-bar">
        <div className="title-text">{this.props.title}</div>
      </div>
    );
  }
}

export default TitleBar;
