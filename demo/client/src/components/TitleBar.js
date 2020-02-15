import React, { Component } from 'react';
import '../css/icon.css'

class TitleBar extends Component {
  render() {
    const icon = this.props.collapsed ? <i className="icon">&#xe806;</i> : <i className="icon">&#xe807;</i>;
    return (
      
      <div className="title-bar">
        <div className="title-text">{this.props.title}</div>
        <div className="collapse-button">
          <a onClick={this.props.onToggleCollapse}>{icon}</a>
        </div>
      </div>
    );
  }
}

export default TitleBar;
