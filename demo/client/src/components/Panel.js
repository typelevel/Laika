import React, { Component } from 'react';
import TitleBar from './TitleBar'

class Panel extends Component {
  
  constructor(props) {
    super(props);
  }

  render() {
    const bodyClasses = `panel-body ${this.props.kind}`;
    return (
      <div>
        
        <TitleBar title={this.props.title} />
        
        <div>
          <div className={bodyClasses}>
            {this.props.children}
          </div>
        </div>

      </div>  
    );
  }
}

export default Panel;
