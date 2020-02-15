import React, { Component } from 'react';
import TitleBar from './TitleBar'
import { collapse, expand } from './AutoHeightTransform'

class Panel extends Component {
  
  constructor(props) {
    super(props);
    this.panelBody = React.createRef();
  }

  state = {
    collapsed: false
  }
  
  handleCollapseToggle = () => { this.setState(prevState => { 
    if (prevState.collapsed) expand(this.panelBody.current); 
    else collapse(this.panelBody.current);
    return {
      collapsed: !prevState.collapsed 
    }
  })}

  render() {
    const bodyClasses = `panel-body ${this.props.kind}`;
    const panelClasses = this.props.bottom ? "bottom-panel" : undefined;
    return (
      <div className={panelClasses}>
        
        <TitleBar title={this.props.title} collapsed={this.state.collapsed} onToggleCollapse={this.handleCollapseToggle}/>
        
        <div className='collapsible' ref={this.panelBody}>
          <div className={bodyClasses}>
            {this.props.children}
          </div>
        </div>

      </div>  
    );
  }
}

export default Panel;
