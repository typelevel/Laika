import React, { Component } from 'react';
import '../css/button.css';

class ButtonGroup extends Component {
  render() {
    const buttons = this.props.items.map( item => {
      const active = item.value == this.props.value  
      const classes = active ? "btn active" : "btn"  
      return (
        <button
          type="button"
          key={item.value}
          className={classes}
          onClick={e => { if (!active) this.props.onChange(item.value) }}>
            {item.display}
        </button>
      ); 
    });
    return (
      <div className="btn-group">
        {buttons}
      </div>
    );
  }
}

export default ButtonGroup;
