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
    const groupClasses = "btn-group " + this.props.position;
    return (
      <div className={groupClasses}>
        {buttons}
      </div>
    );
  }
}

export default ButtonGroup;
