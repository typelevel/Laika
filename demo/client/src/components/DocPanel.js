import React, { Component } from 'react';
import Panel from './Panel'

class DocPanel extends Component {
  render() {
    return (
      <Panel kind="docs" title="Documentation" bottom> 
        <p>The Laika Webtool currently uses the 0.8.0 release of Laika.</p>
        <p>Transformation starts automatically one second after you stop typing.</p>
        <p>The raw document tree and the rewritten tree are often identical. You see
        differences when using features that produce nodes that need to be resolved,
        like link references or headers.</p>
        <p>For more documentation see:</p>
        <ul>
          <li>The <a href="http://planet42.github.io/Laika/">Laika Manual</a></li>
          <li>The <a href="http://daringfireball.net/projects/markdown/syntax">Markdown Syntax Documentation</a></li>
          <li>The <a href="http://docutils.sourceforge.net/docs/user/rst/quickref.html">reStructuredText Quick Reference</a></li>
          <li>The <a href="https://github.com/planet42/Laika-Webtool">Source Code</a> of this web tool.</li>
        </ul>  
      </Panel>
    );
  }
}

export default DocPanel;
