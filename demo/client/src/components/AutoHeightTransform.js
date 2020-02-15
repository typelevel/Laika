
  var lastAction = { cancelled: false }

  function newAction () {
    lastAction.cancelled = true;
    lastAction = { cancelled: false };
    return lastAction;
  }

  export function collapse(element) {
    
    const thisAction = newAction();
    const sectionHeight = element.scrollHeight;
    
    requestAnimationFrame(() => {
      if (thisAction.cancelled) return;
      element.style.height = sectionHeight + 'px';
      
      requestAnimationFrame(() =>  {
        if (thisAction.cancelled) return;
        element.style.height = '0px';
      });
    });
  }
  
  export function expand(element) {

    const thisAction = newAction();
    const sectionHeight = element.scrollHeight;
    element.style.height = sectionHeight + 'px';
  
    const reset = e => {
        element.removeEventListener('transitionend', reset);
        if (!thisAction.cancelled) element.style.height = null;
    }
    element.addEventListener('transitionend', reset);
  }
  