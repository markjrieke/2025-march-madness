function checkElement(element) {
  return element.tagName != "text" | element.textContent.includes("%") | element.tagName == "image" | element.getAttribute("font-weight") == "bold";
};

function changeStyle(dataId) {
  const elements = document.querySelectorAll(`[data-id="${dataId}"]`);
  
  for (var i = 0; i < elements.length; i++) {
    // initialize all elements to 0 opacity
    if (checkElement(elements[i])) {
      elements[i].style.opacity = 0;
    };
    
    // on hover, set opacity to 1 for all elements with a matching data-id
    elements[i].onmouseover = function() {
      for (var k = 0; k < elements.length; k++) {
        if (checkElement(elements[k])) {
          elements[k].style.opacity = 1;
        }
      }
    };
    
    // on mouse-out, revert back to 0
    elements[i].onmouseout = function() {
      for (var k = 0; k < elements.length; k++) {
        if (checkElement(elements[k])) {
          elements[k].style.opacity = 0;
        }
      }
    };
  }
};