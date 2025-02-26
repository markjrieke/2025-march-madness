function changeStyle(dataId) {
  const elements = document.querySelectorAll(`[data-id="${dataId}"]`);
  for (var i = 0; i < elements.length; i++) {
    if (elements[i].tagName != "text") {
      elements[i].setAttribute("fill-opacity", 0);
      elements[i].setAttribute("stroke-opacity", 0);
    };
    elements[i].onmouseover = function() {
      for (var k = 0; k < elements.length; k++) {
        if (elements[k].tagName != "text") {
          elements[k].setAttribute("fill-opacity", 1);
          elements[k].setAttribute("stroke-opacity", 1);
        }
      }
    };
    elements[i].onmouseout = function() {
      for (var k = 0; k < elements.length; k++) {
        if (elements[k].tagName != "text") {
          elements[k].setAttribute("fill-opacity", 0);
          elements[k].setAttribute("stroke-opacity", 0);
        }
      }
    };
  }
};